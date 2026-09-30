{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A scripted loopback Chat Completions server and a bounded runner for the
-- real @hach@ executable. Acceptance tests drive the public executable
-- against this fixture and assert on the HTTP requests it actually sent.
--
-- The fixture answers one request per connection, in script order. Once the
-- script is exhausted every further request is recorded and answered with an
-- error at once, so an unexpected request fails a test promptly instead of
-- hanging it. Everything is torn down when the scope exits.
module Hach.InferenceFixture
  ( -- * Fixture
    Fixture
  , FixtureReply(..)
  , CapturedRequest(..)
  , withInferenceFixture
  , fixturePort
  , fixtureRoot
  , fixtureBaseUrl
  , fixtureRequests
  , awaitRequestCount
  , requestHeader
  , requestJson
  , jsonAt
    -- * Canned replies
  , reply
  , replyJson
  , completion
  , completionWith
  , toolCallCompletion
  , toolCall
  , usage
    -- * Running hach
  , HachRun(..)
  , HachResult(..)
  , hachRun
  , runHach
  , hachExecutable
  , withSandbox
  , sandboxEnvironment
  , writeWorkspaceFile
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, withAsync)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Directory
  ( createDirectory, createDirectoryIfMissing, getTemporaryDirectory
  , removeDirectoryRecursive, removeFile
  )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, openTempFile)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode, readProcess)
import System.Timeout (timeout)

--------------------------------------------------------------------------------
-- Fixture
--------------------------------------------------------------------------------

-- | One HTTP request as received.
data CapturedRequest = CapturedRequest
  { crMethod  :: !BS.ByteString
  , crPath    :: !BS.ByteString
  , crHeaders :: ![(BS.ByteString, BS.ByteString)]  -- ^ Names lowercased.
  , crBody    :: !BS.ByteString
  } deriving (Show, Eq)

-- | What the fixture does with the next request.
data FixtureReply
  = Reply !Int ![(BS.ByteString, BS.ByteString)] !LBS.ByteString
    -- ^ Status, extra headers, body.
  | Stall
    -- ^ Hold the connection open, never answering, until the fixture closes.

data Fixture = Fixture
  { fxPort     :: !PortNumber
  , fxRequests :: !(IORef [CapturedRequest])  -- ^ Newest first.
  }

fixturePort :: Fixture -> Int
fixturePort = fromIntegral . fxPort

-- | @http://127.0.0.1:PORT@, without a path.
fixtureRoot :: Fixture -> Text
fixtureRoot fx = "http://127.0.0.1:" <> T.pack (show (fixturePort fx))

-- | A conventional API root: @http://127.0.0.1:PORT/v1@.
fixtureBaseUrl :: Fixture -> Text
fixtureBaseUrl fx = fixtureRoot fx <> "/v1"

-- | Requests received so far, oldest first.
fixtureRequests :: Fixture -> IO [CapturedRequest]
fixtureRequests fx = reverse <$> readIORef (fxRequests fx)

-- | Wait (bounded) until at least @n@ requests have arrived.
awaitRequestCount :: Fixture -> Int -> IO Bool
awaitRequestCount fx n = fmap (maybe False (const True)) . timeout 30000000 $ loop
  where
    loop = do
      count <- length <$> readIORef (fxRequests fx)
      when (count < n) (threadDelay 20000 >> loop)

requestHeader :: BS.ByteString -> CapturedRequest -> Maybe BS.ByteString
requestHeader name = lookup (BC.map toLower name) . crHeaders

requestJson :: CapturedRequest -> Maybe Value
requestJson = Aeson.decodeStrict . crBody

-- | Follow object keys through a JSON value.
jsonAt :: [Text] -> Value -> Maybe Value
jsonAt [] v = Just v
jsonAt (k : ks) (Aeson.Object o) = KM.lookup (Key.fromText k) o >>= jsonAt ks
jsonAt _ _ = Nothing

-- | Serve the script on an ephemeral loopback port for the duration of the
-- action.
withInferenceFixture :: [FixtureReply] -> (Fixture -> IO a) -> IO a
withInferenceFixture script action =
  bracket openListener close $ \listener -> do
    port <- socketPort listener
    requests <- newIORef []
    remaining <- newMVar script
    handlers <- newIORef []
    let fixture = Fixture port requests
        serve conn = do
          mReq <- readRequest conn
          forM_ mReq $ \req -> do
            atomicModifyIORef' requests (\rs -> (req : rs, ()))
            next <- modifyMVar remaining $ \case
              (r : rs) -> pure (rs, Just r)
              [] -> pure ([], Nothing)
            case next of
              Just (Reply status headers body) -> sendResponse conn status headers body
              Just Stall -> forever (threadDelay 1000000)
              Nothing -> sendResponse conn 599 []
                "{\"error\":{\"message\":\"fixture: unexpected request beyond the script\"}}"
        acceptLoop = forever $ do
          (conn, _) <- accept listener
          handler <- async (serve conn `finally` close conn)
          atomicModifyIORef' handlers (\hs -> (handler : hs, ()))
    withAsync acceptLoop (\_ -> action fixture)
      `finally` (readIORef handlers >>= mapM_ (cancel :: Async () -> IO ()))
  where
    openListener = do
      sock <- socket AF_INET Stream defaultProtocol
      setSocketOption sock ReuseAddr 1
      bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      listen sock 16
      pure sock

-- | Read one request: headers to the blank line, then Content-Length bytes.
readRequest :: Socket -> IO (Maybe CapturedRequest)
readRequest conn = go BS.empty
  where
    go acc = case BS.breakSubstring "\r\n\r\n" acc of
      (headPart, rest) | not (BS.null rest) -> do
        let headerLines = BC.lines (BC.filter (/= '\r') headPart)
            (requestLine, headerFields) = case headerLines of
              (l : ls) -> (l, ls)
              [] -> ("", [])
            headers =
              [ (BC.map toLower (BC.strip name), BC.strip (BC.drop 1 value))
              | field <- headerFields
              , let (name, value) = BC.break (== ':') field
              ]
            len = maybe 0 fst (lookup "content-length" headers >>= BC.readInt)
            (method, pathPart) = case BC.words requestLine of
              (m : p : _) -> (m, p)
              _ -> ("", "")
        body <- readBody (BS.drop 4 rest) len
        pure (Just (CapturedRequest method pathPart headers body))
      _ -> do
        chunk <- NSB.recv conn 65536
        if BS.null chunk then pure Nothing else go (acc <> chunk)

    readBody acc len
      | BS.length acc >= len = pure (BS.take len acc)
      | otherwise = do
          chunk <- NSB.recv conn 65536
          if BS.null chunk then pure acc else readBody (acc <> chunk) len

sendResponse :: Socket -> Int -> [(BS.ByteString, BS.ByteString)] -> LBS.ByteString -> IO ()
sendResponse conn status headers body =
  NSB.sendAll conn $ BS.concat $
    [ "HTTP/1.1 ", BC.pack (show status), " Fixture\r\n"
    , "Content-Length: ", BC.pack (show (LBS.length body)), "\r\n"
    , "Connection: close\r\n"
    ]
    ++ [ name <> ": " <> value <> "\r\n" | (name, value) <- headers ]
    ++ [ "\r\n", LBS.toStrict body ]

--------------------------------------------------------------------------------
-- Canned replies
--------------------------------------------------------------------------------

reply :: Int -> LBS.ByteString -> FixtureReply
reply status = Reply status [("Content-Type", "text/plain")]

replyJson :: Int -> Value -> FixtureReply
replyJson status = Reply status [("Content-Type", "application/json")] . Aeson.encode

-- | A standard assistant text completion with no usage block.
completion :: Text -> FixtureReply
completion text = completionWith text Nothing

-- | A standard assistant text completion with an optional usage block.
completionWith :: Text -> Maybe Value -> FixtureReply
completionWith text mUsage = replyJson 200 $ object $
  [ "id" .= ("chatcmpl-fixture" :: Text)
  , "object" .= ("chat.completion" :: Text)
  , "choices" .=
      [ object
          [ "index" .= (0 :: Int)
          , "message" .= object ["role" .= ("assistant" :: Text), "content" .= text]
          , "finish_reason" .= ("stop" :: Text)
          ]
      ]
  ] ++ maybe [] (\u -> ["usage" .= u]) mUsage

-- | A completion whose first choice asks for tool calls.
toolCallCompletion :: [Value] -> Maybe Value -> FixtureReply
toolCallCompletion calls mUsage = replyJson 200 $ object $
  [ "choices" .=
      [ object
          [ "index" .= (0 :: Int)
          , "message" .= object
              [ "role" .= ("assistant" :: Text)
              , "content" .= Aeson.Null
              , "tool_calls" .= calls
              ]
          , "finish_reason" .= ("tool_calls" :: Text)
          ]
      ]
  ] ++ maybe [] (\u -> ["usage" .= u]) mUsage

-- | One function tool call; arguments are given as a JSON value and sent
-- as the usual JSON-encoded string.
toolCall :: Text -> Text -> Value -> Value
toolCall callId name args = object
  [ "id" .= callId
  , "type" .= ("function" :: Text)
  , "function" .= object
      [ "name" .= name
      , "arguments" .= T.pack (BC.unpack (LBS.toStrict (Aeson.encode args)))
      ]
  ]

-- | A usage block; the cost is included only when given.
usage :: Int -> Int -> Maybe Double -> Value
usage promptTokens completionTokens mCost = object $
  [ "prompt_tokens" .= promptTokens
  , "completion_tokens" .= completionTokens
  , "total_tokens" .= (promptTokens + completionTokens)
  ] ++ maybe [] (\c -> ["cost" .= c]) mCost

--------------------------------------------------------------------------------
-- Running hach
--------------------------------------------------------------------------------

-- | One invocation of the real executable.
data HachRun = HachRun
  { hrArgs      :: ![String]
  , hrEnv       :: ![(String, String)]  -- ^ Inference variables for this run.
  , hrStdin     :: !String
  , hrWorkspace :: !FilePath
  }

data HachResult = HachResult
  { hrExit   :: !ExitCode
  , hrStdout :: !String
  , hrStderr :: !String
  } deriving (Show)

hachRun :: FilePath -> [String] -> HachRun
hachRun workspace args = HachRun args [] "" workspace

hachExecutable :: IO FilePath
hachExecutable = do
  output <- readProcess "cabal" ["list-bin", "exe:hach"] ""
  pure (T.unpack (T.strip (T.pack output)))

-- | Run hach with a deadline; the process is killed if it overruns.
runHach :: HachRun -> IO HachResult
runHach run = do
  executable <- hachExecutable
  environment <- sandboxEnvironment (hrWorkspace run) (hrEnv run)
  let command = (proc executable (hrArgs run))
        { cwd = Just (hrWorkspace run), env = Just environment }
  res <- timeout 90000000 (readCreateProcessWithExitCode command (hrStdin run))
  case res of
    Just (code, out, err) -> pure (HachResult code out err)
    Nothing -> fail ("hach did not finish within 90s: " <> unwords (hrArgs run))

-- | The inherited environment with every inference and proxy variable
-- removed, home and user configuration isolated beside the workspace, the
-- threaded runtime limited, and the given variables added.
sandboxEnvironment :: FilePath -> [(String, String)] -> IO [(String, String)]
sandboxEnvironment workspace extra = do
  inherited <- getEnvironment
  let home = workspace <> "-home"
  createDirectoryIfMissing True home
  pure $
    extra
      ++ [ ("HOME", home)
         , ("CLAUDE_CONFIG_DIR", home </> ".claude")
         , ("GHCRTS", "-N2")
         , ("NO_PROXY", "*")
         , ("no_proxy", "*")
         ]
      ++ filter (not . cleared . fst) inherited
  where
    cleared name =
      name `elem` map fst extra
        || name `elem`
             [ "HOME", "CLAUDE_CONFIG_DIR", "GHCRTS", "HACH_PROVIDER"
             , "http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY"
             , "ALL_PROXY", "all_proxy", "NO_PROXY", "no_proxy"
             ]
        || "OPENROUTER_" `isPrefixOf` name
        || "OPENAI_" `isPrefixOf` name

-- | A fresh empty workspace (and sibling home) removed afterwards.
withSandbox :: (FilePath -> IO a) -> IO a
withSandbox action = do
  tmp <- getTemporaryDirectory
  (workspace, handle) <- openTempFile tmp "hach-inference-test"
  hClose handle
  removeFile workspace
  createDirectory workspace
  action workspace `finally` do
    void (try (removeDirectoryRecursive workspace) :: IO (Either SomeException ()))
    void (try (removeDirectoryRecursive (workspace <> "-home")) :: IO (Either SomeException ()))

writeWorkspaceFile :: FilePath -> FilePath -> Text -> IO ()
writeWorkspaceFile workspace relative contents = do
  let path = workspace </> relative
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (BC.pack (T.unpack contents))
