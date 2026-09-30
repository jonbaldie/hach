{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Executable acceptance tests for OpenAI-compatible inference (Issue #246).
-- Every example runs the real @hach@ binary against the loopback fixture in
-- "Hach.InferenceFixture" and asserts on the requests it actually sent.
module Hach.InferenceCliSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.Socket
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

import Hach.InferenceFixture

spec :: Spec
spec = describe "hach against an inference fixture" $ do
  textJourneySpec
  precedenceSpec
  baseUrlSpec
  requestShapeSpec
  toolCycleSpec
  responseSpec
  failureSpec
  goalSpec
  sessionSpec
  budgetSpec
  localOnlySpec

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

compatible :: Fixture -> [String]
compatible fx =
  [ "--print", "--provider", "openai-compatible"
  , "--base-url", T.unpack (fixtureBaseUrl fx), "--model", "local-model" ]

runWith :: FilePath -> [(String, String)] -> [String] -> IO HachResult
runWith ws envVars args = runHach (hachRun ws args) { hrEnv = envVars }

output :: HachResult -> String
output r = hrStdout r <> hrStderr r

shouldHaveNoRequests :: Fixture -> Expectation
shouldHaveNoRequests fx = fixtureRequests fx >>= (`shouldBe` [])

singleRequest :: Fixture -> IO CapturedRequest
singleRequest fx = fixtureRequests fx >>= \case
  [req] -> pure req
  reqs -> expectationFailure ("expected exactly one request, got " <> show (length reqs))
            >> fail "unreachable"

field :: [Text] -> CapturedRequest -> Maybe Value
field path req = requestJson req >>= jsonAt path

bodyText :: CapturedRequest -> Text
bodyText = TE.decodeUtf8With TE.lenientDecode . crBody

-- | The (tool_call_id, content) pairs of the tool-result messages sent.
toolResults :: CapturedRequest -> [(Text, Text)]
toolResults req = mapMaybe pick (maybe [] arrayItems (field ["messages"] req))
  where
    pick msg = case (jsonAt ["role"] msg, jsonAt ["tool_call_id"] msg, jsonAt ["content"] msg) of
      (Just (String "tool"), Just (String i), Just (String c)) -> Just (i, c)
      _ -> Nothing

arrayItems :: Value -> [Value]
arrayItems (Array xs) = toList xs
arrayItems _ = []

writeSettings :: FilePath -> Value -> IO ()
writeSettings ws = writeWorkspaceFile ws (".claude" </> "settings.json")
  . TE.decodeUtf8 . LBC.toStrict . Aeson.encode

writeDotenv :: FilePath -> [Text] -> IO ()
writeDotenv ws = writeWorkspaceFile ws ".env" . T.unlines

evaluatorReply :: Text -> Text -> FixtureReply
evaluatorReply verdict reason =
  completion (TE.decodeUtf8 (LBC.toStrict (Aeson.encode (object ["verdict" .= verdict, "reason" .= reason]))))

-- | A loopback port with nothing listening on it.
closedPort :: IO Int
closedPort = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  port <- socketPort sock
  close sock
  pure (fromIntegral port)

-- | Every file under a directory, recursively.
filesUnder :: FilePath -> IO [FilePath]
filesUnder dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else do
    entries <- listDirectory dir
    concat <$> mapM visit entries
  where
    visit entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir then filesUnder path else pure [path]

shouldNotLeak :: String -> HachResult -> Expectation
shouldNotLeak secret r = output r `shouldNotSatisfy` (secret `isInfixOf`)

savedArtifactsShouldNotContain :: FilePath -> String -> Expectation
savedArtifactsShouldNotContain ws secret = do
  files <- (++) <$> filesUnder (ws </> ".agents") <*> filesUnder (ws <> "-home")
  forM_ files $ \path -> do
    contents <- BS.readFile path
    (path, BC.pack secret `BS.isInfixOf` contents) `shouldBe` (path, False)

--------------------------------------------------------------------------------
-- Compatible text journey
--------------------------------------------------------------------------------

textJourneySpec :: Spec
textJourneySpec = describe "compatible text journey" $ do
  it "sends one keyless request to <base>/chat/completions and prints the answer" $
    withSandbox $ \ws -> withInferenceFixture [completion "fixture says hi"] $ \fx -> do
      result <- runWith ws [("OPENROUTER_API_KEY", "or-must-not-be-used")] (compatible fx <> ["hi"])
      hrExit result `shouldBe` ExitSuccess
      hrStdout result `shouldSatisfy` ("fixture says hi" `isInfixOf`)
      req <- singleRequest fx
      crMethod req `shouldBe` "POST"
      crPath req `shouldBe` "/v1/chat/completions"
      requestHeader "content-type" req `shouldBe` Just "application/json"
      requestHeader "authorization" req `shouldBe` Nothing
      requestHeader "http-referer" req `shouldBe` Nothing
      requestHeader "x-title" req `shouldBe` Nothing
      field ["model"] req `shouldBe` Just (String "local-model")
      bodyText req `shouldSatisfy` T.isInfixOf "hi"

  it "sends the OPENAI_API_KEY as a bearer credential when one is set" $
    withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
      result <- runWith ws [("OPENAI_API_KEY", "  sk-compat  ")] (compatible fx <> ["hi"])
      hrExit result `shouldBe` ExitSuccess
      req <- singleRequest fx
      requestHeader "authorization" req `shouldBe` Just "Bearer sk-compat"

--------------------------------------------------------------------------------
-- Configuration precedence
--------------------------------------------------------------------------------

precedenceSpec :: Spec
precedenceSpec = describe "configuration precedence" $ do
  it "selects compatible mode, endpoint, model, and key from the process environment" $
    withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
      result <- runWith ws
        [ ("HACH_PROVIDER", "openai-compatible")
        , ("OPENAI_BASE_URL", T.unpack (fixtureBaseUrl fx))
        , ("OPENAI_MODEL", "env-model")
        , ("OPENAI_API_KEY", "sk-env")
        ] ["--print", "hi"]
      hrExit result `shouldBe` ExitSuccess
      req <- singleRequest fx
      crPath req `shouldBe` "/v1/chat/completions"
      field ["model"] req `shouldBe` Just (String "env-model")
      requestHeader "authorization" req `shouldBe` Just "Bearer sk-env"

  forM_ [ ("provider first", \url -> ["HACH_PROVIDER=openai-compatible", "OPENAI_MODEL=dot-model", "OPENAI_API_KEY=sk-dot", "OPENAI_BASE_URL=" <> url])
        , ("key first", \url -> ["OPENAI_API_KEY=sk-dot", "export HACH_PROVIDER=openai-compatible", "OPENAI_BASE_URL=" <> url, "OPENAI_MODEL=dot-model"])
        , ("model first", \url -> ["OPENAI_MODEL=\"dot-model\"", "OPENAI_BASE_URL=" <> url, "OPENAI_API_KEY='sk-dot'", "HACH_PROVIDER=openai-compatible"])
        ] $ \(label, lines') ->
    it ("reads compatible settings from named .env entries (" <> label <> ")") $
      withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
        writeDotenv ws (lines' (fixtureBaseUrl fx))
        result <- runWith ws [] ["--print", "hi"]
        hrExit result `shouldBe` ExitSuccess
        req <- singleRequest fx
        field ["model"] req `shouldBe` Just (String "dot-model")
        requestHeader "authorization" req `shouldBe` Just "Bearer sk-dot"

  it "prefers flags over the process environment, and the process over .env" $
    withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
      writeDotenv ws
        [ "HACH_PROVIDER=openrouter", "OPENAI_API_KEY=sk-dot", "OPENAI_MODEL=dot-model"
        , "OPENAI_BASE_URL=http://127.0.0.1:9/dot" ]
      result <- runWith ws
        [ ("HACH_PROVIDER", "bogus"), ("OPENAI_API_KEY", "sk-proc")
        , ("OPENAI_MODEL", "proc-model"), ("OPENAI_BASE_URL", "not a url") ]
        ["--print", "--provider=openai-compatible", "--base-url=" <> T.unpack (fixtureBaseUrl fx), "hi"]
      hrExit result `shouldBe` ExitSuccess
      req <- singleRequest fx
      field ["model"] req `shouldBe` Just (String "proc-model")
      requestHeader "authorization" req `shouldBe` Just "Bearer sk-proc"

  it "lets a nonblank .env key and model fill blank process values" $
    withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
      writeDotenv ws ["OPENAI_API_KEY=sk-dot", "OPENAI_MODEL=dot-model"]
      result <- runWith ws [("OPENAI_API_KEY", "  "), ("OPENAI_MODEL", "")]
        ["--print", "--provider", "openai-compatible", "--base-url", T.unpack (fixtureBaseUrl fx), "hi"]
      hrExit result `shouldBe` ExitSuccess
      req <- singleRequest fx
      field ["model"] req `shouldBe` Just (String "dot-model")
      requestHeader "authorization" req `shouldBe` Just "Bearer sk-dot"

  it "reads llm_provider, llm_base_url, and model from layered settings" $
    withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
      writeSettings ws $ object
        [ "llm_provider" .= ("openai-compatible" :: Text)
        , "llm_base_url" .= (fixtureRoot fx <> "/settings/v1/")
        , "model" .= ("settings-model" :: Text) ]
      result <- runWith ws [] ["--print", "hi"]
      hrExit result `shouldBe` ExitSuccess
      req <- singleRequest fx
      crPath req `shouldBe` "/settings/v1/chat/completions"
      field ["model"] req `shouldBe` Just (String "settings-model")

  it "passes the model identifier unchanged and ignores OpenRouter model variables in compatible mode" $
    withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
      writeDotenv ws ["OPENROUTER_API_KEY=or-key", "or/line-two-model", "OPENROUTER_MODEL=or/named"]
      result <- runWith ws [("OPENROUTER_MODEL", "or/process"), ("OPENAI_MODEL", "Qwen/Qwen3-Coder:Q4_K_M")]
        ["--print", "--provider", "openai-compatible", "--base-url", T.unpack (fixtureBaseUrl fx), "hi"]
      hrExit result `shouldBe` ExitSuccess
      req <- singleRequest fx
      field ["model"] req `shouldBe` Just (String "Qwen/Qwen3-Coder:Q4_K_M")
      requestHeader "authorization" req `shouldBe` Nothing

  it "requires a model in compatible mode and sends nothing without one" $
    withSandbox $ \ws -> withInferenceFixture [] $ \fx -> do
      result <- runWith ws [("OPENROUTER_MODEL", "or/model")]
        ["--print", "--provider", "openai-compatible", "--base-url", T.unpack (fixtureBaseUrl fx), "hi"]
      hrExit result `shouldNotBe` ExitSuccess
      output result `shouldSatisfy` ("openai-compatible model not specified" `isInfixOf`)
      shouldHaveNoRequests fx

  forM_ [("unknown", "anthropic"), ("blank", "   ")] $ \(label, value) ->
    it ("rejects an " <> label <> " effective provider without falling back") $
      withSandbox $ \ws -> withInferenceFixture [] $ \fx -> do
        result <- runWith ws
          [ ("HACH_PROVIDER", value), ("OPENROUTER_API_KEY", "or-key"), ("OPENROUTER_MODEL", "m") ]
          ["--print", "--base-url", T.unpack (fixtureBaseUrl fx), "hi"]
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` ("provider" `isInfixOf`)
        output result `shouldSatisfy` ("openai-compatible" `isInfixOf`)
        shouldHaveNoRequests fx

  it "lets a valid flag supersede an invalid lower-priority provider and base URL" $
    withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
      writeSettings ws $ object ["llm_provider" .= ("nonsense" :: Text), "llm_base_url" .= ("ftp://x" :: Text)]
      result <- runWith ws [("OPENAI_BASE_URL", "http://user:pw@host/v1")]
        (compatible fx <> ["hi"])
      hrExit result `shouldBe` ExitSuccess
      _ <- singleRequest fx
      pure ()

  it "fails closed on malformed provider settings even when a flag would override them" $
    withSandbox $ \ws -> withInferenceFixture [] $ \fx -> do
      writeSettings ws $ object ["llm_provider" .= (5 :: Int)]
      result <- runWith ws [] (compatible fx <> ["hi"])
      hrExit result `shouldNotBe` ExitSuccess
      output result `shouldSatisfy` ("llm_provider" `isInfixOf`)
      shouldHaveNoRequests fx

  it "never uses OPENROUTER_API_KEY as a compatible credential or OPENAI_API_KEY for OpenRouter" $
    withSandbox $ \ws -> withInferenceFixture [] $ \fx -> do
      result <- runWith ws [("OPENAI_API_KEY", "sk-compat"), ("OPENROUTER_MODEL", "or/model")]
        ["--print", "--provider", "openrouter", "--base-url", T.unpack (fixtureBaseUrl fx), "hi"]
      hrExit result `shouldNotBe` ExitSuccess
      output result `shouldSatisfy` ("OPENROUTER_API_KEY is missing" `isInfixOf`)
      shouldNotLeak "sk-compat" result
      shouldHaveNoRequests fx

  it "ignores OPENAI_BASE_URL and llm_base_url when OpenRouter is selected" $
    withSandbox $ \ws -> withInferenceFixture [] $ \fx -> do
      writeSettings ws $ object ["llm_base_url" .= fixtureBaseUrl fx]
      -- Headless (non --print) mode prints the resolved endpoint in its banner;
      -- an empty task then exits before any request is made.
      result <- runWith ws
        [ ("HACH_PROVIDER", "openai-compatible"), ("OPENAI_BASE_URL", T.unpack (fixtureBaseUrl fx))
        , ("OPENROUTER_API_KEY", "or-key"), ("OPENROUTER_MODEL", "or/model") ]
        ["--no-tui", "--provider", "openrouter"]
      hrStdout result `shouldSatisfy`
        ("Provider:  openrouter (https://openrouter.ai/api/v1/chat/completions)" `isInfixOf`)
      shouldHaveNoRequests fx

  describe "OpenRouter mode" $ do
    it "keeps its key, model, attribution headers, and nested effort" $
      withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
        writeSettings ws $ object ["effort_level" .= ("high" :: Text)]
        result <- runWith ws
          [ ("OPENROUTER_API_KEY", "or-key"), ("OPENROUTER_MODEL", "openai/gpt-test")
          , ("OPENAI_API_KEY", "sk-compat"), ("OPENAI_MODEL", "compat-model") ]
          ["--print", "--base-url", T.unpack (fixtureBaseUrl fx), "hi"]
        hrExit result `shouldBe` ExitSuccess
        req <- singleRequest fx
        requestHeader "authorization" req `shouldBe` Just "Bearer or-key"
        requestHeader "http-referer" req `shouldSatisfy` isJust
        requestHeader "x-title" req `shouldSatisfy` isJust
        field ["model"] req `shouldBe` Just (String "openai/gpt-test")
        field ["reasoning", "effort"] req `shouldBe` Just (String "high")
        field ["reasoning_effort"] req `shouldBe` Nothing

    it "keeps the legitimate legacy second-line model" $
      withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
        writeDotenv ws ["OPENROUTER_API_KEY=or-key", "vendor/legacy-model", "OPENROUTER_MODEL=vendor/named"]
        result <- runWith ws [] ["--print", "--base-url", T.unpack (fixtureBaseUrl fx), "hi"]
        hrExit result `shouldBe` ExitSuccess
        req <- singleRequest fx
        field ["model"] req `shouldBe` Just (String "vendor/legacy-model")

    forM_ [ prefix <> name | name <- ["HACH_PROVIDER", "OPENAI_API_KEY", "OPENAI_MODEL", "OPENAI_BASE_URL"]
                           , prefix <- ["", "export "] ] $ \assignment ->
      it ("never treats a second-line " <> T.unpack assignment <> " assignment as the model") $
        withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
          writeDotenv ws
            [ "OPENROUTER_API_KEY=or-key", assignment <> "=openrouter", "OPENROUTER_MODEL=vendor/named" ]
          result <- runWith ws [] ["--print", "--base-url", T.unpack (fixtureBaseUrl fx), "hi"]
          hrExit result `shouldBe` ExitSuccess
          req <- singleRequest fx
          field ["model"] req `shouldBe` Just (String "vendor/named")
          requestHeader "authorization" req `shouldBe` Just "Bearer or-key"

--------------------------------------------------------------------------------
-- Base URLs
--------------------------------------------------------------------------------

baseUrlSpec :: Spec
baseUrlSpec = describe "base URL handling" $ do
  forM_ [ ("/v1", "/v1/chat/completions")
        , ("/v1///", "/v1/chat/completions")
        , ("", "/chat/completions")
        , ("/", "/chat/completions")
        , ("/gateway/openai/v1", "/gateway/openai/v1/chat/completions")
        , ("/team%20a/v1", "/team%20a/v1/chat/completions")
        ] $ \(suffix, expected) ->
    it ("posts " <> show suffix <> " to " <> BC.unpack expected) $
      withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
        result <- runWith ws [] ["--print", "--provider", "openai-compatible", "--model", "m"
                                , "--base-url", T.unpack (fixtureRoot fx) <> suffix, "hi"]
        hrExit result `shouldBe` ExitSuccess
        req <- singleRequest fx
        crPath req `shouldBe` expected

  it "preserves IPv6 hosts and ports in the endpoint" $
    withSandbox $ \ws -> do
      result <- runWith ws [] ["--no-tui", "--provider", "openai-compatible", "--model", "m"
                              , "--base-url", "http://[::1]:8080/v1/"]
      hrStdout result `shouldSatisfy` ("(http://[::1]:8080/v1/chat/completions)" `isInfixOf`)

  forM_ [ "/v1/chat/completions", "/v1/Chat/Completions/", "/v1?x=1", "/v1#frag"
        , "/v1/%zz", "/v 1" ] $ \suffix ->
    it ("rejects " <> show suffix <> " before any request") $
      withSandbox $ \ws -> withInferenceFixture [] $ \fx -> do
        result <- runWith ws [] ["--print", "--provider", "openai-compatible", "--model", "m"
                                , "--base-url", T.unpack (fixtureRoot fx) <> suffix, "hi"]
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` ("Invalid base URL" `isInfixOf`)
        shouldHaveNoRequests fx

  forM_ [ "ftp://example.com/v1", "http://user:secret@127.0.0.1/v1", "http:///v1"
        , "http://127.0.0.1:99999/v1", "http://127.0.0.1:port/v1", "localhost:8080/v1", "   " ] $ \url ->
    it ("rejects " <> show url <> " before any request") $
      withSandbox $ \ws -> do
        result <- runWith ws [("OPENAI_BASE_URL", url)]
          ["--print", "--provider", "openai-compatible", "--model", "m", "hi"]
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` ("Invalid base URL" `isInfixOf`)

  it "rejects a credential-bearing base URL without echoing the credentials" $
    withSandbox $ \ws -> withInferenceFixture [] $ \fx -> do
      result <- runWith ws [] ["--print", "--provider", "openai-compatible", "--model", "m"
                              , "--base-url", "http://user:hunter2-pw@127.0.0.1/v1", "hi"]
      hrExit result `shouldNotBe` ExitSuccess
      output result `shouldSatisfy` ("must not contain user credentials" `isInfixOf`)
      shouldNotLeak "hunter2-pw" result
      shouldHaveNoRequests fx

  it "rejects an API key containing a line break without echoing it" $
    withSandbox $ \ws -> withInferenceFixture [] $ \fx -> do
      result <- runWith ws [("OPENAI_API_KEY", "sk-first\r\nX-Evil: 1")] (compatible fx <> ["hi"])
      hrExit result `shouldNotBe` ExitSuccess
      output result `shouldSatisfy` ("OPENAI_API_KEY is invalid" `isInfixOf`)
      shouldNotLeak "sk-first" result
      shouldHaveNoRequests fx

--------------------------------------------------------------------------------
-- Request shape
--------------------------------------------------------------------------------

requestShapeSpec :: Spec
requestShapeSpec = describe "request shape" $ do
  it "sends messages, function tools, and automatic tool choice without reasoning options" $
    withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
      result <- runWith ws [] (compatible fx <> ["please list files"])
      hrExit result `shouldBe` ExitSuccess
      req <- singleRequest fx
      let messages = maybe [] arrayItems (field ["messages"] req)
          roles = mapMaybe (jsonAt ["role"]) messages
          tools = maybe [] arrayItems (field ["tools"] req)
      roles `shouldBe` [String "system", String "user"]
      (jsonAt ["content"] =<< lastMaybe messages) `shouldBe` Just (String "please list files")
      field ["tool_choice"] req `shouldBe` Just (String "auto")
      mapMaybe (jsonAt ["type"]) tools `shouldSatisfy` all (== String "function")
      mapMaybe (jsonAt ["function", "name"]) tools `shouldSatisfy` elem (String "write_file")
      field ["reasoning"] req `shouldBe` Nothing
      field ["reasoning_effort"] req `shouldBe` Nothing

  it "sends a configured effort as top-level reasoning_effort only" $
    withSandbox $ \ws -> withInferenceFixture [completion "ok"] $ \fx -> do
      writeSettings ws $ object ["effort_level" .= ("low" :: Text)]
      result <- runWith ws [] (compatible fx <> ["hi"])
      hrExit result `shouldBe` ExitSuccess
      req <- singleRequest fx
      field ["reasoning_effort"] req `shouldBe` Just (String "low")
      field ["reasoning"] req `shouldBe` Nothing

  it "surfaces an endpoint's rejection of the effort without retrying or dropping it" $
    withSandbox $ \ws -> withInferenceFixture
      [replyJson 400 (object ["error" .= object ["message" .= ("reasoning_effort is not supported" :: Text)]])] $ \fx -> do
        writeSettings ws $ object ["effort_level" .= ("high" :: Text)]
        result <- runWith ws [] (compatible fx <> ["hi"])
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` ("reasoning_effort is not supported" `isInfixOf`)
        req <- singleRequest fx
        field ["reasoning_effort"] req `shouldBe` Just (String "high")
  where
    lastMaybe xs = if null xs then Nothing else Just (last xs)

--------------------------------------------------------------------------------
-- Tool cycle
--------------------------------------------------------------------------------

toolCycleSpec :: Spec
toolCycleSpec = describe "tool cycle" $ do
  it "writes and reads a real file, correlating tool results by call id" $
    withSandbox $ \ws -> withInferenceFixture
      [ toolCallCompletion [toolCall "call_write" "write_file" (object ["path" .= ("notes.txt" :: Text), "content" .= ("alpha\nbeta\n" :: Text)])] Nothing
      , toolCallCompletion [toolCall "call_read" "read_file" (object ["path" .= ("notes.txt" :: Text)])] Nothing
      , completion "all done"
      ] $ \fx -> do
        result <- runWith ws [] (compatible fx <> ["--permission-mode", "acceptEdits", "write notes"])
        hrExit result `shouldBe` ExitSuccess
        hrStdout result `shouldSatisfy` ("all done" `isInfixOf`)
        BS.readFile (ws </> "notes.txt") >>= (`shouldBe` "alpha\nbeta\n")
        [_, second, third] <- fixtureRequests fx
        map fst (toolResults second) `shouldBe` ["call_write"]
        map fst (toolResults third) `shouldBe` ["call_write", "call_read"]
        (lookup "call_read" (toolResults third)) `shouldSatisfy` maybe False (T.isInfixOf "alpha\nbeta")

  it "runs several tool calls from one completion" $
    withSandbox $ \ws -> withInferenceFixture
      [ toolCallCompletion
          [ toolCall "call_a" "write_file" (object ["path" .= ("a.txt" :: Text), "content" .= ("A" :: Text)])
          , toolCall "call_b" "write_file" (object ["path" .= ("b.txt" :: Text), "content" .= ("B" :: Text)])
          ] Nothing
      , completion "wrote both"
      ] $ \fx -> do
        result <- runWith ws [] (compatible fx <> ["--permission-mode", "acceptEdits", "write"])
        hrExit result `shouldBe` ExitSuccess
        BS.readFile (ws </> "a.txt") >>= (`shouldBe` "A")
        BS.readFile (ws </> "b.txt") >>= (`shouldBe` "B")
        [_, second] <- fixtureRequests fx
        map fst (toolResults second) `shouldBe` ["call_a", "call_b"]

  it "enforces a denied write through the same connection" $
    withSandbox $ \ws -> withInferenceFixture
      [ toolCallCompletion [toolCall "call_deny" "write_file" (object ["path" .= ("blocked.txt" :: Text), "content" .= ("x" :: Text)])] Nothing
      , completion "could not write"
      ] $ \fx -> do
        result <- runWith ws [] (compatible fx <> ["write"])
        -- The existing headless contract: an all-denied task is blocked.
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` ("Task blocked" `isInfixOf`)
        doesFileExist (ws </> "blocked.txt") >>= (`shouldBe` False)
        [_, second] <- fixtureRequests fx
        map fst (toolResults second) `shouldBe` ["call_deny"]

--------------------------------------------------------------------------------
-- Response variations
--------------------------------------------------------------------------------

responseSpec :: Spec
responseSpec = describe "response handling" $ do
  it "accepts a completion without usage and reports usage without cost in verbose mode" $
    withSandbox $ \ws -> withInferenceFixture [completionWith "counted" (Just (usage 12 5 Nothing))] $ \fx -> do
      result <- runWith ws [] ["--no-tui", "--provider", "openai-compatible", "--model", "m"
                              , "--base-url", T.unpack (fixtureBaseUrl fx), "count"]
      hrExit result `shouldBe` ExitSuccess
      hrStdout result `shouldSatisfy` ("Context tokens: 17 (prompt: 12, completion: 5)" `isInfixOf`)

  it "tolerates object tool arguments and content-part arrays" $
    withSandbox $ \ws -> withInferenceFixture
      [ replyJson 200 $ object
          [ "choices" .= [ object [ "message" .= object
              [ "role" .= ("assistant" :: Text)
              , "tool_calls" .= [ object
                  [ "id" .= ("call_obj" :: Text), "type" .= ("function" :: Text)
                  , "function" .= object [ "name" .= ("write_file" :: Text)
                                         , "arguments" .= object ["path" .= ("o.txt" :: Text), "content" .= ("obj" :: Text)] ] ] ] ] ] ] ]
      , replyJson 200 $ object
          [ "choices" .= [ object [ "message" .= object
              [ "role" .= ("assistant" :: Text)
              , "content" .= [object ["type" .= ("text" :: Text), "text" .= ("parts answer" :: Text)]] ] ] ] ]
      ] $ \fx -> do
        result <- runWith ws [] (compatible fx <> ["--permission-mode", "acceptEdits", "go"])
        hrExit result `shouldBe` ExitSuccess
        BS.readFile (ws </> "o.txt") >>= (`shouldBe` "obj")
        hrStdout result `shouldSatisfy` ("parts answer" `isInfixOf`)

  forM_
    [ ("an empty message", object ["choices" .= [object ["message" .= object ["role" .= ("assistant" :: Text), "content" .= ("  " :: Text)]]]], "no text or tool calls")
    , ("a usage-only body", object ["usage" .= usage 1 1 Nothing], "OpenAI-compatible API")
    , ("a standalone message", object ["role" .= ("assistant" :: Text), "content" .= ("hi" :: Text)], "OpenAI-compatible API")
    , ("a refusal-only message", object ["choices" .= [object ["message" .= object ["role" .= ("assistant" :: Text), "content" .= Null, "refusal" .= ("I can't help" :: Text)]]]], "refused the request: I can't help")
    , ("a content-filtered empty completion", object ["choices" .= [object ["finish_reason" .= ("content_filter" :: Text), "message" .= object ["role" .= ("assistant" :: Text), "content" .= Null]]]], "content filter")
    , ("empty choices", object ["choices" .= ([] :: [Value])], "empty choices")
    ] $ \(label, body, expected) ->
    it ("fails rather than succeeding on " <> label) $
      withSandbox $ \ws -> withInferenceFixture [replyJson 200 body] $ \fx -> do
        result <- runWith ws [] (compatible fx <> ["hi"])
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` (expected `isInfixOf`)
        output result `shouldNotSatisfy` ("OpenRouter" `isInfixOf`)
        _ <- singleRequest fx
        pure ()

  it "keeps ordinary refusal text in content as a normal answer" $
    withSandbox $ \ws -> withInferenceFixture [completion "I won't do that."] $ \fx -> do
      result <- runWith ws [] (compatible fx <> ["hi"])
      hrExit result `shouldBe` ExitSuccess
      hrStdout result `shouldSatisfy` ("I won't do that." `isInfixOf`)

--------------------------------------------------------------------------------
-- Failures, redaction, redirects
--------------------------------------------------------------------------------

secretKey :: String
secretKey = "sk-fixture-secret-0123456789"

failureSpec :: Spec
failureSpec = describe "failures" $ do
  let secret = T.pack secretKey
      cases =
        [ ("401 structured", replyJson 401 (object ["error" .= object ["message" .= ("invalid key " <> secret :: Text)]]), "HTTP 401")
        , ("429 plain text", reply 429 (LBC.pack ("slow down " <> secretKey)), "HTTP 429")
        , ("500 plain text", reply 500 "upstream exploded", "upstream exploded")
        , ("503 empty body", reply 503 "", "HTTP 503")
        , ("2xx error envelope", replyJson 200 (object ["error" .= object ["message" .= ("quota gone" :: Text)]]), "quota gone")
        , ("non-2xx completion-shaped body", Reply 502 [] (Aeson.encode (object ["choices" .= [object ["message" .= object ["role" .= ("assistant" :: Text), "content" .= ("looks fine" :: Text)]]]])), "HTTP 502")
        , ("malformed JSON", reply 200 "{not json", "JSON")
        , ("non-UTF-8 body", Reply 500 [] (LBC.pack "bad \xff\xfe bytes"), "bad")
        , ("terminal control characters", reply 500 (LBC.pack "evil \ESC[2J clear"), "evil")
        ]
  forM_ cases $ \(label, script, expected) ->
    it ("reports " <> label <> " once, safely, and without the key") $
      withSandbox $ \ws -> withInferenceFixture [script] $ \fx -> do
        result <- runWith ws [("OPENAI_API_KEY", secretKey)] (compatible fx <> ["hi"])
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` ("OpenAI-compatible API" `isInfixOf`)
        output result `shouldSatisfy` (expected `isInfixOf`)
        output result `shouldNotSatisfy` ("OpenRouter" `isInfixOf`)
        output result `shouldNotSatisfy` ('\ESC' `elem`)
        shouldNotLeak secretKey result
        savedArtifactsShouldNotContain ws secretKey
        _ <- singleRequest fx
        pure ()

  it "bounds an oversized error body and redacts a key straddling the excerpt boundary" $
    withSandbox $ \ws -> do
      let body = LBC.pack (replicate 2040 'x' <> secretKey <> replicate 20000 'y')
      withInferenceFixture [reply 500 body] $ \fx -> do
        result <- runWith ws [("OPENAI_API_KEY", secretKey)] (compatible fx <> ["hi"])
        hrExit result `shouldNotBe` ExitSuccess
        -- The failure is reported by the event log and by the result, each
        -- as one bounded excerpt.
        map length (lines (output result)) `shouldSatisfy` all (<= 2200)
        length (output result) `shouldSatisfy` (< 6000)
        output result `shouldSatisfy` ("[truncated]" `isInfixOf`)
        shouldNotLeak secretKey result
        shouldNotLeak (take 12 secretKey) result

  it "emits valid inference-error JSON in JSON print mode" $
    withSandbox $ \ws -> withInferenceFixture
      [replyJson 401 (object ["error" .= object ["message" .= ("bad " <> secret :: Text)]])] $ \fx -> do
        result <- runWith ws [("OPENAI_API_KEY", secretKey)] (compatible fx <> ["--output-format", "json", "hi"])
        hrExit result `shouldNotBe` ExitSuccess
        case Aeson.decodeStrict (BC.pack (hrStdout result)) of
          Just v -> jsonAt ["error"] v `shouldSatisfy` \case
            Just (String err) -> "OpenAI-compatible API error (HTTP 401)" `T.isInfixOf` err
            _ -> False
          Nothing -> expectationFailure ("stdout is not JSON: " <> hrStdout result)
        shouldNotLeak secretKey result

  it "reports a connection failure against the selected endpoint" $
    withSandbox $ \ws -> do
      port <- closedPort
      result <- runWith ws [] ["--print", "--provider", "openai-compatible", "--model", "m"
                              , "--base-url", "http://127.0.0.1:" <> show port <> "/v1", "hi"]
      hrExit result `shouldNotBe` ExitSuccess
      output result `shouldSatisfy` ("OpenAI-compatible API request failed" `isInfixOf`)

  it "does not resend a request when a reused connection closes without answering" $
    withSandbox $ \ws -> withInferenceFixture
      [ keepAlive (toolCallCompletion [toolCall "call_ls" "list_dir" (object ["path" .= ("." :: Text)])] Nothing)
      , Hangup
      , completion "should not be reached"
      ] $ \fx -> do
        result <- runWith ws [] (compatible fx <> ["look around"])
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` ("OpenAI-compatible API request failed" `isInfixOf`)
        length <$> fixtureRequests fx `shouldReturn` 2

  it "refuses a cross-origin redirect without contacting the target" $
    withSandbox $ \ws -> withInferenceFixture [completion "should not be reached"] $ \target ->
      withInferenceFixture
        [Reply 302 [("Location", TE.encodeUtf8 (fixtureBaseUrl target <> "/chat/completions"))] ""] $ \fx -> do
          result <- runWith ws [("OPENAI_API_KEY", secretKey)] (compatible fx <> ["hi"])
          hrExit result `shouldNotBe` ExitSuccess
          output result `shouldSatisfy` ("redirect refused" `isInfixOf`)
          _ <- singleRequest fx
          shouldHaveNoRequests target

  it "refuses a same-origin redirect without a second request" $
    withSandbox $ \ws -> withInferenceFixture
      [Reply 307 [("Location", "/v2/chat/completions")] "", completion "should not be reached"] $ \fx -> do
        result <- runWith ws [] (compatible fx <> ["hi"])
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` ("redirect refused" `isInfixOf`)
        _ <- singleRequest fx
        pure ()

--------------------------------------------------------------------------------
-- Goal evaluation
--------------------------------------------------------------------------------

goalSpec :: Spec
goalSpec = describe "goal evaluation" $ do
  it "evaluates a met goal through the same connection without tools" $
    withSandbox $ \ws -> withInferenceFixture [completion "worked on it", evaluatorReply "met" "looks done"] $ \fx -> do
      writeSettings ws $ object ["effort_level" .= ("medium" :: Text)]
      result <- runWith ws [("OPENAI_API_KEY", "sk-goal")] (compatible fx <> ["/goal the work is done"])
      hrExit result `shouldBe` ExitSuccess
      [turn, evaluation] <- fixtureRequests fx
      crPath evaluation `shouldBe` crPath turn
      requestHeader "authorization" evaluation `shouldBe` Just "Bearer sk-goal"
      field ["model"] evaluation `shouldBe` Just (String "local-model")
      field ["reasoning_effort"] evaluation `shouldBe` Just (String "medium")
      field ["tools"] evaluation `shouldBe` Nothing
      field ["tool_choice"] evaluation `shouldBe` Nothing
      field ["tools"] turn `shouldSatisfy` isJust

  it "reports an impossible goal as a failure with the evaluator's reason" $
    withSandbox $ \ws -> withInferenceFixture [completion "tried", evaluatorReply "impossible" "no such file can exist"] $ \fx -> do
      result <- runWith ws [] (compatible fx <> ["/goal a contradiction"])
      hrExit result `shouldNotBe` ExitSuccess
      output result `shouldSatisfy` ("no such file can exist" `isInfixOf`)
      reqs <- fixtureRequests fx
      length reqs `shouldBe` 2

  it "continues after a not_yet_met verdict with another ordinary turn" $
    withSandbox $ \ws -> withInferenceFixture
      [completion "first", evaluatorReply "not_yet_met" "keep going", completion "second", evaluatorReply "met" "done"] $ \fx -> do
        result <- runWith ws [] (compatible fx <> ["/goal finish"])
        hrExit result `shouldBe` ExitSuccess
        reqs <- fixtureRequests fx
        map (isJust . field ["tools"]) reqs `shouldBe` [True, False, True, False]

  it "treats an evaluator HTTP failure as not yet met, without a transport retry, under a finite cap" $
    withSandbox $ \ws -> withInferenceFixture [completion "first", reply 500 "evaluator down"] $ \fx -> do
      result <- runWith ws [] (compatible fx <> ["--max-turns", "1", "/goal finish"])
      hrExit result `shouldNotBe` ExitSuccess
      output result `shouldSatisfy` ("evaluator down" `isInfixOf`)
      reqs <- fixtureRequests fx
      map (isJust . field ["tools"]) reqs `shouldBe` [True, False]
      map crPath reqs `shouldBe` ["/v1/chat/completions", "/v1/chat/completions"]

--------------------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------------------

sessionSpec :: Spec
sessionSpec = describe "sessions" $
  it "continues a saved session at a newly configured endpoint and key" $
    withSandbox $ \ws ->
      withInferenceFixture [completion "noted the zebra"] $ \old ->
        withInferenceFixture [completion "it was a zebra"] $ \new -> do
          first' <- runWith ws [("OPENAI_API_KEY", "sk-old-key")] (compatible old <> ["remember the word zebra"])
          hrExit first' `shouldBe` ExitSuccess
          second' <- runWith ws [("OPENAI_API_KEY", "sk-new-key")] (compatible new <> ["-c", "which animal?"])
          hrExit second' `shouldBe` ExitSuccess
          hrStdout second' `shouldSatisfy` ("it was a zebra" `isInfixOf`)
          oldReqs <- fixtureRequests old
          length oldReqs `shouldBe` 1
          req <- singleRequest new
          requestHeader "authorization" req `shouldBe` Just "Bearer sk-new-key"
          bodyText req `shouldSatisfy` T.isInfixOf "remember the word zebra"
          bodyText req `shouldSatisfy` T.isInfixOf "noted the zebra"
          savedArtifactsShouldNotContain ws "sk-old-key"
          savedArtifactsShouldNotContain ws "sk-new-key"
          savedArtifactsShouldNotContain ws (show (fixturePort old))

--------------------------------------------------------------------------------
-- Budgets
--------------------------------------------------------------------------------

budgetSpec :: Spec
budgetSpec = describe "budgets" $ do
  it "sends no request under a zero budget" $
    withSandbox $ \ws -> withInferenceFixture [] $ \fx -> do
      result <- runWith ws [] (compatible fx <> ["--max-budget-usd", "0", "hi"])
      hrExit result `shouldNotBe` ExitSuccess
      shouldHaveNoRequests fx

  it "stops before the next request once a tool turn's reported cost crosses the budget" $
    withSandbox $ \ws -> withInferenceFixture
      [toolCallCompletion [toolCall "call_ls" "list_dir" (object ["path" .= ("." :: Text)])] (Just (usage 10 10 (Just 1.5)))] $ \fx -> do
        result <- runWith ws [] (compatible fx <> ["--max-budget-usd", "1", "look"])
        hrExit result `shouldNotBe` ExitSuccess
        output result `shouldSatisfy` ("budget" `isInfixOf`)
        _ <- singleRequest fx
        pure ()

  it "keeps a final answer whose reported cost crosses the budget" $
    withSandbox $ \ws -> withInferenceFixture [completionWith "pricey answer" (Just (usage 10 10 (Just 5)))] $ \fx -> do
      result <- runWith ws [] (compatible fx <> ["--max-budget-usd", "1", "hi"])
      hrExit result `shouldBe` ExitSuccess
      hrStdout result `shouldSatisfy` ("pricey answer" `isInfixOf`)

--------------------------------------------------------------------------------
-- Local-only intents
--------------------------------------------------------------------------------

localOnlySpec :: Spec
localOnlySpec = describe "local-only intents" $ do
  let invalid =
        [ ("HACH_PROVIDER", "bogus"), ("OPENAI_BASE_URL", "ftp://nowhere")
        , ("OPENAI_API_KEY", "bad\nkey"), ("OPENROUTER_API_KEY", "") ]
  forM_ [ ["--help"], ["--version"], ["--exec", "echo local-ok"], ["--init"]
        , ["--provider", "bogus", "--base-url", "nope", "--version"] ] $ \args ->
    it ("runs " <> unwords args <> " with invalid inference configuration") $
      withSandbox $ \ws -> do
        result <- runWith ws invalid args
        hrExit result `shouldBe` ExitSuccess
        output result `shouldNotSatisfy` ("Configuration error" `isInfixOf`)
