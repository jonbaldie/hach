{-# LANGUAGE OverloadedStrings #-}

-- | PTY acceptance journeys for the public TUI against the inference
-- fixture (Issue #246). The real @hach@ executable runs on a pseudo-terminal
-- of fixed size and type; the test consumes its output continuously and
-- synchronises on observed requests and rendered text, never on sleeps.
module Hach.InferenceTuiSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (Handle, hClose, hFlush, hSetBinaryMode, hSetBuffering, BufferMode (..))
import System.Posix.IO (fdToHandle)
import System.Posix.Terminal (openPseudoTerminal)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

import Hach.InferenceFixture

spec :: Spec
spec = describe "TUI on a pseudo-terminal" $ do
  it "runs a turn with a Tool Card, switches model, reports unknown cost, and recovers from cancellation" $
    withSandbox $ \ws -> do
      writeWorkspaceFile ws (".claude/settings.json") "{\"effort_level\": \"low\"}"
      withInferenceFixture
        [ toolCallCompletion [toolCall "call_ls" "list_dir" (object ["path" .= ("." :: Text)])] (Just (usage 30 4 Nothing))
        , completionWith "fixture-answer-one" (Just (usage 40 6 Nothing))
        , completion "fixture-answer-two"
        , Stall
        , completion "fixture-answer-three"
        ] $ \fx ->
        withTui ws [("OPENAI_API_KEY", "sk-tui")]
          ["--provider", "openai-compatible", "--base-url", T.unpack (fixtureBaseUrl fx), "--model", "tui-model"] $ \tui -> do
            -- A normal turn with a Tool Card.
            submit tui "look around"
            awaitText tui "fixture-answer-one"
            screenText tui >>= (`shouldSatisfy` ("list_dir" `isInfixOf`))
            reqs1 <- fixtureRequests fx
            length reqs1 `shouldBe` 2

            -- Switching model keeps the connection and effort.
            submit tui "/model other-model"
            awaitText tui "Model switched to: other-model"
            submit tui "and again"
            awaitText tui "fixture-answer-two"
            [first', _, third] <- fixtureRequests fx
            field ["model"] first' `shouldBe` Just (String "tui-model")
            field ["model"] third `shouldBe` Just (String "other-model")
            requestHeader "authorization" third `shouldBe` Just "Bearer sk-tui"
            crPath third `shouldBe` crPath first'
            field ["reasoning_effort"] third `shouldBe` Just (String "low")

            -- Missing cost is reported as unknown, never estimated.
            submit tui "/cost"
            awaitText tui "not reported by the endpoint"
            screenText tui >>= (`shouldNotSatisfy` ("Estimated Cost" `isInfixOf`))

            -- Cancel an outstanding request, then run another task.
            submit tui "hang please"
            arrived <- awaitRequestCount fx 4
            arrived `shouldBe` True
            sendKeys tui "\ETX"
            awaitText tui "cancelled"
            submit tui "after the cancel"
            awaitText tui "fixture-answer-three"
            reqs3 <- fixtureRequests fx
            length reqs3 `shouldBe` 5
            screenText tui >>= (`shouldNotSatisfy` ("sk-tui" `isInfixOf`))
  it "reports the cost and cached tokens the endpoint returns, without estimating" $
    withSandbox $ \ws ->
      withInferenceFixture
        [ completionWith "fixture-priced-answer" (Just reportedUsage) ] $ \fx ->
        withTui ws []
          ["--provider", "openai-compatible", "--base-url", T.unpack (fixtureBaseUrl fx), "--model", "tui-model"] $ \tui -> do
            submit tui "what does it cost"
            awaitText tui "fixture-priced-answer"
            submit tui "/cost"
            awaitText tui "Reported API Cost: $0.0123"
            screen <- screenText tui
            screen `shouldSatisfy` ("12 cached" `isInfixOf`)
            screen `shouldNotSatisfy` ("Estimated Cost" `isInfixOf`)
            screen `shouldNotSatisfy` ("not reported by the endpoint" `isInfixOf`)
            length <$> fixtureRequests fx `shouldReturn` 1
  where
    field path req = requestJson req >>= jsonAt path
    reportedUsage = object
      [ "prompt_tokens" .= (50 :: Int)
      , "completion_tokens" .= (7 :: Int)
      , "total_tokens" .= (57 :: Int)
      , "prompt_tokens_details" .= object ["cached_tokens" .= (12 :: Int)]
      , "cost" .= (0.0123 :: Double)
      ]

--------------------------------------------------------------------------------
-- Pseudo-terminal driver
--------------------------------------------------------------------------------

data Tui = Tui
  { tuiInput  :: !Handle
  , tuiOutput :: !(IORef BS.ByteString)
  }

-- | Run hach on a fresh pseudo-terminal (200x50, xterm-256color), feeding
-- its output into a buffer until the scope ends, then quit it (killing it if
-- it does not exit promptly).
withTui :: FilePath -> [(String, String)] -> [String] -> (Tui -> IO a) -> IO a
withTui ws extraEnv args action = do
  executable <- hachExecutable
  environment <- sandboxEnvironment ws (("TERM", "xterm-256color") : extraEnv)
  (master, slave) <- openPseudoTerminal
  masterH <- fdToHandle master
  slaveH <- fdToHandle slave
  mapM_ (\h -> hSetBinaryMode h True >> hSetBuffering h NoBuffering) [masterH, slaveH]
  let command = (proc "/bin/sh" (["-c", "stty rows 50 cols 200 && exec \"$0\" \"$@\"", executable] ++ args))
        { cwd = Just ws
        , env = Just environment
        , std_in = UseHandle slaveH
        , std_out = UseHandle slaveH
        , std_err = UseHandle slaveH
        , new_session = True
        }
  buffer <- newIORef BS.empty
  let pump = do
        chunk <- try (BS.hGetSome masterH 65536) :: IO (Either SomeException BS.ByteString)
        case chunk of
          Right bytes | not (BS.null bytes) -> atomicModifyIORef' buffer (\b -> (b <> bytes, ())) >> pump
          _ -> pure ()
  bracket (createProcess command) (stop masterH) $ \(_, _, _, ph) ->
    withAsync pump $ \_ -> do
      let tui = Tui masterH buffer
      awaitText tui "tui-model"
      result <- action tui
      sendKeys tui "\DC1"  -- Ctrl+Q
      exited <- timeout 10000000 (waitForProcess ph)
      unless (isJust exited) (expectationFailure "hach did not exit after Ctrl+Q")
      pure result
  where
    stop masterH (_, _, _, ph) = do
      terminateProcess ph
      void (timeout 5000000 (waitForProcess ph))
      void (try (hClose masterH) :: IO (Either SomeException ()))

sendKeys :: Tui -> BS.ByteString -> IO ()
sendKeys tui keys = BS.hPut (tuiInput tui) keys >> hFlush (tuiInput tui)

-- | Type a line into the prompt and press Enter.
submit :: Tui -> Text -> IO ()
submit tui line = do
  sendKeys tui (BC.pack (T.unpack line))
  sendKeys tui "\r"

-- | Everything rendered so far, with escape sequences removed.
screenText :: Tui -> IO String
screenText tui = stripEscapes . BC.unpack <$> readIORef (tuiOutput tui)

-- | Wait (bounded) until the rendered output contains the text.
awaitText :: Tui -> String -> IO ()
awaitText tui needle = do
  found <- timeout 30000000 loop
  unless (isJust found) $ do
    screen <- screenText tui
    expectationFailure ("TUI never showed " <> show needle <> "; last output:\n" <> lastChars screen)
  where
    loop = do
      screen <- screenText tui
      unless (needle `isInfixOf` screen) (threadDelay 20000 >> loop)
    lastChars s = drop (length s - 3000) s

-- | Drop CSI, OSC, and two-byte escape sequences.
stripEscapes :: String -> String
stripEscapes = go
  where
    go ('\ESC' : '[' : rest) = go (drop 1 (dropWhile (not . isFinal) rest))
    go ('\ESC' : ']' : rest) = go (drop 1 (dropWhile (/= '\BEL') rest))
    go ('\ESC' : _ : rest) = go rest
    go (c : rest) = c : go rest
    go [] = []
    isFinal c = c >= '@' && c <= '~'
