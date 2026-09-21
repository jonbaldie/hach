{-# LANGUAGE OverloadedStrings #-}

module Hach.HooksSpec (spec) where

import Hach.Core (AgentAlgebra (..))
import Hach.Hooks
import Hach.Interpreter.IO
  ( IOEnv
  , IOEnvPermissions (..)
  , defaultIOEnvPermissions
  , ioAlgebraWithLog
  , newIOEnvWithPermissions
  )
import Hach.Types
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import Test.Hspec

spec :: Spec
spec = describe "Hach.Hooks" $ do
  describe "Matcher filtering" $ do
    let h1 = HookHandler (HookCommand "echo 1") (Just "run_command") False
        h2 = HookHandler (HookCommand "echo 2") (Just "*") False
        h3 = HookHandler (HookCommand "echo 3") (Just "read_file") False
        handlers = [h1, h2, h3]

    it "selects matching handlers for a tool" $ do
      let matched = filterMatchingHandlers (Just "run_command") handlers
      map (hhType) matched `shouldBe` [HookCommand "echo 1", HookCommand "echo 2"]

    it "includes wildcard matcher for any tool" $ do
      let matched = filterMatchingHandlers (Just "write_file") handlers
      map (hhType) matched `shouldBe` [HookCommand "echo 2"]

  -- A matcher names a tool, not a spelling of it. The model picks whichever
  -- registered alias it likes, so a hook guarding `run_command` has to fire
  -- for `Bash` too, or it is no guard at all.
  describe "Registry aliases" $ do
    let handlerFor m = HookHandler (HookCommand "echo 1") (Just m) False
        matchedBy m tool = filterMatchingHandlers (Just tool) [handlerFor m] /= []

    it "matches a canonical matcher against every alias of that tool" $ do
      matchedBy "run_command" "Bash" `shouldBe` True
      matchedBy "run_command" "bash" `shouldBe` True
      matchedBy "replace_file_content" "Edit" `shouldBe` True
      matchedBy "find_files" "Glob" `shouldBe` True

    it "matches an alias matcher against the canonical name" $ do
      matchedBy "Bash" "run_command" `shouldBe` True
      matchedBy "Edit" "replace_file_content" `shouldBe` True

    it "matches aliases case-insensitively" $
      matchedBy "RUN_COMMAND" "Bash" `shouldBe` True

    it "still rejects a matcher naming a different tool" $ do
      matchedBy "read_file" "Bash" `shouldBe` False
      matchedBy "run_command" "read_file" `shouldBe` False

    it "leaves unregistered tool names matched by exact spelling only" $ do
      matchedBy "mcp__srv__do" "mcp__srv__do" `shouldBe` True
      matchedBy "run_command" "mcp__srv__do" `shouldBe` False

  describe "Exit code protocol parsing" $ do
    it "exit code 0 returns pass with no modifications" $ do
      let res = parseHookOutput 0 "All checks passed"
      hrDecision res `shouldBe` Nothing
      hrAdditionalContext res `shouldBe` Nothing
      hrError res `shouldBe` Nothing

    it "exit code 2 parses structured JSON decision, context, and modified input" $ do
      let rawJson = "{\n\
        \  \"permissionDecision\": {\"decision\": \"deny\", \"reason\": \"Lint check failed\"},\n\
        \  \"additionalContext\": \"Fix lints first\",\n\
        \  \"modifiedToolInput\": {\"clean\": true}\n\
        \}"
          res = parseHookOutput 2 rawJson
      hrDecision res `shouldBe` Just (PermDeny "Lint check failed")
      hrAdditionalContext res `shouldBe` Just "Fix lints first"
      hrModifiedInput res `shouldBe` Just (object ["clean" .= True])

    it "non-0/2 exit code returns non-blocking error" $ do
      let res = parseHookOutput 1 "Command crashed"
      hrError res `shouldBe` Just "Command crashed"
      hrDecision res `shouldBe` Nothing

    it "captures stdout in hrError when command exits non-zero and stderr is empty" $ do
      let handler = HookHandler (HookCommand "echo 'policy violation'; exit 1") Nothing False
      res <- runHookHandler "." (object []) handler
      case hrError res of
        Just err -> ("policy violation" `T.isInfixOf` err) `shouldBe` True
        Nothing  -> expectationFailure "Expected hrError to be Just with error message"

  -- Regression for issue #167: the whole live hook chain, from the payload
  -- Core hands the interpreter down to the blocking decision.
  describe "Hook runtime (issue #167)" $ do
    let bashArgs = "{\"command\":\"echo via-alias\"}"

    it "blocks a run_command hook when the model calls the Bash alias" $ do
      res <- runLiveHook HookPreToolUse ("Bash " <> bashArgs)
      hrDecision res `shouldSatisfy` isDeny

    it "blocks a run_command hook when the model calls run_command" $ do
      res <- runLiveHook HookPreToolUse ("run_command " <> bashArgs)
      hrDecision res `shouldSatisfy` isDeny

    it "fires a post_tool_use run_command hook for the Bash alias" $ do
      res <- runLiveHook HookPostToolUse "Bash command finished"
      hrDecision res `shouldSatisfy` isDeny

    it "leaves an unrelated tool alone" $ do
      res <- runLiveHook HookPreToolUse ("read_file {\"path\":\"README.md\"}")
      hrDecision res `shouldBe` Nothing

  -- Regression for issue #168: http handlers used to return a pass without
  -- sending anything.
  describe "HTTP handlers (issue #168)" $ do
    let payload = object ["hook_event_name" .= ("stop" :: Text), "result" .= ("completed" :: Text)]

    it "POSTs the hook payload as JSON to the configured URL" $ do
      (res, request) <- withOneShotServer (httpResponse "200 OK" "") $ \url ->
        runHookHandler "." payload (HookHandler (HookHttp (url <> "/hook")) Nothing False)
      res `shouldBe` defaultHookResult
      BS.unpack request `shouldStartWith` "POST /hook HTTP/1.1"
      BS.unpack request `shouldContain` "Content-Type: application/json"
      BS.unpack request `shouldContain` "\"hook_event_name\":\"stop\""

    it "applies a decision returned in the response body" $ do
      let body = "{\"permissionDecision\":{\"decision\":\"deny\",\"reason\":\"not now\"}}"
      (res, _) <- withOneShotServer (httpResponse "200 OK" body) $ \url ->
        runHookHandler "." payload (HookHandler (HookHttp url) Nothing False)
      hrDecision res `shouldBe` Just (PermDeny "not now")

    it "reports a non-2xx response as a non-blocking error" $ do
      (res, _) <- withOneShotServer (httpResponse "500 Internal Server Error" "boom") $ \url ->
        runHookHandler "." payload (HookHandler (HookHttp url) Nothing False)
      hrDecision res `shouldBe` Nothing
      fmap ("500" `T.isInfixOf`) (hrError res) `shouldBe` Just True

    it "reports an unreachable URL as a non-blocking error" $ do
      res <- runHookHandler "." payload (HookHandler (HookHttp "http://127.0.0.1:1/hook") Nothing False)
      hrDecision res `shouldBe` Nothing
      hrError res `shouldSatisfy` (/= Nothing)

isDeny :: Maybe PermissionDecision -> Bool
isDeny (Just (PermDeny _)) = True
isDeny _                   = False

-- | An 'IOEnv' carrying one blocking hook matched on the canonical
-- @run_command@ name, for both tool-use events.
blockingRunCommandEnv :: IO IOEnv
blockingRunCommandEnv = do
  let handler = HookHandler (HookCommand "echo blocked; exit 2") (Just "run_command") False
      perms = defaultIOEnvPermissions
        { iopHooks = Map.fromList
            [ (HookPreToolUse, [handler])
            , (HookPostToolUse, [handler])
            ]
        }
  newIOEnvWithPermissions perms "test-key" "test-model" "." False

-- | Run a hook event through the real IO interpreter, using the same
-- @"<tool> <payload>"@ encoding the agent core sends.
runLiveHook :: HookEvent -> Text -> IO HookResult
runLiveHook ev payload = do
  env <- blockingRunCommandEnv
  interpRunHook (ioAlgebraWithLog (const (pure ())) env) ev payload

httpResponse :: BS.ByteString -> BS.ByteString -> BS.ByteString
httpResponse status body =
  "HTTP/1.1 " <> status <> "\r\nContent-Length: " <> BS.pack (show (BS.length body))
    <> "\r\nConnection: close\r\n\r\n" <> body

-- | Serve one HTTP request on a loopback port with a canned response, and
-- return the raw request alongside the action's result.
withOneShotServer :: BS.ByteString -> (Text -> IO a) -> IO (a, BS.ByteString)
withOneShotServer response action =
  bracket open close $ \sock -> do
    port <- socketPort sock
    withAsync (serve sock) $ \server -> do
      res <- action ("http://127.0.0.1:" <> T.pack (show port))
      -- A handler that never connects yields an empty request, not a hang.
      request <- timeout 5000000 (wait server)
      pure (res, fromMaybe "" request)
  where
    open = do
      sock <- socket AF_INET Stream defaultProtocol
      bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      listen sock 1
      pure sock
    serve sock = bracket (fst <$> accept sock) close $ \conn -> do
      request <- readRequest conn ""
      NSB.sendAll conn response
      pure request
    readRequest conn acc
      | complete acc = pure acc
      | otherwise = do
          chunk <- NSB.recv conn 4096
          if BS.null chunk then pure acc else readRequest conn (acc <> chunk)
    complete acc =
      let (headers, rest) = BS.breakSubstring "\r\n\r\n" acc
      in not (BS.null rest) && BS.length rest - 4 >= contentLength headers
    contentLength headers =
      case [ BS.drop 15 l | l <- BS.lines headers, BS.map toLowerAscii (BS.take 15 l) == "content-length:" ] of
        (v : _) -> maybe 0 fst (BS.readInt (BS.dropWhile (== ' ') v))
        []      -> 0
    toLowerAscii c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
