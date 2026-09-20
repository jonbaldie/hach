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
import Data.Aeson (object, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
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
