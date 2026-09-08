{-# LANGUAGE OverloadedStrings #-}

module Hach.InterpreterIOSpec (spec) where

import Hach.Core (AgentAlgebra (..), agentLoop, foldAgentProgram)
import Hach.Types (AgentConfig (..))
import Hach.Env (resolvePermissionMode)
import Hach.Interpreter.IO
import Hach.Permissions (isProtectedPath)
import Hach.Settings (Settings (..), defaultSettings)
import Hach.Tools (ReplaceFileContentArgs (..), WriteFileArgs (..), executeCodingTool, executeReplaceFileContent, executeWriteFile)
import Hach.Types
import Hach.TUI.State (updateTui)
import Hach.TUI.Types
import Control.Monad (when)
import Data.Aeson ((.=), object)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "Hach.Interpreter.IO (permission + hook enforcement)" $ do
  let testDir = "dist-newstyle/test-ioperms"
      planPerms = defaultIOEnvPermissions { iopInitialMode = ModePlan }
      bypassPerms = defaultIOEnvPermissions { iopInitialMode = ModeBypassPermissions }
      denyBashPerms = defaultIOEnvPermissions
        { iopRules = [PermissionRule RuleDeny (Just "bash") Nothing]
        }

  around_ (\action -> do
    exists <- doesDirectoryExist testDir
    when exists (removeDirectoryRecursive testDir)
    createDirectoryIfMissing True testDir
    action
    existsAfter <- doesDirectoryExist testDir
    when existsAfter (removeDirectoryRecursive testDir)) $ do

    describe "interpCheckPermission" $ do
      it "denies write_file under plan mode while allowing reads" $ do
        env <- newIOEnvWithPermissions planPerms "k" "test-model" testDir False
        let alg = ioAlgebra env
        interpCheckPermission alg "write_file" "{\"path\":\"out.txt\"}"
          `shouldReturn` False
        interpCheckPermission alg "read_file" "{\"path\":\"out.txt\"}"
          `shouldReturn` True

      it "denies writes to protected paths in default mode" $ do
        isProtectedPath ".git/config" `shouldBe` True
        env <- newIOEnv "k" "test-model" testDir False
        let alg = ioAlgebra env
        interpCheckPermission alg "write_file" "{\"path\":\".git/config\"}"
          `shouldReturn` False

      it "honours deny rules from settings" $ do
        env <- newIOEnvWithPermissions denyBashPerms "k" "test-model" testDir False
        let alg = ioAlgebra env
        interpCheckPermission alg "Bash" "{\"command\":\"ls\"}"
          `shouldReturn` False

      it "allows everything under bypassPermissions" $ do
        env <- newIOEnvWithPermissions bypassPerms "k" "test-model" testDir False
        let alg = ioAlgebra env
        interpCheckPermission alg "write_file" "{\"path\":\".git/config\"}"
          `shouldReturn` True

      it "switches enforcement live via setIOPermissionMode" $ do
        env <- newIOEnvWithPermissions bypassPerms "k" "test-model" testDir False
        let alg = ioAlgebra env
        interpCheckPermission alg "write_file" "{\"path\":\"out.txt\"}"
          `shouldReturn` True
        setIOPermissionMode env ModePlan
        currentIOPermissionMode env `shouldReturn` ModePlan
        interpCheckPermission alg "write_file" "{\"path\":\"out.txt\"}"
          `shouldReturn` False
        interpCheckPermission alg "read_file" "{\"path\":\"out.txt\"}"
          `shouldReturn` True

    describe "interpRunHook" $ do
      it "runs configured PreToolUse command hooks and blocks on exit code 2" $ do
        let hookCmd = "printf '%s' '{\"permissionDecision\":{\"decision\":\"deny\",\"reason\":\"no writes\"}}'; exit 2"
            hookPerms = defaultIOEnvPermissions
              { iopHooks = Map.singleton HookPreToolUse
                  [HookHandler (HookCommand hookCmd) Nothing False]
              }
        env <- newIOEnvWithPermissions hookPerms "k" "test-model" testDir False
        let alg = ioAlgebra env
        res <- interpRunHook alg HookPreToolUse "write_file {\"path\":\"x.txt\"}"
        hrDecision res `shouldBe` Just (PermDeny "no writes")

      it "passes through with no effect when no hooks are configured" $ do
        env <- newIOEnv "k" "test-model" testDir False
        res <- interpRunHook (ioAlgebra env) HookPreToolUse "write_file {}"
        res `shouldBe` defaultHookResult

    describe "agentLoop through ioAlgebra" $ do
      it "blocks a write_file tool call into a protected path end to end" $ do
        env <- newIOEnvWithPermissions planPerms "k" "test-model" testDir False
        eventsRef <- newIORef [] :: IO (IORef [AgentEvent])
        let call1 = ToolCall "c1" "write_file" "{\"path\":\".git/pwned.txt\",\"content\":\"pwned\"}"
        stepsRef <- newIORef
          [ \_ _ -> Right (AssistantResponse Nothing [call1] Nothing)
          , \_ _ -> Right (AssistantResponse (Just "done") [] Nothing)
          ] :: IO (IORef [[Message] -> [ToolDef] -> Either Text AssistantResponse])
        let alg = (ioAlgebra env)
              { interpPrompt = \msgs tools -> do
                  steps <- readIORef stepsRef
                  case steps of
                    (step : rest) -> do
                      writeIORef stepsRef rest
                      pure (step msgs tools)
                    [] -> pure (Right (AssistantResponse (Just "done") [] Nothing))
              , interpLog = \ev -> modifyIORef' eventsRef (ev :)
              }
            cfg = AgentConfig
              { cfgModel        = "test-model"
              , cfgSystemPrompt = Nothing
              , cfgMaxTurns     = Nothing
              }
        (_result, hist) <- foldAgentProgram alg (agentLoop cfg [] [UserMsg "write it"])
        hist `shouldSatisfy` any (\case
          ToolMsg _ _ c -> "denied" `T.isInfixOf` T.toLower c
          _ -> False)
        exists <- doesFileExist (testDir </> ".git" </> "pwned.txt")
        exists `shouldBe` False

    describe "executeWriteFile (tool layer)" $ do
      it "refuses to write into protected paths" $ do
        res <- executeWriteFile testDir (WriteFileArgs ".git/pwned.txt" "pwned")
        res `shouldSatisfy` \case
          ToolError e -> "Protected path" `T.isPrefixOf` e
          ToolSuccess _ -> False
        exists <- doesFileExist (testDir </> ".git" </> "pwned.txt")
        exists `shouldBe` False

      it "refuses protected paths through executeCodingTool" $ do
        res <- executeCodingTool testDir
          (ToolCall "c1" "write_file" "{\"path\":\".agents/evil.txt\",\"content\":\"evil\"}")
        res `shouldSatisfy` \case
          ToolError _ -> True
          ToolSuccess _ -> False
        exists <- doesFileExist (testDir </> ".agents" </> "evil.txt")
        exists `shouldBe` False

      it "refuses protected paths through executeReplaceFileContent" $ do
        res <- executeReplaceFileContent testDir
          (ReplaceFileContentArgs ".git/config" "old" "new")
        res `shouldSatisfy` \case
          ToolError e -> "Protected path" `T.isPrefixOf` e
          ToolSuccess _ -> False

    describe "resolvePermissionMode (CLI + settings threading)" $ do
      it "defaults to ModeDefault when nothing is configured" $ do
        resolvePermissionMode Nothing False defaultSettings `shouldBe` ModeDefault

      it "prefers the --permission-mode flag over settings" $ do
        resolvePermissionMode (Just ModePlan) False defaultSettings `shouldBe` ModePlan
        resolvePermissionMode (Just ModeDefault) False defaultSettings { setPermissionMode = Just ModeAuto }
          `shouldBe` ModeDefault

      it "falls back to settings' permission_mode" $ do
        resolvePermissionMode Nothing False defaultSettings { setPermissionMode = Just ModeAuto }
          `shouldBe` ModeAuto

      it "gives --dangerously-skip-permissions the highest precedence" $ do
        resolvePermissionMode Nothing True defaultSettings `shouldBe` ModeBypassPermissions
        resolvePermissionMode (Just ModePlan) True defaultSettings `shouldBe` ModeBypassPermissions

    describe "TUI /plan" $ do
      it "switches the live permission mode instead of only printing a notice" $ do
        let (s, actions) = updateTui (EvSubmit "/plan") (initialTuiState "test-model" Nothing)
        tsPermissionMode s `shouldBe` ModePlan
        actions `shouldContain` [ActionSetPermissionMode ModePlan]

      it "reports the current permission mode in /permissions" $ do
        let planned = (initialTuiState "test-model" Nothing) { tsPermissionMode = ModePlan }
            (s, _) = updateTui (EvSubmit "/permissions") planned
        tsHistory s `shouldContain` [DiNotice "Permissions policy: plan"]

      it "switches to acceptEdits on /fewer-permission-prompts" $ do
        let (s, actions) = updateTui (EvSubmit "/fewer-permission-prompts") (initialTuiState "test-model" Nothing)
        tsPermissionMode s `shouldBe` ModeAcceptEdits
        actions `shouldContain` [ActionSetPermissionMode ModeAcceptEdits]