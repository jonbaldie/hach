{-# LANGUAGE OverloadedStrings #-}

module Hach.InterpreterIOSpec (spec) where

import Hach.Core (AgentAlgebra (..), agentLoop, foldAgentProgram)
import Hach.Env (resolveEffortLevel, resolvePermissionMode)
import Hach.Interpreter.IO
import Hach.Permissions (isProtectedPath)
import Hach.Settings (Settings (..), defaultSettings, loadLayeredSettings)
import Hach.Tools (ReplaceFileContentArgs (..), WriteFileArgs (..), executeCodingTool, executeReplaceFileContent, executeWriteFile)
import Hach.Types
import Hach.TUI.App
  ( awaitPermissionAsk
  , cancelPermissionAsk
  , newPermissionGate
  , resolveAskWithGate
  , respondPermission
  , runEnvForModel
  )
import Hach.TUI.State (updateTui)
import Hach.TUI.Types
import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, try)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, hFlush, openTempFile, readFile', stderr, stdout)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Process (callProcess)
import Test.Hspec

spec :: Spec
spec = (renderEventQuietSpec >>) $ describe "Hach.Interpreter.IO (permission + hook enforcement)" $ do
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

    describe "initializeProjectWorkspace" $ do
      it "creates a non-empty Markdown template in the active workspace" $ do
        env <- newIOEnv "k" "test-model" testDir False
        result <- initializeProjectWorkspace env
        result `shouldBe` ProjectInitialized
        content <- TIO.readFile (testDir </> "CLAUDE.md")
        T.strip content `shouldNotBe` ""
        T.isPrefixOf "# " content `shouldBe` True

      it "leaves an existing CLAUDE.md untouched" $ do
        let existing = "# Keep this file\n"
        TIO.writeFile (testDir </> "CLAUDE.md") existing
        env <- newIOEnv "k" "test-model" testDir False
        result <- initializeProjectWorkspace env
        result `shouldBe` ProjectAlreadyPresent
        TIO.readFile (testDir </> "CLAUDE.md") `shouldReturn` existing

      it "uses the active workspace after a worktree switch" $ do
        let activeWorkspace = testDir </> "active-workspace"
        createDirectoryIfMissing True activeWorkspace
        env <- newIOEnv "k" "test-model" testDir False
        writeIORef (ioCurrentWorkspace env) activeWorkspace
        result <- initializeProjectWorkspace env
        result `shouldBe` ProjectInitialized
        doesFileExist (activeWorkspace </> "CLAUDE.md") `shouldReturn` True
        doesFileExist (testDir </> "CLAUDE.md") `shouldReturn` False

      it "reports an actionable error when the workspace cannot be written" $ do
        let blockedWorkspace = testDir </> "not-a-directory"
        writeFile blockedWorkspace "blocked"
        env <- newIOEnv "k" "test-model" blockedWorkspace False
        result <- initializeProjectWorkspace env
        result `shouldSatisfy` \case
          ProjectInitializationFailed err ->
            "CLAUDE.md" `T.isInfixOf` err && not ("Initialized" `T.isInfixOf` err)
          _ -> False

    describe "interpCheckPermission" $ do
      it "denies write_file under plan mode while allowing reads" $ do
        env <- newIOEnvWithPermissions planPerms "k" "test-model" testDir False
        let alg = ioAlgebra env
        interpCheckPermission alg "write_file" "{\"path\":\"out.txt\"}"
          `shouldReturn` False
        interpCheckPermission alg "read_file" "{\"path\":\"out.txt\"}"
          `shouldReturn` True

      it "authorizes Edit aliases as workspace writes" $ do
        env <- newIOEnvWithPermissions planPerms "k" "test-model" testDir False
        let alg = ioAlgebra env
            args = "{\"path\":\"out.txt\",\"old_content\":\"old\",\"new_content\":\"new\"}"
        interpCheckPermission alg "Edit" args `shouldReturn` False
        setIOPermissionMode env ModeAcceptEdits
        interpCheckPermission alg "edit" args `shouldReturn` True

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

      it "does not auto-deny default-mode write_file when the ask resolver approves (Issue #91)" $ do
        env0 <- newIOEnv "k" "test-model" testDir False
        let env = env0 { ioResolveAsk = \_ _ _ -> pure True }
            alg = ioAlgebra env
        interpCheckPermission alg "write_file" "{\"path\":\"hello.txt\",\"content\":\"hello\"}"
          `shouldReturn` True

      it "keeps headless unresolved asks denied" $ do
        env <- newIOEnv "k" "test-model" testDir False
        interpCheckPermission (ioAlgebra env) "write_file" "{\"path\":\"hello.txt\"}"
          `shouldReturn` False

      it "does not consult the ask resolver for explicit policy denies" $ do
        env0 <- newIOEnv "k" "test-model" testDir False
        let env = env0 { ioResolveAsk = \_ _ _ -> pure True }
        interpCheckPermission (ioAlgebra env) "write_file" "{\"path\":\".git/config\"}"
          `shouldReturn` False

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
      let runSkillLoop env = do
            let skillCall = ToolCall "c1" "Skill" "{\"name\":\"plan-command-guard\"}"
            stepsRef <- newIORef
              [ \_ _ -> Right (AssistantResponse Nothing [skillCall] Nothing)
              , \_ _ -> Right (AssistantResponse (Just "done") [] Nothing)
              ] :: IO (IORef [[Message] -> [ToolDef] -> Either Text AssistantResponse])
            eventsRef <- newIORef [] :: IO (IORef [AgentEvent])
            let alg = (ioAlgebra env)
                  { interpPrompt = \_ _ -> do
                      steps <- readIORef stepsRef
                      case steps of
                        step : rest -> writeIORef stepsRef rest >> pure (step [] [])
                        [] -> pure (Right (AssistantResponse (Just "done") [] Nothing))
                  , interpLog = \ev -> modifyIORef' eventsRef (ev :)
                  }
                cfg = AgentConfig
                  { cfgModel = "test-model"
                  , cfgSystemPrompt = Nothing
                  , cfgMaxTurns = Nothing
                  }
            _ <- foldAgentProgram alg (agentLoop cfg [] [UserMsg "run the skill"])
            readIORef eventsRef

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

      it "rejects a skill before its dynamic command expands in plan mode" $ do
        let skillDir = testDir </> ".claude" </> "skills" </> "plan-command-guard"
            marker = testDir </> "dynamic-command-ran"
        createDirectoryIfMissing True skillDir
        TIO.writeFile (skillDir </> "SKILL.md") "---\nname: plan-command-guard\n---\n!touch dynamic-command-ran\n"
        env <- newIOEnvWithPermissions planPerms "k" "test-model" testDir False
        events <- runSkillLoop env
        doesFileExist marker `shouldReturn` False
        events `shouldContain` [EvPermissionDenied "Skill" "Permission denied by policy"]

      it "expands a skill command after normal command approval" $ do
        let skillDir = testDir </> ".claude" </> "skills" </> "plan-command-guard"
            marker = testDir </> "dynamic-command-ran"
        createDirectoryIfMissing True skillDir
        TIO.writeFile (skillDir </> "SKILL.md") "---\nname: plan-command-guard\n---\n!touch dynamic-command-ran\n"
        env0 <- newIOEnv "k" "test-model" testDir False
        let env = env0 { ioResolveAsk = \tool _ _ -> pure (tool == "Skill") }
        events <- runSkillLoop env
        doesFileExist marker `shouldReturn` True
        events `shouldNotContain` [EvPermissionDenied "Skill" "Permission denied by policy"]

    describe "permission-to-TUI ask path (Issue #91)" $ do
      let writeCall = ToolCall "c1" "write_file" "{\"path\":\"hello.txt\",\"content\":\"hello\"}"
          runWriteLoop env = do
            stepsRef <- newIORef
              [ \_ _ -> Right (AssistantResponse Nothing [writeCall] Nothing)
              , \_ _ -> Right (AssistantResponse (Just "done") [] Nothing)
              ] :: IO (IORef [[Message] -> [ToolDef] -> Either Text AssistantResponse])
            eventsRef <- newIORef [] :: IO (IORef [AgentEvent])
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
            result <- foldAgentProgram alg (agentLoop cfg [] [UserMsg "write hello.txt"])
            events <- readIORef eventsRef
            pure (result, events)

      it "executes the pending write once when the ask is approved" $ do
        env0 <- newIOEnv "k" "test-model" testDir False
        let env = env0 { ioResolveAsk = \_ _ _ -> pure True }
        (_result, events) <- runWriteLoop env
        doesFileExist (testDir </> "hello.txt") `shouldReturn` True
        events `shouldNotContain` [EvPermissionDenied "write_file" "Permission denied by policy"]

      it "leaves the write unapplied when the ask is denied" $ do
        env0 <- newIOEnv "k" "test-model" testDir False
        let env = env0 { ioResolveAsk = \_ _ _ -> pure False }
        (_result, events) <- runWriteLoop env
        doesFileExist (testDir </> "hello.txt") `shouldReturn` False
        events `shouldContain` [EvPermissionDenied "write_file" "Permission denied by policy"]

      it "pauses on PermAsk until the TUI gate answers, then approves" $ do
        env0 <- newIOEnv "k" "test-model" testDir False
        gate <- newPermissionGate
        let env = env0 { ioResolveAsk = resolveAskWithGate gate (\_ -> pure ()) }
        done <- newEmptyMVar
        _ <- forkIO $ do
          r <- try (runWriteLoop env) :: IO (Either SomeException ((AgentResult, [Message]), [AgentEvent]))
          putMVar done r
        mAsk <- awaitPermissionAsk gate 2000000
        case mAsk of
          Nothing -> expectationFailure "timed out waiting for permission ask"
          Just (askId, tool, _args, reason) -> do
            tool `shouldBe` "write_file"
            reason `shouldBe` "Tool execution requires approval: write_file"
            answered <- respondPermission gate askId True
            answered `shouldBe` True
        outcome <- takeMVar done
        case outcome of
          Left ex -> expectationFailure ("worker failed: " <> show ex)
          Right _ -> doesFileExist (testDir </> "hello.txt") `shouldReturn` True

      it "denies through the TUI gate without writing the file" $ do
        env0 <- newIOEnv "k" "test-model" testDir False
        gate <- newPermissionGate
        let env = env0 { ioResolveAsk = resolveAskWithGate gate (\_ -> pure ()) }
        done <- newEmptyMVar
        _ <- forkIO $ do
          r <- try (runWriteLoop env) :: IO (Either SomeException ((AgentResult, [Message]), [AgentEvent]))
          putMVar done r
        mAsk <- awaitPermissionAsk gate 2000000
        case mAsk of
          Nothing -> expectationFailure "timed out waiting for permission ask"
          Just (askId, _, _, _) -> do
            answered <- respondPermission gate askId False
            answered `shouldBe` True
        outcome <- takeMVar done
        case outcome of
          Left ex -> expectationFailure ("worker failed: " <> show ex)
          Right (_, events) -> do
            doesFileExist (testDir </> "hello.txt") `shouldReturn` False
            events `shouldContain` [EvPermissionDenied "write_file" "Permission denied by policy"]

      it "releases a waiting worker on cancel and ignores a stale approval" $ do
        env0 <- newIOEnv "k" "test-model" testDir False
        gate <- newPermissionGate
        let env = env0 { ioResolveAsk = resolveAskWithGate gate (\_ -> pure ()) }
        done <- newEmptyMVar
        tid <- forkIO $ do
          r <- try (runWriteLoop env) :: IO (Either SomeException ((AgentResult, [Message]), [AgentEvent]))
          putMVar done r
        mAsk <- awaitPermissionAsk gate 2000000
        case mAsk of
          Nothing -> expectationFailure "timed out waiting for permission ask"
          Just (askId, _, _, _) -> do
            cancelPermissionAsk gate
            killThread tid
            stale <- respondPermission gate askId True
            stale `shouldBe` False
        _ <- takeMVar done
        doesFileExist (testDir </> "hello.txt") `shouldReturn` False
        env2 <- newIOEnv "k" "test-model" testDir False
        let envLater = env2 { ioResolveAsk = resolveAskWithGate gate (\_ -> pure ()) }
        done2 <- newEmptyMVar
        _ <- forkIO $ do
          r <- try (runWriteLoop envLater) :: IO (Either SomeException ((AgentResult, [Message]), [AgentEvent]))
          putMVar done2 r
        mAsk2 <- awaitPermissionAsk gate 2000000
        case mAsk2 of
          Nothing -> expectationFailure "timed out waiting for second permission ask"
          Just (askId2, _, _, _) -> do
            answered <- respondPermission gate askId2 False
            answered `shouldBe` True
        outcome2 <- takeMVar done2
        case outcome2 of
          Left ex -> expectationFailure ("second worker failed: " <> show ex)
          Right _ -> doesFileExist (testDir </> "hello.txt") `shouldReturn` False

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

    describe "plan mode switching in IO interpreter" $ do
      it "updates the live permission mode for EnterPlanMode aliases" $ do
        env <- newIOEnv "k" "test-model" testDir False
        let alg = ioAlgebra env
        mapM_ (\name -> do
          _ <- interpTool alg (ToolCall "p1" name "{}")
          currentIOPermissionMode env `shouldReturn` ModePlan)
          ["EnterPlanMode", "enter_plan_mode"]

      it "restores the default permission mode for ExitPlanMode aliases" $ do
        env <- newIOEnvWithPermissions planPerms "k" "test-model" testDir False
        let alg = ioAlgebra env
        mapM_ (\name -> do
          _ <- interpTool alg (ToolCall "p1" name "{}")
          currentIOPermissionMode env `shouldReturn` ModeDefault)
          ["ExitPlanMode", "exit_plan_mode"]

    describe "worktree switching in IO interpreter" $ do
      it "fails ExitWorktree when not inside a worktree" $ do
        env <- newIOEnv "k" "test-model" testDir False
        let alg = ioAlgebra env
        res <- interpTool alg (ToolCall "w1" "ExitWorktree" "{}")
        res `shouldSatisfy` \case
          ToolError _ -> True
          ToolSuccess _ -> False

      it "switches the workspace used by subsequent tools on EnterWorktree and restores on ExitWorktree" $ do
        callProcess "git" ["-C", testDir, "init"]
        callProcess "git" ["-C", testDir, "config", "user.name", "Test"]
        callProcess "git" ["-C", testDir, "config", "user.email", "test@test.com"]
        callProcess "git" ["-C", testDir, "commit", "--allow-empty", "-m", "init"]
        env <- newIOEnv "k" "test-model" testDir False
        let alg = ioAlgebra env
        enterRes <- interpTool alg (ToolCall "w1" "EnterWorktree" "{\"name\":\"feat-isolation\"}")
        enterRes `shouldSatisfy` \case
          ToolSuccess msg -> "feat-isolation" `T.isInfixOf` msg
          ToolError _ -> False
        let wtPath = testDir </> ".agents" </> "worktrees" </> "feat-isolation"
        writeRes <- interpTool alg (ToolCall "c1" "write_file" "{\"path\":\"worktree-only.txt\",\"content\":\"isolated\"}")
        writeRes `shouldSatisfy` \case
          ToolSuccess _ -> True
          ToolError _ -> False
        doesFileExist (wtPath </> "worktree-only.txt") `shouldReturn` True
        doesFileExist (testDir </> "worktree-only.txt") `shouldReturn` False
        exitRes <- interpTool alg (ToolCall "w2" "ExitWorktree" "{}")
        exitRes `shouldSatisfy` \case
          ToolSuccess _ -> True
          ToolError _ -> False
        writeRootRes <- interpTool alg (ToolCall "c2" "write_file" "{\"path\":\"root-only.txt\",\"content\":\"root\"}")
        writeRootRes `shouldSatisfy` \case
          ToolSuccess _ -> True
          ToolError _ -> False
        doesFileExist (testDir </> "root-only.txt") `shouldReturn` True

      it "switches workspace via interpEnterWorktree and restores via interpExitWorktree" $ do
        let wtDir = testDir </> "custom-wt"
        createDirectoryIfMissing True wtDir
        env <- newIOEnv "k" "test-model" testDir False
        let alg = ioAlgebra env
        interpEnterWorktree alg wtDir
        _ <- interpTool alg (ToolCall "c1" "write_file" "{\"path\":\"wt.txt\",\"content\":\"hi\"}")
        doesFileExist (wtDir </> "wt.txt") `shouldReturn` True
        doesFileExist (testDir </> "wt.txt") `shouldReturn` False
        interpExitWorktree alg
        _ <- interpTool alg (ToolCall "c2" "write_file" "{\"path\":\"root.txt\",\"content\":\"hi\"}")
        doesFileExist (testDir </> "root.txt") `shouldReturn` True

    describe "effort_level propagation to OpenRouter requests" $ do
      let userDir = testDir </> "user-config"
          writeSettings rel json = do
            let path = testDir </> rel
            createDirectoryIfMissing True (takeDirectory path)
            writeFile path json
          withUserConfig action = do
            orig <- lookupEnv "CLAUDE_CONFIG_DIR"
            createDirectoryIfMissing True userDir
            setEnv "CLAUDE_CONFIG_DIR" userDir
            action `finally` case orig of
              Just v  -> setEnv "CLAUDE_CONFIG_DIR" v
              Nothing -> unsetEnv "CLAUDE_CONFIG_DIR"
          envFromLoadedSettings = do
            settings <- loadLayeredSettings testDir
            effort <- case resolveEffortLevel settings of
              Left err -> fail err
              Right e  -> pure e
            env0 <- newIOEnv "k" "openai/gpt-5.6-luna" testDir False
            pure env0 { ioEffortLevel = effort }
          productionJson env =
            Aeson.toJSON (chatRequestFor env [UserMsg "hello"] [] (Just "auto"))
          evaluatorJson env =
            Aeson.toJSON (chatRequestFor env [UserMsg "eval"] [] Nothing)

      it "emits reasoning.effort from loaded project settings" $ withUserConfig $ do
        writeSettings (".agents" </> "settings.json")
          "{\"effort_level\":\"high\",\"permission_mode\":\"default\"}"
        env <- envFromLoadedSettings
        reasoningEffort (productionJson env) `shouldBe` Just "high"

      it "lets local settings override project effort" $ withUserConfig $ do
        writeSettings (".agents" </> "settings.json") "{\"effort_level\":\"high\"}"
        writeSettings (".agents" </> "settings.local.json") "{\"effort_level\":\"low\"}"
        env <- envFromLoadedSettings
        reasoningEffort (productionJson env) `shouldBe` Just "low"

      it "omits reasoning when effort is unset" $ withUserConfig $ do
        writeSettings (".agents" </> "settings.json") "{\"permission_mode\":\"default\"}"
        env <- envFromLoadedSettings
        reasoningEffort (productionJson env) `shouldBe` Nothing

      it "includes effort on goal-evaluator requests" $ withUserConfig $ do
        writeSettings (".agents" </> "settings.json") "{\"effort_level\":\"high\"}"
        env <- envFromLoadedSettings
        reasoningEffort (evaluatorJson env) `shouldBe` Just "high"

      it "preserves effort when the active model changes" $ withUserConfig $ do
        writeSettings (".agents" </> "settings.json") "{\"effort_level\":\"high\"}"
        env <- envFromLoadedSettings
        let runEnv = runEnvForModel "openai/gpt-4o" env
        ioModel runEnv `shouldBe` "openai/gpt-4o"
        reasoningEffort (productionJson runEnv) `shouldBe` Just "high"

      it "rejects unsupported effort from loaded settings" $ withUserConfig $ do
        writeSettings (".agents" </> "settings.json") "{\"effort_level\":\"turbo\"}"
        settings <- loadLayeredSettings testDir
        case resolveEffortLevel settings of
          Left err -> err `shouldContain` "Unsupported effort_level: turbo"
          Right v  -> expectationFailure ("expected Left, got " <> show v)

reasoningEffort :: Aeson.Value -> Maybe Text
reasoningEffort (Aeson.Object o) =
  case KeyMap.lookup "reasoning" o of
    Just (Aeson.Object r) ->
      case KeyMap.lookup "effort" r of
        Just (Aeson.String e) -> Just e
        _ -> Nothing
    _ -> Nothing
reasoningEffort _ = Nothing

-- | '--print' / '-p' runs the interpreter with verbose off; Main then writes
-- the formatted result as the only stdout. Rendering must not add to it.
renderEventQuietSpec :: Spec
renderEventQuietSpec = describe "renderEventIO in quiet (--print) mode" $ do
  let notices =
        [ EvError "boom"
        , EvGoalSet "cond"
        , EvGoalEvaluated GoalNotYetMet "reason"
        , EvGoalAchieved "cond"
        , EvGoalFailed "cond" "reason"
        , EvGoalCleared "cond"
        , EvGoalBlocked "cond"
        , EvPermissionDenied "bash" "denied"
        , EvPermissionAsk 1 "bash" "{}" "ask"
        , EvNotificationSent "note"
        ]

  it "writes nothing for EvDone, leaving the answer to formatPrintResult" $ do
    (out, err) <- captureStdStreams (renderEventIO False (EvDone "4"))
    out `shouldBe` ""
    err `shouldBe` ""

  it "keeps notices off stdout and reports them on stderr" $
    mapM_ (\ev -> do
      (out, err) <- captureStdStreams (renderEventIO False ev)
      out `shouldBe` ""
      err `shouldNotBe` "") notices

  it "still prints the Final Answer banner when verbose" $ do
    (out, _) <- captureStdStreams (renderEventIO True (EvDone "4"))
    out `shouldContain` "Final Answer"
    out `shouldContain` "4"

captureStdStreams :: IO () -> IO (String, String)
captureStdStreams action = do
  tmp <- getTemporaryDirectory
  (outPath, outH) <- openTempFile tmp "hach-stdout"
  (errPath, errH) <- openTempFile tmp "hach-stderr"
  hFlush stdout
  hFlush stderr
  savedOut <- hDuplicate stdout
  savedErr <- hDuplicate stderr
  (do hDuplicateTo outH stdout
      hDuplicateTo errH stderr
      action)
    `finally` do
      hFlush stdout
      hFlush stderr
      hDuplicateTo savedOut stdout
      hDuplicateTo savedErr stderr
      hClose savedOut
      hClose savedErr
      hClose outH
      hClose errH
  out <- readFile' outPath
  err <- readFile' errPath
  removeFile outPath
  removeFile errPath
  pure (out, err)
