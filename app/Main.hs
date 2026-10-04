{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Hach.Core
import Hach.CLI
import Hach.Env
import Hach.Inference (InferenceConnection(..), interfaceName)
import qualified Hach.Git as Git
import Hach.Interpreter.IO
import Hach.Memory
  ( ProjectInitializationResult(..)
  , initializeWorkspaceInstructionsFile
  , loadProjectInstructions
  , scaffoldInstructionsFileName
  )
import Hach.Sessions
  ( buildSessionHistory
  , generateSessionId
  , resolveSessionLoad
  , resolveSessionTarget
  , saveRunSession
  )
import Hach.Skills (discoverSkills, expandSlashInvokedPrompt)
import Hach.Settings (Settings (..))
import Hach.Tools
import Hach.TUI.App (runTui)
import Hach.Types
import Control.Exception (finally, tryJust)
import Control.Monad (when)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Environment (getArgs)
import Data.Version (showVersion)
import qualified Paths_hach as Paths
import System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)
import System.IO (stderr)
import System.IO.Error (isEOFError)

-- | Background tasks run in their own process groups, so nothing else would
-- stop them when Hach exits, however it exits.
main :: IO ()
main = runHach `finally` stopBackgroundTasks

runHach :: IO ()
runHach = do
  rawArgs <- getArgs
  cwd     <- getCurrentDirectory

  opts@CliOptions{..} <- case parseCliArgs rawArgs of
    Left err -> do
      putStrLn ("Argument error: " <> err)
      putStrLn cliUsageHint
      exitFailure
    Right parsed -> pure parsed

  let intent = startupIntent opts
  case intent of
    IntentHelp -> do
      putStr cliHelpText
      exitSuccess
    IntentVersion -> do
      putStrLn ("hach " <> showVersion Paths.version)
      exitSuccess
    _ -> pure ()

  activeWorkspace <- resolveStartupWorkspace cwd optWorktree

  case intent of
    IntentExec cmd -> do
      (code, out, err) <- runExecCommand activeWorkspace cmd
      TIO.putStr out
      TIO.hPutStr stderr err
      exitWith code
    IntentInit -> do
      result <- initializeWorkspaceInstructionsFile activeWorkspace
      case result of
        ProjectInitialized -> do
          putStrLn ("Initialized " <> scaffoldInstructionsFileName <> " guidelines template.")
          exitSuccess
        ProjectAlreadyPresent -> do
          putStrLn (scaffoldInstructionsFileName <> " already exists; left it unchanged.")
          exitSuccess
        ProjectInitializationFailed err -> do
          TIO.putStrLn ("Error: " <> err)
          exitFailure
    IntentTui -> pure ()
    IntentHeadless -> pure ()
    IntentHelp -> pure ()
    IntentVersion -> pure ()

  -- Under '--print' even a configuration failure is the run's result, so it
  -- honours '--output-format' like any other failed run.
  let failConfiguration :: String -> IO a
      failConfiguration err = do
        if optPrint
          then TIO.putStrLn (formatPrintResult optOutputFormat (AgentFailed (T.pack err)))
          else putStrLn err
        exitFailure

  envRes <- resolveEnvConfig (InferenceFlags optProvider optBaseUrl optModel) (Just ".env")
  EnvConfig{..} <- either (failConfiguration . renderEnvError) pure envRes

  effort <- either (failConfiguration . ("Configuration error: " <>)) pure
    (resolveEffortLevel envSettings)

  let perms = defaultIOEnvPermissions
        { iopInitialMode = resolvePermissionMode optPermissionMode optDangerouslySkipPerms envSettings
        , iopRules       = setPermissionRules envSettings
        , iopHooks       = setHooks envSettings
        }
  -- Additional working directories are resolved against the startup cwd so a
  -- relative @--add-dir@ keeps meaning the same directory after the agent
  -- enters a worktree.
  workingDirs <- mapM makeAbsolute (resolveWorkingDirs optAddDir envSettings)
  ioEnv0 <- newIOEnvWithPermissions perms envConnection envModel cwd (headlessVerbose opts)
  let ioEnv = ioEnv0 { ioEffortLevel = effort, ioWorkingDirs = workingDirs }
  case optWorktree of
    Nothing -> pure ()
    Just _  -> interpEnterWorktree (ioAlgebra ioEnv) activeWorkspace

  targetWorkspace <- currentIOWorkspace ioEnv
  let sessionTarget = resolveSessionTarget optContinue optResume optSessionId
  sessionRes <- resolveSessionLoad targetWorkspace sessionTarget
  (activeSid, mLoadedSession) <- case sessionRes of
    Left err -> do
      if optPrint
        then TIO.putStrLn (formatPrintResult optOutputFormat (AgentFailed (T.pack err)))
        else putStrLn err
      exitFailure
    Right Nothing -> do
      sid <- generateSessionId
      pure (sid, Nothing)
    Right (Just sess@(info, _)) ->
      pure (siId info, Just sess)

  let mPrevInfo = fmap fst mLoadedSession
      mLoadedHistory = fmap snd mLoadedSession

  _ <- runIO ioEnv (sessionStartHook activeSid (isJust mLoadedSession))

  let maxBudgetUsd = resolveMaxBudgetUsd optMaxBudgetUsd envSettings

  case intent of
    IntentTui -> runTui ioEnv optPrompt optMaxTurns maxBudgetUsd optAppendSystemPrompt (setTheme envSettings) activeSid mLoadedSession
    _ -> do
      currentWorkspace <- currentIOWorkspace ioEnv
      when (headlessEmitsBanners opts) $ do
        putStrLn "========================================================"
        putStrLn "  Haskell Agentic Coding Harness (hach)                 "
        putStrLn "========================================================"
        putStrLn ("Workspace: " <> currentWorkspace)
        putStrLn ("Provider:  " <> T.unpack (interfaceName (icInterface envConnection))
                    <> " (" <> T.unpack (icEndpoint envConnection) <> ")")
        putStrLn ("Model:     " <> T.unpack envModel)
        putStrLn ("Permissions: " <> T.unpack (permissionModeName (iopInitialMode perms)))
        putStrLn "========================================================"

      taskPrompt <- case optPrompt of
        Just p  -> pure p
        Nothing -> do
          when (not optPrint) $
            putStrLn "Enter your task/request:"
          result <- tryJust (\err -> if isEOFError err then Just () else Nothing) TIO.getLine
          pure (either (const T.empty) id result)

      when (T.null (T.strip taskPrompt)) $ do
        let err = "Empty task prompt provided. Exiting."
        if optPrint
          then TIO.putStrLn (formatPrintResult optOutputFormat (AgentFailed err))
          else TIO.putStrLn err
        exitFailure

      skills <- discoverSkills currentWorkspace
      mGuidelines <- loadProjectInstructions currentWorkspace
      let sysPrompt = buildSystemPromptWithAppend mGuidelines optAppendSystemPrompt

      case validateHeadlessGoalPrompt taskPrompt of
        Just (Left err) -> do
          if optPrint
            then TIO.putStrLn (formatPrintResult optOutputFormat (AgentFailed err))
            else TIO.putStrLn err
          exitFailure
        Just (Right condition) -> do
          when (not optPrint) $
            putStrLn ("\nStarting goal-directed agent loop for condition: " <> T.unpack condition)
          let agentConfig = AgentConfig
                { cfgModel        = envModel
                , cfgSystemPrompt = Just sysPrompt
                , cfgMaxTurns     = optMaxTurns
                , cfgMaxBudgetUsd = maxBudgetUsd
                }
              initialHistory = buildSessionHistory sysPrompt mLoadedHistory condition
          (result, finalHistory, goalState, spentUsd) <-
            runIO ioEnv (goalLoop agentConfig allToolDefs condition defaultBlockCap initialHistory)
          saveRunSession currentWorkspace activeSid envModel mPrevInfo spentUsd finalHistory

          if optPrint
            then TIO.putStrLn (formatPrintGoalResult optOutputFormat goalState result)
            else do
              reportHeadlessOutcome "Task completed." finalHistory (runOutcome result (Just goalState))
              printGoalSummary goalState

          exitOnHeadlessFailureWith (Just goalState) result

        Nothing -> do
          let trimmedPrompt = T.strip taskPrompt
          finalPrompt <- expandSlashInvokedPrompt currentWorkspace skills trimmedPrompt
          let agentConfig = AgentConfig
                { cfgModel        = envModel
                , cfgSystemPrompt = Just sysPrompt
                , cfgMaxTurns     = optMaxTurns
                , cfgMaxBudgetUsd = maxBudgetUsd
                }
              initialHistory = buildSessionHistory sysPrompt mLoadedHistory finalPrompt

          when (not optPrint) $
            putStrLn ("\nStarting agent loop for task: " <> T.unpack taskPrompt)
          (result, finalHistory, spentUsd) <- runIO ioEnv (agentLoop agentConfig allToolDefs initialHistory)
          saveRunSession currentWorkspace activeSid envModel mPrevInfo spentUsd finalHistory

          if optPrint
            then TIO.putStrLn (formatPrintResult optOutputFormat result)
            else reportHeadlessOutcome "Task successfully completed!" finalHistory (runOutcome result Nothing)

          exitOnHeadlessFailure result

-- | Print how a headless (non-'--print') run ended, from its classified
-- outcome.
reportHeadlessOutcome :: String -> [Message] -> RunOutcome -> IO ()
reportHeadlessOutcome successBanner finalHistory outcome = case outcome of
  RunSucceeded _ -> do
    putStrLn ("\n" <> successBanner)
    putStrLn ("Total dialogue messages in history: " <> show (length finalHistory))
  RunFailed (AgentError err) ->
    putStrLn ("\nAgent failed with error: " <> T.unpack err)
  _ ->
    putStrLn ("\n" <> T.unpack (renderRunOutcome outcome))

-- | Headless failures must be visible to shell callers through the process
-- status, after the result has been rendered in the requested format.
exitOnHeadlessFailure :: AgentResult -> IO ()
exitOnHeadlessFailure = exitOnHeadlessFailureWith Nothing

-- | Headless failure exit taking optional goal state into account.
exitOnHeadlessFailureWith :: Maybe GoalState -> AgentResult -> IO ()
exitOnHeadlessFailureWith mGs result =
  case resolveHeadlessExitCode result mGs of
    ExitSuccess      -> pure ()
    ExitFailure code -> exitWith (ExitFailure code)

-- | Resolve the workspace selected by the CLI before any task, command, or TUI
-- work begins. The process remains rooted at the repository checkout so the
-- interpreter can still create sibling worktrees and exit back to that root.
resolveStartupWorkspace :: FilePath -> Maybe T.Text -> IO FilePath
resolveStartupWorkspace cwd Nothing = pure cwd
resolveStartupWorkspace cwd (Just name) = do
  result <- Git.createWorktree cwd name
  case result of
    Left err -> do
      putStrLn ("Worktree error: " <> T.unpack err)
      exitFailure
    Right workspace -> pure workspace

-- | Print a summary of the goal state after a headless goal run.
printGoalSummary :: GoalState -> IO ()
printGoalSummary gs = do
  putStrLn "\n--- Goal Summary ---"
  TIO.putStrLn ("  Condition: " <> gsCondition gs)
  putStrLn ("  Status:    " <> show (gsStatus gs))
  putStrLn ("  Turns:     " <> show (gsTurnCount gs))
  case gsLastReason gs of
    Just r  -> TIO.putStrLn ("  Reason:    " <> r)
    Nothing -> pure ()
  case gsLastVerdict gs of
    Just v  -> putStrLn ("  Verdict:   " <> show v)
    Nothing -> pure ()
