{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Hach.Core
import Hach.Env
import qualified Hach.Git as Git
import Hach.Interpreter.IO
import Hach.Skills (discoverSkills, expandSlashInvokedPrompt)
import Hach.Settings (Settings (..))
import Hach.Tools
import Hach.TUI.App (runTui)
import Hach.Types
import Control.Exception (tryJust)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import Data.Version (showVersion)
import qualified Paths_hach as Paths
import System.Exit (exitFailure, exitSuccess, exitWith)
import System.IO (stderr)
import System.IO.Error (isEOFError)

main :: IO ()
main = do
  rawArgs <- getArgs
  cwd     <- getCurrentDirectory

  opts@CliOptions{..} <- case parseCliArgs rawArgs of
    Left err -> do
      putStrLn ("Argument error: " <> err)
      putStrLn "Usage: hach [--model <model_name>] [--no-tui] [--exec <cmd>] [task prompt...]"
      exitFailure
    Right parsed -> pure parsed

  let intent = startupIntent opts
  case intent of
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
    IntentTui -> pure ()
    IntentHeadless -> pure ()
    IntentVersion -> pure ()

  envRes <- resolveEnvConfig optModel (Just ".env")
  EnvConfig{..} <- case envRes of
    Left err -> do
      putStrLn ("Configuration error: " <> err)
      putStrLn "Please set OPENROUTER_API_KEY in the environment or in .env."
      exitFailure
    Right cfg -> pure cfg

  effort <- case resolveEffortLevel envSettings of
    Left err -> do
      putStrLn ("Configuration error: " <> err)
      exitFailure
    Right e -> pure e

  let perms = defaultIOEnvPermissions
        { iopInitialMode = resolvePermissionMode optPermissionMode optDangerouslySkipPerms envSettings
        , iopRules       = setPermissionRules envSettings
        , iopHooks       = setHooks envSettings
        }
  ioEnv0 <- newIOEnvWithPermissions perms envApiKey envModel cwd (headlessVerbose opts)
  let ioEnv = ioEnv0 { ioEffortLevel = effort }
  case optWorktree of
    Nothing -> pure ()
    Just _  -> interpEnterWorktree (ioAlgebra ioEnv) activeWorkspace

  case intent of
    IntentTui -> runTui ioEnv optPrompt optMaxTurns optAppendSystemPrompt
    _ -> do
      currentWorkspace <- currentIOWorkspace ioEnv
      when (headlessEmitsBanners opts) $ do
        putStrLn "========================================================"
        putStrLn "  Haskell Agentic Coding Harness (hach)                 "
        putStrLn "========================================================"
        putStrLn ("Workspace: " <> currentWorkspace)
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
        putStrLn "Empty task prompt provided. Exiting."
        exitFailure

      skills <- discoverSkills currentWorkspace
      mGuidelines <- loadProjectInstructions currentWorkspace
      let sysPrompt = buildSystemPromptWithAppend mGuidelines optAppendSystemPrompt

      let trimmedPrompt = T.strip taskPrompt
          isGoalCommand = trimmedPrompt == "/goal" || T.isPrefixOf "/goal " trimmedPrompt
      if isGoalCommand
        then do
          let argText = if trimmedPrompt == "/goal"
                         then ""
                         else T.strip (T.drop (T.length ("/goal " :: T.Text)) trimmedPrompt)
          if T.null argText
            then do
              putStrLn "Usage: /goal <condition> or /goal clear"
              putStrLn "Example: /goal all tests pass"
            else if goalArgIsClear argText
              then putStrLn "No active goal to clear (headless mode has no persistent goal state)."
            else if T.length argText > maxGoalConditionLength
              then do
                putStrLn ("Goal condition too long (max " <> show maxGoalConditionLength <> " characters).")
              else do
                let condition = argText
                when (not optPrint) $
                  putStrLn ("\nStarting goal-directed agent loop for condition: " <> T.unpack condition)
                let agentConfig = AgentConfig
                      { cfgModel        = envModel
                      , cfgSystemPrompt = Just sysPrompt
                      , cfgMaxTurns     = optMaxTurns
                      }
                    initialHistory =
                      [ SystemMsg sysPrompt
                      , UserMsg condition
                      ]
                (result, finalHistory, goalState) <-
                  runIO ioEnv (goalLoop agentConfig allToolDefs condition defaultBlockCap initialHistory)

                if optPrint
                  then TIO.putStrLn (formatPrintResult optOutputFormat result)
                  else case result of
                    AgentCompleted _ans -> do
                      putStrLn "\nTask completed."
                      putStrLn ("Total dialogue messages in history: " <> show (length finalHistory))
                      printGoalSummary goalState
                    AgentMaxTurnsReached turns -> do
                      putStrLn ("\nAgent reached maximum turn limit of " <> show turns <> ".")
                      printGoalSummary goalState
                    AgentFailed err -> do
                      putStrLn ("\nAgent failed with error: " <> T.unpack err)
                      printGoalSummary goalState

        else do
          finalPrompt <- expandSlashInvokedPrompt currentWorkspace skills trimmedPrompt
          let agentConfig = AgentConfig
                { cfgModel        = envModel
                , cfgSystemPrompt = Just sysPrompt
                , cfgMaxTurns     = optMaxTurns
                }
              initialHistory =
                [ SystemMsg sysPrompt
                , UserMsg finalPrompt
                ]

          when (not optPrint) $
            putStrLn ("\nStarting agent loop for task: " <> T.unpack taskPrompt)
          (result, finalHistory) <- runIO ioEnv (agentLoop agentConfig allToolDefs initialHistory)

          if optPrint
            then TIO.putStrLn (formatPrintResult optOutputFormat result)
            else case result of
              AgentCompleted _ans -> do
                putStrLn "\nTask successfully completed!"
                putStrLn ("Total dialogue messages in history: " <> show (length finalHistory))
              AgentMaxTurnsReached turns -> do
                putStrLn ("\nAgent reached maximum turn limit of " <> show turns <> ".")
              AgentFailed err -> do
                putStrLn ("\nAgent failed with error: " <> T.unpack err)

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
