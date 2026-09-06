{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Agent.Core
import Agent.Env
import Agent.Interpreter.IO
import Agent.Skills (discoverSkills, injectSkillsIntoPrompt, parseSkillInvocations)
import Agent.Tools
import Agent.TUI.App (runTui)
import Agent.Types
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  rawArgs <- getArgs
  cwd     <- getCurrentDirectory

  CliOptions{..} <- case parseCliArgs rawArgs of
    Left err -> do
      putStrLn ("Argument error: " <> err)
      putStrLn "Usage: agent-harness [--model <model_name>] [--no-tui] [task prompt...]"
      exitFailure
    Right opts -> pure opts

  envRes <- resolveEnvConfig optModel (Just ".env")
  EnvConfig{..} <- case envRes of
    Left err -> do
      putStrLn ("Configuration error: " <> err)
      putStrLn "Please set OPENROUTER_API_KEY in the environment or in .env."
      exitFailure
    Right cfg -> pure cfg

  ioEnv <- newIOEnv envApiKey envModel cwd True

  if not optNoTui
    then runTui ioEnv optPrompt
    else do
      putStrLn "========================================================"
      putStrLn "  Haskell Agentic Coding Harness (Functional Pearl)     "
      putStrLn "========================================================"
      putStrLn ("Workspace: " <> cwd)
      putStrLn ("Model:     " <> T.unpack envModel)
      putStrLn "========================================================"

      taskPrompt <- case optPrompt of
        Just p  -> pure p
        Nothing -> do
          putStrLn "Enter your task/request:"
          TIO.getLine

      when (T.null (T.strip taskPrompt)) $ do
        putStrLn "Empty task prompt provided. Exiting."
        exitFailure

      skills <- discoverSkills cwd
      mGuidelines <- loadProjectInstructions cwd
      let sysPrompt = buildSystemPrompt mGuidelines

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
                putStrLn ("\nStarting goal-directed agent loop for condition: " <> T.unpack condition)
                let agentConfig = AgentConfig
                      { cfgModel        = envModel
                      , cfgSystemPrompt = Just sysPrompt
                      , cfgMaxTurns     = 20
                      }
                    initialHistory =
                      [ SystemMsg sysPrompt
                      , UserMsg condition
                      ]
                (result, finalHistory, goalState) <-
                  runIO ioEnv (goalLoop agentConfig allToolDefs condition defaultBlockCap initialHistory)

                case result of
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
          let (cleaned, invoked) = parseSkillInvocations skills trimmedPrompt
              finalPrompt = injectSkillsIntoPrompt invoked cleaned
              agentConfig = AgentConfig
                { cfgModel        = envModel
                , cfgSystemPrompt = Just sysPrompt
                , cfgMaxTurns     = 10
                }
              initialHistory =
                [ SystemMsg sysPrompt
                , UserMsg finalPrompt
                ]

          putStrLn ("\nStarting agent loop for task: " <> T.unpack taskPrompt)
          (result, finalHistory) <- runIO ioEnv (agentLoop agentConfig allToolDefs initialHistory)

          case result of
            AgentCompleted _ans -> do
              putStrLn "\nTask successfully completed!"
              putStrLn ("Total dialogue messages in history: " <> show (length finalHistory))
            AgentMaxTurnsReached turns -> do
              putStrLn ("\nAgent reached maximum turn limit of " <> show turns <> ".")
            AgentFailed err -> do
              putStrLn ("\nAgent failed with error: " <> T.unpack err)

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
