{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Core
import Agent.Env
import Agent.Interpreter.IO
import Agent.Tools
import Agent.Types
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)

defaultSystemPrompt :: T.Text
defaultSystemPrompt =
  "You are an expert autonomous coding assistant. You have access to tools " <>
  "to inspect files, write code, run shell commands, and explore the workspace. " <>
  "Always inspect existing code before making changes, verify your work by running commands, " <>
  "and provide a concise final summary when complete."

main :: IO ()
main = do
  args <- getArgs
  cwd  <- getCurrentDirectory

  envRes <- loadEnvConfig ".env"
  EnvConfig{..} <- case envRes of
    Left err -> do
      putStrLn ("Configuration error: " <> err)
      putStrLn "Please ensure .env contains OPENROUTER_API_KEY and line 2 specifies the model."
      exitFailure
    Right cfg -> pure cfg

  putStrLn "========================================================"
  putStrLn "  Haskell Agentic Coding Harness (Functional Pearl)     "
  putStrLn "========================================================"
  putStrLn ("Workspace: " <> cwd)
  putStrLn ("Model:     " <> T.unpack envModel <> " (loaded from line 2 of .env)")
  putStrLn "========================================================"

  taskPrompt <- case args of
    [] -> do
      putStrLn "Enter your task/request:"
      TIO.getLine
    _ -> pure (T.pack (unwords args))

  when (T.null (T.strip taskPrompt)) $ do
    putStrLn "Empty task prompt provided. Exiting."
    exitFailure

  ioEnv <- newIOEnv envApiKey envModel cwd True

  let agentConfig = AgentConfig
        { cfgModel        = envModel
        , cfgSystemPrompt = Just defaultSystemPrompt
        , cfgMaxTurns     = 10
        }
      initialHistory =
        [ SystemMsg defaultSystemPrompt
        , UserMsg taskPrompt
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
