{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Hach.Core
import Hach.Env
import Hach.Interpreter.IO
import Hach.Tools
import Hach.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

main :: IO ()
main = do
  putStrLn "=== Running Live OpenRouter Integration Test ==="

  -- 1. Load .env config and verify model from line 2
  envRes <- loadEnvConfig ".env"
  EnvConfig{..} <- case envRes of
    Left err -> do
      putStrLn ("FAIL: Could not load .env: " <> err)
      exitFailure
    Right cfg -> pure cfg

  putStrLn ("Using model from line 2 of .env: " <> T.unpack envModel)

  -- 2. Setup isolated sandbox workspace for test
  cwd <- getCurrentDirectory
  let sandboxDir = cwd </> ".test-sandbox"
  createDirectoryIfMissing True sandboxDir

  -- The integration test exercises the autonomous write path, so it opts
  -- out of permission enforcement explicitly.
  let perms = defaultIOEnvPermissions { iopInitialMode = ModeBypassPermissions }
  ioEnv <- newIOEnvWithPermissions perms envApiKey envModel sandboxDir True

  let prompt =
        "Please use the write_file tool to write 'Hello from Haskell Pearl' into a file named 'live_test.txt'. " <>
        "Then use the read_file tool to read 'live_test.txt'. " <>
        "Finally, answer with the file contents."

  let agentConfig = AgentConfig
        { cfgModel = envModel
        , cfgSystemPrompt = Just "You are an autonomous coding assistant. Use the provided tools to complete user requests."
        , cfgMaxTurns = Just 6
        }
      initMsgs = [UserMsg prompt]

  putStrLn "\n--- Launching Agent Loop ---"
  (result, _history) <- runIO ioEnv (agentLoop agentConfig allToolDefs initMsgs)

  putStrLn "\n--- Agent Loop Concluded ---"
  case result of
    AgentCompleted finalAns -> do
      putStrLn ("Agent completed with answer: " <> T.unpack finalAns)
      -- Check that file was actually written to the sandbox
      let createdFile = sandboxDir </> "live_test.txt"
      fileCreated <- doesFileExist createdFile
      if fileCreated
        then do
          content <- TIO.readFile createdFile
          putStrLn ("Sandbox file content: " <> T.unpack content)
          putStrLn "SUCCESS: Live integration test passed!"
          -- Cleanup sandbox
          removeDirectoryRecursive sandboxDir
          exitSuccess
        else do
          putStrLn "FAIL: live_test.txt was not created on disk."
          removeDirectoryRecursive sandboxDir
          exitFailure

    AgentMaxTurnsReached n -> do
      putStrLn ("FAIL: Max turns reached (" <> show n <> ")")
      removeDirectoryRecursive sandboxDir
      exitFailure

    AgentFailed err -> do
      putStrLn ("FAIL: Agent failed with: " <> T.unpack err)
      removeDirectoryRecursive sandboxDir
      exitFailure
