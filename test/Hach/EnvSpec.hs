{-# LANGUAGE OverloadedStrings #-}

module Hach.EnvSpec (spec) where

import Hach.Env
import Hach.Settings (defaultSettings)
import Hach.Types (PermissionMode(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "parseEnvContent" $ do
    it "parses standard key-value pairs" $ do
      let content = "KEY1=value1\nKEY2=value2\n"
          res = parseEnvContent content
      Map.lookup "KEY1" res `shouldBe` Just "value1"
      Map.lookup "KEY2" res `shouldBe` Just "value2"

    it "ignores comments and empty lines" $ do
      let content = "# comment\n\nKEY1=value1\n   # another comment\nKEY2=value2\n"
          res = parseEnvContent content
      Map.lookup "KEY1" res `shouldBe` Just "value1"
      Map.lookup "KEY2" res `shouldBe` Just "value2"
      Map.size res `shouldBe` 2

    it "strips quotes from values" $ do
      let content = "FOO=\"quoted value\"\nBAR='single quoted'\n"
          res = parseEnvContent content
      Map.lookup "FOO" res `shouldBe` Just "quoted value"
      Map.lookup "BAR" res `shouldBe` Just "single quoted"

    it "strips export prefix from keys" $ do
      let content = "export OPENROUTER_API_KEY=sk-secret\nexport OPENROUTER_MODEL=meta/muse-glimmer-30b\n"
          res = parseEnvContent content
      Map.lookup "OPENROUTER_API_KEY" res `shouldBe` Just "sk-secret"
      Map.lookup "OPENROUTER_MODEL" res `shouldBe` Just "meta/muse-glimmer-30b"

    it "handles mixed export and non-export lines" $ do
      let content = "export KEY1=val1\nKEY2=val2\n"
          res = parseEnvContent content
      Map.lookup "KEY1" res `shouldBe` Just "val1"
      Map.lookup "KEY2" res `shouldBe` Just "val2"

  describe "parseLineTwoModel" $ do
    it "extracts the model from line two of .env" $ do
      let ls = [ "OPENROUTER_API_KEY=sk-test"
               , "OPENROUTER_MODEL=meta/muse-glimmer-30b"
               ]
      parseLineTwoModel ls `shouldBe` Just "meta/muse-glimmer-30b"

    it "handles whitespace and quotes on line two" $ do
      let ls = [ "OPENROUTER_API_KEY=sk-test"
               , "  OPENROUTER_MODEL=\"anthropic/claude-3\"  "
               ]
      parseLineTwoModel ls `shouldBe` Just "anthropic/claude-3"

    it "handles bare model string on line two" $ do
      let ls = [ "OPENROUTER_API_KEY=sk-test"
               , "openai/gpt-4o"
               ]
      parseLineTwoModel ls `shouldBe` Just "openai/gpt-4o"

    it "returns Nothing if fewer than two lines" $ do
      parseLineTwoModel ["OPENROUTER_API_KEY=sk-test"] `shouldBe` Nothing

  describe "parseCliArgs" $ do
    it "parses --model with separate argument" $ do
      let args = ["--model", "meta/llama-3", "do", "something"]
      parseCliArgs args `shouldBe` Right defaultCliOptions
        { optModel = Just "meta/llama-3"
        , optPrompt = Just "do something"
        , optNoTui = False
        }

    it "parses --model= syntax" $ do
      let args = ["--model=anthropic/claude-3", "run", "all", "tests"]
      parseCliArgs args `shouldBe` Right defaultCliOptions
        { optModel = Just "anthropic/claude-3"
        , optPrompt = Just "run all tests"
        , optNoTui = False
        }

    it "parses short flag -m" $ do
      let args = ["-m", "openai/gpt-4o", "hello"]
      parseCliArgs args `shouldBe` Right defaultCliOptions
        { optModel = Just "openai/gpt-4o"
        , optPrompt = Just "hello"
        , optNoTui = False
        }

    it "parses flag positioned between prompt words" $ do
      let args = ["hello", "--model", "meta/muse-glimmer-30b", "world"]
      parseCliArgs args `shouldBe` Right defaultCliOptions
        { optModel = Just "meta/muse-glimmer-30b"
        , optPrompt = Just "hello world"
        , optNoTui = False
        }

    it "parses --no-tui flag" $ do
      let args = ["--no-tui", "echo", "hello"]
      parseCliArgs args `shouldBe` Right defaultCliOptions
        { optModel = Nothing
        , optPrompt = Just "echo hello"
        , optNoTui = True
        }

    it "parses arguments when no model flag is provided" $ do
      let args = ["run", "my", "task"]
      parseCliArgs args `shouldBe` Right defaultCliOptions
        { optModel = Nothing
        , optPrompt = Just "run my task"
        , optNoTui = False
        }

    it "parses empty arguments" $ do
      parseCliArgs [] `shouldBe` Right defaultCliOptions
        { optModel = Nothing
        , optPrompt = Nothing
        , optNoTui = False
        }

    it "treats whitespace-only arguments as Nothing for optPrompt" $ do
      parseCliArgs ["", "   "] `shouldBe` Right defaultCliOptions
        { optModel = Nothing
        , optPrompt = Nothing
        , optNoTui = False
        }

    it "fails when --model has no argument" $ do
      case parseCliArgs ["--model"] of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected parseCliArgs to fail when --model has no argument"

    it "fails when --model= is empty" $ do
      case parseCliArgs ["--model="] of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected parseCliArgs to fail when --model= is empty"

    it "rejects flag-like values as --model argument" $ do
      case parseCliArgs ["--model", "--no-tui"] of
        Left _  -> pure ()
        Right _ -> expectationFailure "Expected parseCliArgs to reject --no-tui as model value"

    it "rejects flag-like values as -m argument" $ do
      case parseCliArgs ["-m", "--verbose"] of
        Left _  -> pure ()
        Right _ -> expectationFailure "Expected parseCliArgs to reject --verbose as model value"

    it "parses --print and -p flags" $ do
      parseCliArgs ["--print", "hi"] `shouldBe` Right defaultCliOptions
        { optPrint = True
        , optNoTui = True
        , optPrompt = Just "hi"
        }
      parseCliArgs ["-p", "hi"] `shouldBe` Right defaultCliOptions
        { optPrint = True
        , optNoTui = True
        , optPrompt = Just "hi"
        }

    it "parses --output-format json and text" $ do
      parseCliArgs ["--output-format", "json"] `shouldBe` Right defaultCliOptions
        { optOutputFormat = OutputJson }
      parseCliArgs ["--output-format=text"] `shouldBe` Right defaultCliOptions
        { optOutputFormat = OutputText }

    it "parses --continue / -c and --resume / -r" $ do
      parseCliArgs ["--continue"] `shouldBe` Right defaultCliOptions { optContinue = True }
      parseCliArgs ["-c"] `shouldBe` Right defaultCliOptions { optContinue = True }
      parseCliArgs ["--resume"] `shouldBe` Right defaultCliOptions { optResume = True }
      parseCliArgs ["-r"] `shouldBe` Right defaultCliOptions { optResume = True }

    it "parses --session-id" $ do
      parseCliArgs ["--session-id", "sess-123"] `shouldBe` Right defaultCliOptions
        { optSessionId = Just "sess-123" }

    it "parses --max-turns and --max-budget-usd" $ do
      parseCliArgs ["--max-turns", "50", "--max-budget-usd", "5.25"] `shouldBe` Right defaultCliOptions
        { optMaxTurns = Just 50
        , optMaxBudgetUsd = Just 5.25
        }

    it "parses --worktree / -w" $ do
      parseCliArgs ["--worktree", "feat-1"] `shouldBe` Right defaultCliOptions
        { optWorktree = Just "feat-1" }
      parseCliArgs ["-w", "feat-2"] `shouldBe` Right defaultCliOptions
        { optWorktree = Just "feat-2" }

    it "parses --permission-mode" $ do
      parseCliArgs ["--permission-mode", "acceptEdits"] `shouldBe` Right defaultCliOptions
        { optPermissionMode = Just ModeAcceptEdits }
      parseCliArgs ["--permission-mode=plan"] `shouldBe` Right defaultCliOptions
        { optPermissionMode = Just ModePlan }

    it "parses --dangerously-skip-permissions" $ do
      parseCliArgs ["--dangerously-skip-permissions"] `shouldBe` Right defaultCliOptions
        { optDangerouslySkipPerms = True }

    it "parses --append-system-prompt flag with separate argument" $ do
      let args = ["--append-system-prompt", "Always reply in uppercase", "hello"]
      parseCliArgs args `shouldBe` Right defaultCliOptions
        { optAppendSystemPrompt = Just "Always reply in uppercase"
        , optPrompt = Just "hello"
        , optNoTui = False
        }

    it "parses --append-system-prompt= syntax" $ do
      let args = ["--append-system-prompt=Always reply in uppercase", "hello"]
      parseCliArgs args `shouldBe` Right defaultCliOptions
        { optAppendSystemPrompt = Just "Always reply in uppercase"
        , optPrompt = Just "hello"
        , optNoTui = False
        }

    it "fails when --append-system-prompt has no argument" $ do
      case parseCliArgs ["--append-system-prompt"] of
        Left err -> err `shouldContain` "requires an argument"
        Right _  -> expectationFailure "Expected failure when --append-system-prompt has no argument"

    it "fails on unknown flag" $ do
      case parseCliArgs ["--some-bogus-flag"] of
        Left err -> err `shouldContain` "Unknown flag"
        Right _  -> expectationFailure "Expected failure on unknown flag"

  describe "resolveConfigWith" $ do
    let dotEnvSample = "OPENROUTER_API_KEY=sk-dotenv\nOPENROUTER_MODEL=meta/muse-glimmer-30b\n"

    it "CLI flag overrides both OS env and .env for model" $ do
      let res = resolveConfigWith
                  (Just "custom/cli-model")
                  (Just "sk-os-env")
                  (Just "os-model")
                  (Just dotEnvSample)
      res `shouldBe` Right EnvConfig
        { envApiKey = "sk-os-env"
        , envModel = "custom/cli-model"
        , envSettings = defaultSettings
        }

    it "uses OS environment API key in preference to .env" $ do
      let res = resolveConfigWith
                  Nothing
                  (Just "sk-os-env")
                  Nothing
                  (Just dotEnvSample)
      res `shouldBe` Right EnvConfig
        { envApiKey = "sk-os-env"
        , envModel = "meta/muse-glimmer-30b"
        , envSettings = defaultSettings
        }

    it "falls back to .env when OS environment variables are missing" $ do
      let res = resolveConfigWith
                  Nothing
                  Nothing
                  Nothing
                  (Just dotEnvSample)
      res `shouldBe` Right EnvConfig
        { envApiKey = "sk-dotenv"
        , envModel = "meta/muse-glimmer-30b"
        , envSettings = defaultSettings
        }

    it "resolves config from .env with export-prefixed lines" $ do
      let dotEnvExport = "export OPENROUTER_API_KEY=sk-dotenv\nexport OPENROUTER_MODEL=meta/muse-glimmer-30b\n"
          res = resolveConfigWith
                  Nothing
                  Nothing
                  Nothing
                  (Just dotEnvExport)
      res `shouldBe` Right EnvConfig
        { envApiKey = "sk-dotenv"
        , envModel = "meta/muse-glimmer-30b"
        , envSettings = defaultSettings
        }

    it "uses OS environment model when no CLI flag given and .env missing" $ do
      let res = resolveConfigWith
                  Nothing
                  (Just "sk-os-env")
                  (Just "os-model")
                  Nothing
      res `shouldBe` Right EnvConfig
        { envApiKey = "sk-os-env"
        , envModel = "os-model"
        , envSettings = defaultSettings
        }

    it "fails if API key is not in OS env or .env" $ do
      let res = resolveConfigWith
                  Nothing
                  Nothing
                  (Just "os-model")
                  Nothing
      case res of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected error when API key is missing"

  describe "buildSystemPrompt" $ do
    it "returns default prompt when no project instructions exist" $ do
      let prompt = buildSystemPrompt Nothing
      prompt `shouldSatisfy` ("expert autonomous coding assistant" `T.isInfixOf`)

    it "appends project guidelines when provided" $ do
      let guidelines = "Follow functional pearl style."
          prompt = buildSystemPrompt (Just guidelines)
      prompt `shouldSatisfy` ("# Project Guidelines:" `T.isInfixOf`)
      prompt `shouldSatisfy` ("Follow functional pearl style." `T.isInfixOf`)

  describe "buildSystemPromptWithAppend" $ do
    it "returns base prompt when guidelines and append are Nothing" $ do
      let prompt = buildSystemPromptWithAppend Nothing Nothing
      prompt `shouldBe` buildSystemPrompt Nothing

    it "appends custom instructions when provided without project guidelines" $ do
      let prompt = buildSystemPromptWithAppend Nothing (Just "Always reply in uppercase")
      prompt `shouldSatisfy` ("expert autonomous coding assistant" `T.isInfixOf`)
      prompt `shouldSatisfy` ("Always reply in uppercase" `T.isInfixOf`)

    it "combines project guidelines and custom appended instructions" $ do
      let guidelines = "Follow functional pearl style."
          prompt = buildSystemPromptWithAppend (Just guidelines) (Just "Always reply in uppercase")
      prompt `shouldSatisfy` ("# Project Guidelines:" `T.isInfixOf`)
      prompt `shouldSatisfy` ("Follow functional pearl style." `T.isInfixOf`)
      prompt `shouldSatisfy` ("Always reply in uppercase" `T.isInfixOf`)

    it "ignores Nothing or whitespace-only appended instructions" $ do
      let promptEmpty = buildSystemPromptWithAppend Nothing (Just "   ")
          promptNothing = buildSystemPromptWithAppend Nothing Nothing
      promptEmpty `shouldBe` buildSystemPrompt Nothing
      promptNothing `shouldBe` buildSystemPrompt Nothing

  describe "loadProjectInstructions" $ do
    let testSandbox = "dist-newstyle/test-sandbox-env"
    around_ (\action -> do
      createDirectoryIfMissing True testSandbox
      action
      removeDirectoryRecursive testSandbox) $ do
      it "returns Nothing when neither AGENT.md nor CLAUDE.md exists" $ do
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Nothing

      it "loads AGENT.md when it exists" $ do
        TIO.writeFile (testSandbox </> "AGENT.md") "Agent rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Agent rules"

      it "loads CLAUDE.md when AGENT.md does not exist" $ do
        TIO.writeFile (testSandbox </> "CLAUDE.md") "Claude rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Claude rules"

      it "prefers AGENT.md over CLAUDE.md when both exist" $ do
        TIO.writeFile (testSandbox </> "AGENT.md") "Agent rules"
        TIO.writeFile (testSandbox </> "CLAUDE.md") "Claude rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Agent rules"
