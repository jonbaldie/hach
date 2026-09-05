{-# LANGUAGE OverloadedStrings #-}

module Agent.EnvSpec (spec) where

import Agent.Env
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
      parseCliArgs args `shouldBe` Right CliOptions
        { optModel = Just "meta/llama-3"
        , optPrompt = Just "do something"
        , optNoTui = False
        }

    it "parses --model= syntax" $ do
      let args = ["--model=anthropic/claude-3", "run", "all", "tests"]
      parseCliArgs args `shouldBe` Right CliOptions
        { optModel = Just "anthropic/claude-3"
        , optPrompt = Just "run all tests"
        , optNoTui = False
        }

    it "parses short flag -m" $ do
      let args = ["-m", "openai/gpt-4o", "hello"]
      parseCliArgs args `shouldBe` Right CliOptions
        { optModel = Just "openai/gpt-4o"
        , optPrompt = Just "hello"
        , optNoTui = False
        }

    it "parses flag positioned between prompt words" $ do
      let args = ["hello", "--model", "meta/muse-glimmer-30b", "world"]
      parseCliArgs args `shouldBe` Right CliOptions
        { optModel = Just "meta/muse-glimmer-30b"
        , optPrompt = Just "hello world"
        , optNoTui = False
        }

    it "parses --no-tui flag" $ do
      let args = ["--no-tui", "echo", "hello"]
      parseCliArgs args `shouldBe` Right CliOptions
        { optModel = Nothing
        , optPrompt = Just "echo hello"
        , optNoTui = True
        }

    it "parses arguments when no model flag is provided" $ do
      let args = ["run", "my", "task"]
      parseCliArgs args `shouldBe` Right CliOptions
        { optModel = Nothing
        , optPrompt = Just "run my task"
        , optNoTui = False
        }

    it "parses empty arguments" $ do
      parseCliArgs [] `shouldBe` Right CliOptions
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
