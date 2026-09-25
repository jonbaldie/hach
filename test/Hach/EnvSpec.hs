{-# LANGUAGE OverloadedStrings #-}

module Hach.EnvSpec (spec) where

import Hach.Env
import Hach.Settings (Settings(..), defaultSettings)
import Hach.Types (EffortLevel(..), parseEffortLevel)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "resolveEffortLevel" $ do
    it "leaves unset effort as Nothing" $ do
      resolveEffortLevel defaultSettings `shouldBe` Right Nothing

    it "accepts a supported configured effort" $ do
      resolveEffortLevel defaultSettings { setEffortLevel = Just "high" }
        `shouldBe` Right (Just EffortHigh)

    it "normalizes case and surrounding whitespace" $ do
      resolveEffortLevel defaultSettings { setEffortLevel = Just "  HIGH  " }
        `shouldBe` Right (Just EffortHigh)

    it "rejects unsupported values with a clear error" $ do
      case resolveEffortLevel defaultSettings { setEffortLevel = Just "turbo" } of
        Left err -> do
          err `shouldContain` "Unsupported effort_level: turbo"
          err `shouldContain` "high"
        Right v -> expectationFailure ("expected Left, got " <> show v)

    it "rejects empty effort_level" $ do
      case parseEffortLevel "   " of
        Left err -> err `shouldContain` "Unsupported effort_level"
        Right v  -> expectationFailure ("expected Left, got " <> show v)

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

  describe "resolveMaxBudgetUsd" $ do
    it "prefers the CLI flag over settings" $ do
      resolveMaxBudgetUsd (Just 1.5) defaultSettings { setMaxBudgetUsd = Just 9 }
        `shouldBe` Just 1.5

    it "uses settings when the flag is absent" $ do
      resolveMaxBudgetUsd Nothing defaultSettings { setMaxBudgetUsd = Just 0 }
        `shouldBe` Just 0

    it "ignores negative settings values" $ do
      resolveMaxBudgetUsd Nothing defaultSettings { setMaxBudgetUsd = Just (-1) }
        `shouldBe` Nothing

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

      it "loads AGENTS.md when it is the only instructions file" $ do
        TIO.writeFile (testSandbox </> "AGENTS.md") "Agents rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Agents rules"

      it "prefers AGENTS.md over AGENT.md and CLAUDE.md when all exist" $ do
        TIO.writeFile (testSandbox </> "AGENTS.md") "Agents rules"
        TIO.writeFile (testSandbox </> "AGENT.md") "Agent rules"
        TIO.writeFile (testSandbox </> "CLAUDE.md") "Claude rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Agents rules"
