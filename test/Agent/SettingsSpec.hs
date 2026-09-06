{-# LANGUAGE OverloadedStrings #-}

module Agent.SettingsSpec (spec) where

import Agent.Settings
import Agent.Types
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = describe "Agent.Settings" $ do
  describe "JSON deserialization" $ do
    it "parses complete settings object" $ do
      let raw = "{\n\
        \  \"model\": \"claude-3-opus\",\n\
        \  \"fallback_model\": \"claude-3-sonnet\",\n\
        \  \"effort_level\": \"high\",\n\
        \  \"max_budget_usd\": 10.5,\n\
        \  \"permission_mode\": \"acceptEdits\",\n\
        \  \"theme\": \"nord\",\n\
        \  \"env_allowlist\": [\"PATH\", \"HOME\"],\n\
        \  \"working_directories\": [\"/tmp\", \"/var\"],\n\
        \  \"output_style\": \"concise\"\n\
        \}"
      case decode (BSL.fromStrict raw) of
        Nothing -> expectationFailure "Failed to parse Settings JSON"
        Just s -> do
          setModel s `shouldBe` Just "claude-3-opus"
          setFallbackModel s `shouldBe` Just "claude-3-sonnet"
          setEffortLevel s `shouldBe` Just "high"
          setMaxBudgetUsd s `shouldBe` Just 10.5
          setPermissionMode s `shouldBe` Just ModeAcceptEdits
          setTheme s `shouldBe` Just "nord"
          setEnvAllowlist s `shouldBe` ["PATH", "HOME"]
          setWorkingDirs s `shouldBe` ["/tmp", "/var"]
          setOutputStyle s `shouldBe` Just StyleConcise

  describe "Layered merging" $ do
    it "overrides earlier layers with later layers" $ do
      let userSettings = defaultSettings
            { setModel = Just "user-model"
            , setTheme = Just "dark"
            , setWorkingDirs = ["/user/dir"]
            }
          projectSettings = defaultSettings
            { setModel = Just "project-model"
            , setPermissionMode = Just ModeAcceptEdits
            }
          localSettings = defaultSettings
            { setModel = Just "local-model"
            }
          merged = mergeSettings (mergeSettings userSettings projectSettings) localSettings
      setModel merged `shouldBe` Just "local-model"
      setTheme merged `shouldBe` Just "dark"
      setPermissionMode merged `shouldBe` Just ModeAcceptEdits

    it "unions keybindings preferring later layer" $ do
      let base = defaultSettings { setKeybindings = Map.fromList [("ctrl-c", "cancel"), ("ctrl-r", "search")] }
          over = defaultSettings { setKeybindings = Map.fromList [("ctrl-r", "redo")] }
          res  = mergeSettings base over
      Map.lookup "ctrl-c" (setKeybindings res) `shouldBe` Just "cancel"
      Map.lookup "ctrl-r" (setKeybindings res) `shouldBe` Just "redo"
