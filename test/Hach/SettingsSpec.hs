{-# LANGUAGE OverloadedStrings #-}

module Hach.SettingsSpec (spec) where

import Hach.Settings
import Hach.Types
import Control.Exception (finally)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

spec :: Spec
spec = describe "Hach.Settings" $ do
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

  describe "Loading files that fail to decode (Issue #165)" $ do
    it "reports the file and the reason instead of discarding it" $
      withTemporaryDirectory $ \dir -> do
        let path = dir </> "settings.json"
        writeFile path "{\"max_budget_usd\": 0,}"
        res <- loadSettingsFromFile path
        case res of
          Right s -> expectationFailure ("expected a failure, got " <> show s)
          Left err -> do
            seFile err `shouldBe` path
            seMessage err `shouldNotBe` ""
            renderSettingsError err `shouldContain` path

    it "treats a missing file as absent, not as a failure" $
      withTemporaryDirectory $ \dir ->
        loadSettingsFromFile (dir </> "settings.json") `shouldReturn` Right Nothing

    it "fails the layered load rather than dropping the layer's rules" $
      withTemporaryWorkspace $ \workspace -> do
        let path = workspace </> ".claude" </> "settings.json"
        createDirectoryIfMissing True (workspace </> ".claude")
        writeFile path "{\"permission_rules\": 7}"
        res <- loadLayeredSettings workspace
        case res of
          Right s -> expectationFailure ("expected a failure, got " <> show s)
          Left err -> seFile err `shouldBe` path

    it "does not fall through to the .agents copy when .claude fails to decode" $
      withTemporaryWorkspace $ \workspace -> do
        createDirectoryIfMissing True (workspace </> ".claude")
        createDirectoryIfMissing True (workspace </> ".agents")
        writeFile (workspace </> ".claude" </> "settings.json") "{\"max_budget_usd\": 0,}"
        writeFile (workspace </> ".agents" </> "settings.json") "{\"max_budget_usd\": 0}"
        res <- loadLayeredSettings workspace
        either seFile (const "") res `shouldBe` (workspace </> ".claude" </> "settings.json")

    it "still merges layers that all decode" $
      withTemporaryWorkspace $ \workspace -> do
        createDirectoryIfMissing True (workspace </> ".claude")
        writeFile (workspace </> ".claude" </> "settings.json") "{\"model\": \"project-model\", \"theme\": \"nord\"}"
        writeFile (workspace </> ".claude" </> "settings.local.json") "{\"model\": \"local-model\"}"
        res <- loadLayeredSettings workspace
        fmap setModel res `shouldBe` Right (Just "local-model")
        fmap setTheme res `shouldBe` Right (Just "nord")

-- | A fresh empty directory, removed afterwards.
withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action = do
  temporaryDirectory <- getTemporaryDirectory
  (dir, handle) <- openTempFile temporaryDirectory "hach-settings-test"
  hClose handle
  removeFile dir
  createDirectory dir
  action dir `finally` removeDirectoryRecursive dir

-- | A temporary workspace with the user settings layer pointed somewhere empty,
-- so the developer's own ~/.claude/settings.json cannot influence the result.
withTemporaryWorkspace :: (FilePath -> IO a) -> IO a
withTemporaryWorkspace action =
  withTemporaryDirectory $ \dir -> do
    let workspace = dir </> "workspace"
    createDirectory workspace
    original <- lookupEnv "CLAUDE_CONFIG_DIR"
    setEnv "CLAUDE_CONFIG_DIR" (dir </> "isolated-config")
    action workspace `finally` maybe (unsetEnv "CLAUDE_CONFIG_DIR") (setEnv "CLAUDE_CONFIG_DIR") original
