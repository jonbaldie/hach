{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Hach.Settings
  ( Settings(..)
  , defaultSettings
  , mergeSettings
  , loadSettingsFromFile
  , loadLayeredSettings
  ) where

import Hach.Types
import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.:?), (.!=), object, (.=), withObject
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Global, project, and local settings record.
data Settings = Settings
  { setModel            :: !(Maybe Text)
  , setFallbackModel    :: !(Maybe Text)
  , setEffortLevel      :: !(Maybe Text)
  , setMaxBudgetUsd     :: !(Maybe Double)
  , setPermissionMode   :: !(Maybe PermissionMode)
  , setPermissionRules  :: ![PermissionRule]
  , setHooks            :: !(Map HookEvent [HookHandler])
  , setTheme            :: !(Maybe Text)
  , setKeybindings      :: !(Map Text Text)
  , setStatusLine       :: !(Maybe Text)
  , setEnvAllowlist     :: ![Text]
  , setWorkingDirs      :: ![FilePath]
  , setOutputStyle      :: !(Maybe OutputStyle)
  , setAutoCompactLimit :: !(Maybe Int)
  } deriving (Show, Eq, Generic)

-- | Sensible initial empty settings.
defaultSettings :: Settings
defaultSettings = Settings
  { setModel            = Nothing
  , setFallbackModel    = Nothing
  , setEffortLevel      = Nothing
  , setMaxBudgetUsd     = Nothing
  , setPermissionMode   = Nothing
  , setPermissionRules  = []
  , setHooks            = Map.empty
  , setTheme            = Nothing
  , setKeybindings      = Map.empty
  , setStatusLine       = Nothing
  , setEnvAllowlist     = []
  , setWorkingDirs      = []
  , setOutputStyle      = Nothing
  , setAutoCompactLimit = Nothing
  }

instance ToJSON Settings where
  toJSON Settings{..} = object
    [ "model"               .= setModel
    , "fallback_model"      .= setFallbackModel
    , "effort_level"        .= setEffortLevel
    , "max_budget_usd"      .= setMaxBudgetUsd
    , "permission_mode"     .= setPermissionMode
    , "permission_rules"    .= setPermissionRules
    , "hooks"               .= setHooks
    , "theme"               .= setTheme
    , "keybindings"         .= setKeybindings
    , "status_line"         .= setStatusLine
    , "env_allowlist"       .= setEnvAllowlist
    , "working_directories" .= setWorkingDirs
    , "output_style"        .= setOutputStyle
    , "auto_compact_limit"  .= setAutoCompactLimit
    ]

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o ->
    Settings
      <$> o .:? "model"
      <*> o .:? "fallback_model"
      <*> o .:? "effort_level"
      <*> o .:? "max_budget_usd"
      <*> o .:? "permission_mode"
      <*> o .:? "permission_rules" .!= []
      <*> o .:? "hooks"            .!= Map.empty
      <*> o .:? "theme"
      <*> o .:? "keybindings"      .!= Map.empty
      <*> o .:? "status_line"
      <*> o .:? "env_allowlist"    .!= []
      <*> o .:? "working_directories" .!= []
      <*> o .:? "output_style"
      <*> o .:? "auto_compact_limit"

-- | Merge two settings layers. The second (later) layer takes precedence over the first.
mergeSettings :: Settings -> Settings -> Settings
mergeSettings earlier later = Settings
  { setModel            = setModel later <|> setModel earlier
  , setFallbackModel    = setFallbackModel later <|> setFallbackModel earlier
  , setEffortLevel      = setEffortLevel later <|> setEffortLevel earlier
  , setMaxBudgetUsd     = setMaxBudgetUsd later <|> setMaxBudgetUsd earlier
  , setPermissionMode   = setPermissionMode later <|> setPermissionMode earlier
  , setPermissionRules  = setPermissionRules later ++ setPermissionRules earlier
  , setHooks            = Map.unionWith (++) (setHooks later) (setHooks earlier)
  , setTheme            = setTheme later <|> setTheme earlier
  , setKeybindings      = Map.union (setKeybindings later) (setKeybindings earlier)
  , setStatusLine       = setStatusLine later <|> setStatusLine earlier
  , setEnvAllowlist     = nub (setEnvAllowlist later ++ setEnvAllowlist earlier)
  , setWorkingDirs      = nub (setWorkingDirs later ++ setWorkingDirs earlier)
  , setOutputStyle      = setOutputStyle later <|> setOutputStyle earlier
  , setAutoCompactLimit = setAutoCompactLimit later <|> setAutoCompactLimit earlier
  }

-- | Load settings from a specific JSON file.
loadSettingsFromFile :: FilePath -> IO (Maybe Settings)
loadSettingsFromFile path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      res <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _      -> pure Nothing
        Right bytes -> pure (Aeson.decodeStrict bytes)

-- | Discover and load layered settings:
-- 1. User layer (~/.claude/settings.json or ~/.agents/settings.json)
-- 2. Project layer (<workspace>/.claude/settings.json or <workspace>/.agents/settings.json)
-- 3. Local layer (<workspace>/.claude/settings.local.json or <workspace>/.agents/settings.local.json)
loadLayeredSettings :: FilePath -> IO Settings
loadLayeredSettings workspace = do
  mCustomConfig <- lookupEnv "CLAUDE_CONFIG_DIR"
  homeDir <- getHomeDirectory
  let userBase = case mCustomConfig of
        Just p  -> p
        Nothing -> homeDir

  userSettings <- loadFirst
    [ userBase </> ".claude" </> "settings.json"
    , userBase </> ".agents" </> "settings.json"
    ]

  projectSettings <- loadFirst
    [ workspace </> ".claude" </> "settings.json"
    , workspace </> ".agents" </> "settings.json"
    ]

  localSettings <- loadFirst
    [ workspace </> ".claude" </> "settings.local.json"
    , workspace </> ".agents" </> "settings.local.json"
    ]

  let merged = foldl mergeSettings defaultSettings [userSettings, projectSettings, localSettings]
  pure merged
  where
    loadFirst [] = pure defaultSettings
    loadFirst (p : ps) = do
      mSet <- loadSettingsFromFile p
      case mSet of
        Just s  -> pure s
        Nothing -> loadFirst ps
