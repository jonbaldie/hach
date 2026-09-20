{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Hach.Settings
  ( Settings(..)
  , SettingsError(..)
  , renderSettingsError
  , defaultSettings
  , mergeSettings
  , loadSettingsFromFile
  , loadLayeredSettings
  ) where

import Hach.Types
import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Bifunctor (bimap)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.:?), (.!=), object, (.=), withObject
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
  , setEnvAllowlist     = ordNub (setEnvAllowlist later ++ setEnvAllowlist earlier)
  , setWorkingDirs      = ordNub (setWorkingDirs later ++ setWorkingDirs earlier)
  , setOutputStyle      = setOutputStyle later <|> setOutputStyle earlier
  , setAutoCompactLimit = setAutoCompactLimit later <|> setAutoCompactLimit earlier
  }

-- | Order-preserving unique: first occurrence wins, O(n log n).
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise           = x : go (Set.insert x seen) xs

-- | A settings file that exists on disk but could not be turned into 'Settings'.
data SettingsError = SettingsError
  { seFile    :: !FilePath
  , seMessage :: !String
  } deriving (Show, Eq)

-- | Render a settings failure for the user, naming the file and the reason.
renderSettingsError :: SettingsError -> String
renderSettingsError SettingsError{..} =
  "Settings file " <> seFile <> " could not be loaded: " <> seMessage

-- | Load settings from a specific JSON file. A missing file is 'Nothing'; a
-- file that is present but unreadable or undecodable is an error, never a
-- silent fallback to 'defaultSettings'.
loadSettingsFromFile :: FilePath -> IO (Either SettingsError (Maybe Settings))
loadSettingsFromFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right Nothing)
    else do
      res <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case res of
        Left err    -> Left (failure (show err))
        Right bytes -> bimap failure Just (Aeson.eitherDecodeStrict bytes)
  where
    failure = SettingsError path

-- | Discover and load layered settings:
-- 1. User layer (~/.claude/settings.json or ~/.agents/settings.json)
-- 2. Project layer (<workspace>/.claude/settings.json or <workspace>/.agents/settings.json)
-- 3. Local layer (<workspace>/.claude/settings.local.json or <workspace>/.agents/settings.local.json)
-- A layer whose file fails to load aborts the whole load: running on with the
-- layer's permission rules quietly dropped is the one outcome we must avoid.
loadLayeredSettings :: FilePath -> IO (Either SettingsError Settings)
loadLayeredSettings workspace = do
  mCustomConfig <- lookupEnv "CLAUDE_CONFIG_DIR"
  homeDir <- getHomeDirectory
  let userBase = case mCustomConfig of
        Just p  -> p
        Nothing -> homeDir

  layers <- traverse loadFirst
    [ [ userBase  </> ".claude" </> "settings.json"
      , userBase  </> ".agents" </> "settings.json"
      ]
    , [ workspace </> ".claude" </> "settings.json"
      , workspace </> ".agents" </> "settings.json"
      ]
    , [ workspace </> ".claude" </> "settings.local.json"
      , workspace </> ".agents" </> "settings.local.json"
      ]
    ]

  pure (foldl mergeSettings defaultSettings <$> sequence layers)
  where
    loadFirst [] = pure (Right defaultSettings)
    loadFirst (p : ps) = do
      res <- loadSettingsFromFile p
      case res of
        Left err       -> pure (Left err)
        Right (Just s) -> pure (Right s)
        Right Nothing  -> loadFirst ps
