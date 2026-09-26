{-# LANGUAGE OverloadedStrings #-}

-- | Runtime environment resolution: OpenRouter credentials and model from the
-- process environment and @.env@, layered settings, and the system prompt.
-- Command-line parsing lives in "Hach.CLI".
module Hach.Env
  ( EnvConfig(..)
  , EnvError(..)
  , renderEnvError
  , parseEnvContent
  , parseLineTwoModel
  , resolvePermissionMode
  , resolveMaxBudgetUsd
  , resolveWorkingDirs
  , resolveEffortLevel
  , resolveConfigWith
  , resolveConfigWithSettings
  , resolveEnvConfig
  , loadEnvConfig
  , loadProjectInstructions
  , loadProjectInstructionsFile
  , buildSystemPrompt
  , buildSystemPromptWithAppend
  ) where

import Hach.Settings
  ( Settings(..)
  , SettingsError
  , defaultSettings
  , loadLayeredSettings
  , renderSettingsError
  )
import Hach.Types (EffortLevel, PermissionMode(..), parseEffortLevel)
import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Parsed environment configuration for running the agent harness.
data EnvConfig = EnvConfig
  { envApiKey   :: !Text
  , envModel    :: !Text
  , envSettings :: !Settings
  } deriving (Show, Eq)

-- | Effective permission mode for a run. Precedence:
-- @--dangerously-skip-permissions@ > @--permission-mode@ flag > layered
-- settings > 'ModeDefault'.
resolvePermissionMode
  :: Maybe PermissionMode   -- ^ @--permission-mode@ flag
  -> Bool                   -- ^ @--dangerously-skip-permissions@
  -> Settings               -- ^ Layered settings
  -> PermissionMode
resolvePermissionMode mFlag skipPerms settings
  | skipPerms = ModeBypassPermissions
  | otherwise = fromMaybe ModeDefault (mFlag <|> setPermissionMode settings)

-- | Spending ceiling for a run. The CLI flag overrides settings.json.
-- Non-finite or negative settings values are ignored.
resolveMaxBudgetUsd
  :: Maybe Double
  -> Settings
  -> Maybe Double
resolveMaxBudgetUsd mFlag settings =
  mFlag <|> (setMaxBudgetUsd settings >>= finiteNonNegativeUsd)

finiteNonNegativeUsd :: Double -> Maybe Double
finiteNonNegativeUsd d
  | d >= 0 && not (isNaN d) && not (isInfinite d) = Just d
  | otherwise = Nothing

-- | Additional directories tools may reach, beyond the primary workspace
-- root. @--add-dir@ flags and the @working_directories@ setting accumulate
-- rather than override: a repeated directory is kept once, flags first.
resolveWorkingDirs
  :: [FilePath]             -- ^ @--add-dir@ flags, in command-line order
  -> Settings               -- ^ Layered settings
  -> [FilePath]
resolveWorkingDirs flagDirs settings =
  ordNubPaths (filter (not . null) (flagDirs ++ setWorkingDirs settings))

-- | Order-preserving unique: first occurrence wins.
ordNubPaths :: [FilePath] -> [FilePath]
ordNubPaths = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise           = x : go (Set.insert x seen) xs

-- | Resolve `effort_level` from layered settings. Unset stays unset so the
-- OpenRouter request omits `reasoning`. Unsupported values are an error.
resolveEffortLevel :: Settings -> Either String (Maybe EffortLevel)
resolveEffortLevel settings =
  case setEffortLevel settings of
    Nothing -> Right Nothing
    Just raw -> Just <$> parseEffortLevel raw

-- | Extract the model specifically from line two of the lines of .env.
-- Follows the requirement: "always use the model on line two of the .env".
parseLineTwoModel :: [Text] -> Maybe Text
parseLineTwoModel rawLines =
  case drop 1 rawLines of
    (line2 : _) ->
      let trimmed = T.strip line2
      in if T.null trimmed || T.isPrefixOf "#" trimmed
           then Nothing
           else case T.breakOn "=" trimmed of
                  (_, val) | not (T.null val) -> Just (cleanVal (T.drop 1 val))
                  _                           -> Just (cleanVal trimmed)
    _ -> Nothing
  where
    cleanVal = T.dropAround (\c -> c == '"' || c == '\'' || isSpace c)

-- | Parse key-value pairs from .env content, respecting comments and quotes.
parseEnvContent :: Text -> Map Text Text
parseEnvContent content =
  let ls = T.lines content
      pairs = [ (T.strip k, clean (T.drop 1 v))
              | line <- ls
              , let trimmed = stripExport (T.strip line)
              , not (T.null trimmed)
              , not (T.isPrefixOf "#" trimmed)
              , let (k, v) = T.breakOn "=" trimmed
              , not (T.null v)
              ]
  in Map.fromList pairs
  where
    clean = T.dropAround (\c -> c == '"' || c == '\'' || isSpace c)
    stripExport s = fromMaybe s (T.stripPrefix "export " s)

-- | Configuration resolver with layered Settings.
resolveConfigWithSettings
  :: Maybe Text      -- ^ CLI model override (e.g. from --model flag)
  -> Maybe Text      -- ^ OS process environment OPENROUTER_API_KEY
  -> Maybe Text      -- ^ OS process environment OPENROUTER_MODEL
  -> Maybe Text      -- ^ .env file content
  -> Settings        -- ^ Layered settings
  -> Either String EnvConfig
resolveConfigWithSettings mCliModel mOsApiKey mOsModel mDotEnvContent settings =
  let mDotEnvMap   = fmap parseEnvContent mDotEnvContent
      mDotLines    = fmap T.lines mDotEnvContent
      mDotKey      = mDotEnvMap >>= Map.lookup "OPENROUTER_API_KEY"
      mDotLine2    = mDotLines >>= parseLineTwoModel
      mDotKeyModel = mDotEnvMap >>= Map.lookup "OPENROUTER_MODEL"

      -- API Key resolution: OS environment takes precedence over .env
      mResolvedApiKey =
        (mOsApiKey >>= nonBlank) `orFallback` (mDotKey >>= nonBlank)

      -- Model resolution: CLI flag > OS environment > line 2 of .env > OPENROUTER_MODEL in .env > settings
      mResolvedModel =
        (mCliModel >>= nonBlank)
          `orFallback` (mOsModel >>= nonBlank)
          `orFallback` (mDotLine2 >>= nonBlank)
          `orFallback` (mDotKeyModel >>= nonBlank)
          `orFallback` (setModel settings >>= nonBlank)
  in case (mResolvedApiKey, mResolvedModel) of
    (Just key, Just model) ->
      Right EnvConfig { envApiKey = key, envModel = model, envSettings = settings }
    (Nothing, _) ->
      Left "OPENROUTER_API_KEY is missing from both process environment and .env"
    (_, Nothing) ->
      Left "OpenRouter model not specified (use --model CLI flag, OPENROUTER_MODEL env var, line 2 of .env, or settings.json)"
  where
    nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s
    orFallback (Just x) _ = Just x
    orFallback Nothing my = my

-- | Pure configuration resolver implementing precedence:
-- 1. API key: OS process environment -> .env file.
-- 2. Model: CLI flag -> OS process environment -> line 2 of .env -> OPENROUTER_MODEL in .env.
resolveConfigWith
  :: Maybe Text      -- ^ CLI model override (e.g. from --model flag)
  -> Maybe Text      -- ^ OS process environment OPENROUTER_API_KEY
  -> Maybe Text      -- ^ OS process environment OPENROUTER_MODEL
  -> Maybe Text      -- ^ .env file content
  -> Either String EnvConfig
resolveConfigWith mCliModel mOsApiKey mOsModel mDotEnvContent =
  resolveConfigWithSettings mCliModel mOsApiKey mOsModel mDotEnvContent defaultSettings

-- | Why startup configuration could not be resolved.
data EnvError
  = EnvSettingsInvalid SettingsError  -- ^ A settings file exists but could not be loaded.
  | EnvConfigUnresolved String        -- ^ API key or model could not be resolved.
  deriving (Show, Eq)

-- | Render a startup failure, with the hint that fits the cause.
renderEnvError :: EnvError -> String
renderEnvError (EnvSettingsInvalid err) =
  "Configuration error: " <> renderSettingsError err <> "\n" <>
  "Fix that file or move it aside; hach will not run with its settings ignored."
renderEnvError (EnvConfigUnresolved err) =
  "Configuration error: " <> err <> "\n" <>
  "Please set OPENROUTER_API_KEY in the environment or in .env."

-- | Resolve configuration from process environment, .env file, and layered settings.
resolveEnvConfig
  :: Maybe Text       -- ^ Optional CLI model override
  -> Maybe FilePath   -- ^ Optional path to .env file
  -> IO (Either EnvError EnvConfig)
resolveEnvConfig mCliModel mDotEnvPath = do
  mOsApiKey <- fmap (fmap T.pack) (lookupEnv "OPENROUTER_API_KEY")
  mOsModel  <- fmap (fmap T.pack) (lookupEnv "OPENROUTER_MODEL")
  mDotEnvContent <- case mDotEnvPath of
    Just path -> do
      exists <- doesFileExist path
      if exists then Just <$> TIO.readFile path else pure Nothing
    Nothing -> pure Nothing
  settingsRes <- loadLayeredSettings "."
  pure $ case settingsRes of
    Left err -> Left (EnvSettingsInvalid err)
    Right settings ->
      first EnvConfigUnresolved
        (resolveConfigWithSettings mCliModel mOsApiKey mOsModel mDotEnvContent settings)

-- | Legacy helper to load configuration specifically from a .env file.
loadEnvConfig :: FilePath -> IO (Either EnvError EnvConfig)
loadEnvConfig path = resolveEnvConfig Nothing (Just path)

-- | Load project instructions from AGENTS.md, AGENT.md, or CLAUDE.md in the workspace directory.
-- Precedence: AGENTS.md is preferred; then AGENT.md; then CLAUDE.md.
loadProjectInstructions :: FilePath -> IO (Maybe Text)
loadProjectInstructions = fmap (fmap snd) . loadProjectInstructionsFile

-- | Like 'loadProjectInstructions', also naming the file the instructions came from.
loadProjectInstructionsFile :: FilePath -> IO (Maybe (FilePath, Text))
loadProjectInstructionsFile workspace = firstExisting ["AGENTS.md", "AGENT.md", "CLAUDE.md"]
  where
    firstExisting [] = pure Nothing
    firstExisting (name : names) = do
      let fp = workspace </> name
      exists <- doesFileExist fp
      if exists then fmap ((,) name) <$> readFileUtf8 fp else firstExisting names

    readFileUtf8 fp = do
      res <- try (BS.readFile fp) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _ -> pure Nothing
        Right bytes -> pure (Just (TE.decodeUtf8With (\_ _ -> Just ' ') bytes))

-- | Build the combined system prompt, appending project guidelines if present.
buildSystemPrompt :: Maybe Text -> Text
buildSystemPrompt mProjectGuidelines =
  let basePrompt =
        "You are an expert autonomous coding assistant. You have access to tools " <>
        "to inspect files, write code, run shell commands, and explore the workspace. " <>
        "Always inspect existing code before making changes, verify your work by running commands, " <>
        "and provide a concise final summary when complete."
  in case mProjectGuidelines of
    Just guidelines | not (T.null (T.strip guidelines)) ->
      basePrompt <> "\n\n# Project Guidelines:\n" <> guidelines
    _ -> basePrompt

-- | Build the combined system prompt, appending project guidelines and optional custom instructions.
buildSystemPromptWithAppend :: Maybe Text -> Maybe Text -> Text
buildSystemPromptWithAppend mProjectGuidelines mAppend =
  let basePrompt = buildSystemPrompt mProjectGuidelines
  in case mAppend of
    Just extra | not (T.null (T.strip extra)) ->
      basePrompt <> "\n\n" <> extra
    _ -> basePrompt
