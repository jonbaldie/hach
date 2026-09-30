{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  , InferenceFlags(..)
  , noInferenceFlags
  , inferenceEnvNames
  , resolveInferenceConfig
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
import Hach.Inference
  ( InferenceConnection
  , InferenceInterface(..)
  , mkInferenceConnection
  , openAIBaseUrl
  , openRouterBaseUrl
  , parseInferenceInterface
  )
import Hach.Types
  ( EffortLevel
  , PermissionMode(..)
  , parseEffortLevel
  )
import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)
import Data.Bifunctor (first)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable (for)
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
  { envConnection :: !InferenceConnection
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
-- request omits reasoning options. Unsupported values are an error.
resolveEffortLevel :: Settings -> Either String (Maybe EffortLevel)
resolveEffortLevel settings =
  case setEffortLevel settings of
    Nothing -> Right Nothing
    Just raw -> Just <$> parseEffortLevel raw

-- | Extract the model specifically from line two of the lines of .env.
-- Follows the requirement: "always use the model on line two of the .env".
-- An assignment to a provider-selection or OpenAI variable is never a
-- model: it would otherwise send a provider name, endpoint, or credential
-- to OpenRouter as the model identifier.
parseLineTwoModel :: [Text] -> Maybe Text
parseLineTwoModel rawLines =
  case drop 1 rawLines of
    (line2 : _) ->
      let trimmed = T.strip line2
      in if T.null trimmed || T.isPrefixOf "#" trimmed
           then Nothing
           else case T.breakOn "=" trimmed of
                  (name, val)
                    | not (T.null val) ->
                        if T.strip (stripExport name) `elem` notModels
                          then Nothing
                          else Just (cleanVal (T.drop 1 val))
                  _ -> Just (cleanVal trimmed)
    _ -> Nothing
  where
    cleanVal = T.dropAround (\c -> c == '"' || c == '\'' || isSpace c)
    stripExport s = fromMaybe s (T.stripPrefix "export " s)
    notModels = ["HACH_PROVIDER", "OPENAI_API_KEY", "OPENAI_MODEL", "OPENAI_BASE_URL"]

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

-- | Command-line choices that shape the inference connection. Values are
-- raw: they are validated during resolution, after precedence is applied.
data InferenceFlags = InferenceFlags
  { flagProvider :: !(Maybe Text)  -- ^ @--provider@
  , flagBaseUrl  :: !(Maybe Text)  -- ^ @--base-url@
  , flagModel    :: !(Maybe Text)  -- ^ @--model@
  } deriving (Show, Eq)

-- | No inference flags given.
noInferenceFlags :: InferenceFlags
noInferenceFlags = InferenceFlags Nothing Nothing Nothing

-- | Environment variables that can shape the inference connection.
inferenceEnvNames :: [Text]
inferenceEnvNames =
  [ "HACH_PROVIDER"
  , "OPENROUTER_API_KEY", "OPENROUTER_MODEL"
  , "OPENAI_API_KEY", "OPENAI_MODEL", "OPENAI_BASE_URL"
  ]

-- | Resolve the inference connection and model. The interface is chosen
-- first (@--provider@ > process @HACH_PROVIDER@ > dotenv @HACH_PROVIDER@ >
-- @llm_provider@ > openrouter); only then are that interface's own endpoint,
-- key, and model looked up, so one service's credentials can never be sent
-- to another.
--
-- * @openai-compatible@: base URL from @--base-url@ > @OPENAI_BASE_URL@
--   (process, then dotenv) > @llm_base_url@ > the OpenAI API; optional key
--   from @OPENAI_API_KEY@; model from @--model@ > @OPENAI_MODEL@ > settings.
-- * @openrouter@: base URL from @--base-url@ or OpenRouter's; required
--   @OPENROUTER_API_KEY@; model from @--model@ > process @OPENROUTER_MODEL@ >
--   line two of .env > dotenv @OPENROUTER_MODEL@ > settings.
resolveInferenceConfig
  :: InferenceFlags
  -> Map Text Text   -- ^ Process environment (see 'inferenceEnvNames')
  -> Maybe Text      -- ^ .env file content
  -> Settings        -- ^ Layered settings
  -> Either String EnvConfig
resolveInferenceConfig InferenceFlags{..} processEnv mDotEnvContent settings = do
  let rawProvider = flagProvider <|> fromProcess "HACH_PROVIDER"
                      <|> fromDotEnv "HACH_PROVIDER" <|> setLlmProvider settings
  iface <- maybe (Right InterfaceOpenRouter) parseInferenceInterface rawProvider
  case iface of
    InterfaceOpenAICompatible -> do
      let baseUrl = fromMaybe openAIBaseUrl
            (flagBaseUrl <|> fromProcess "OPENAI_BASE_URL"
               <|> fromDotEnv "OPENAI_BASE_URL" <|> setLlmBaseUrl settings)
          mKey = nonBlankOf "OPENAI_API_KEY"
          mModel = (flagModel >>= nonBlank)
            <|> nonBlankOf "OPENAI_MODEL"
            <|> (setModel settings >>= nonBlank)
      model <- maybe (Left compatibleModelMissing) Right mModel
      conn <- connection "OPENAI_API_KEY" iface baseUrl mKey
      pure (EnvConfig conn model settings)
    InterfaceOpenRouter -> do
      let baseUrl = fromMaybe openRouterBaseUrl flagBaseUrl
          mModel = (flagModel >>= nonBlank)
            <|> (fromProcess "OPENROUTER_MODEL" >>= nonBlank)
            <|> (mDotEnvContent >>= parseLineTwoModel . T.lines >>= nonBlank)
            <|> (fromDotEnv "OPENROUTER_MODEL" >>= nonBlank)
            <|> (setModel settings >>= nonBlank)
      key <- maybe (Left openRouterKeyMissing) Right (nonBlankOf "OPENROUTER_API_KEY")
      model <- maybe (Left openRouterModelMissing) Right mModel
      conn <- connection "OPENROUTER_API_KEY" iface baseUrl (Just key)
      pure (EnvConfig conn model settings)
  where
    dotEnv = maybe Map.empty parseEnvContent mDotEnvContent
    fromProcess name = Map.lookup name processEnv
    fromDotEnv name = Map.lookup name dotEnv

    -- A blank key or model counts as absent, so dotenv can fill a blank
    -- process value.
    nonBlankOf name = (fromProcess name >>= nonBlank) <|> (fromDotEnv name >>= nonBlank)
    nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s

    connection keyName iface baseUrl mKey =
      first (\err -> if "Invalid base URL" `isPrefixOf` err then err else keyName <> " is invalid: " <> err)
        (mkInferenceConnection iface baseUrl mKey)

    openRouterKeyMissing =
      "OPENROUTER_API_KEY is missing from both process environment and .env. "
        <> "Set it, or select another provider with --provider or HACH_PROVIDER."
    openRouterModelMissing =
      "OpenRouter model not specified (use --model CLI flag, OPENROUTER_MODEL env var, line 2 of .env, or settings.json)"
    compatibleModelMissing =
      "openai-compatible model not specified (use --model CLI flag, OPENAI_MODEL env var or .env entry, or settings.json)"

-- | OpenRouter-only configuration resolver with layered Settings. Provider
-- selection from dotenv or settings is ignored.
resolveConfigWithSettings
  :: Maybe Text      -- ^ CLI model override (e.g. from --model flag)
  -> Maybe Text      -- ^ OS process environment OPENROUTER_API_KEY
  -> Maybe Text      -- ^ OS process environment OPENROUTER_MODEL
  -> Maybe Text      -- ^ .env file content
  -> Settings        -- ^ Layered settings
  -> Either String EnvConfig
resolveConfigWithSettings mCliModel mOsApiKey mOsModel =
  resolveInferenceConfig
    noInferenceFlags { flagProvider = Just "openrouter", flagModel = mCliModel }
    (Map.fromList (catMaybes
      [ (,) "OPENROUTER_API_KEY" <$> mOsApiKey
      , (,) "OPENROUTER_MODEL" <$> mOsModel
      ]))

-- | Pure OpenRouter configuration resolver implementing precedence:
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
  | EnvConfigUnresolved String        -- ^ Provider, endpoint, key, or model could not be resolved.
  deriving (Show, Eq)

-- | Render a startup failure, with the hint that fits the cause.
renderEnvError :: EnvError -> String
renderEnvError (EnvSettingsInvalid err) =
  "Configuration error: " <> renderSettingsError err <> "\n" <>
  "Fix that file or move it aside; hach will not run with its settings ignored."
renderEnvError (EnvConfigUnresolved err) =
  "Configuration error: " <> err <> "\n" <>
  "Run hach --help for the inference configuration options."

-- | Resolve configuration from process environment, .env file, and layered settings.
resolveEnvConfig
  :: InferenceFlags   -- ^ Command-line inference choices
  -> Maybe FilePath   -- ^ Optional path to .env file
  -> IO (Either EnvError EnvConfig)
resolveEnvConfig flags mDotEnvPath = do
  processEnv <- fmap (Map.fromList . catMaybes) . for inferenceEnvNames $ \name ->
    fmap ((,) name . T.pack) <$> lookupEnv (T.unpack name)
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
        (resolveInferenceConfig flags processEnv mDotEnvContent settings)

-- | Load configuration from the environment and a .env file, as the live
-- integration executable does.
loadEnvConfig :: FilePath -> IO (Either EnvError EnvConfig)
loadEnvConfig path = resolveEnvConfig noInferenceFlags (Just path)

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
