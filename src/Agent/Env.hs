{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.Env
  ( EnvConfig(..)
  , CliOptions(..)
  , parseEnvContent
  , parseLineTwoModel
  , parseCliArgs
  , resolveConfigWith
  , resolveEnvConfig
  , loadEnvConfig
  ) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

-- | Parsed environment configuration for running the agent harness.
data EnvConfig = EnvConfig
  { envApiKey :: !Text
  , envModel  :: !Text
  } deriving (Show, Eq)

-- | CLI options parsed from command line arguments.
data CliOptions = CliOptions
  { optModel  :: !(Maybe Text)
  , optPrompt :: !(Maybe Text)
  } deriving (Show, Eq)

-- | Parse command line arguments into 'CliOptions'.
-- Supports:
--   --model <name>
--   --model=<name>
--   -m <name>
--   -m=<name>
-- Positional arguments are concatenated to form the task prompt.
parseCliArgs :: [String] -> Either String CliOptions
parseCliArgs args = go args Nothing []
  where
    go [] mModel promptWords =
      let mPrompt = case promptWords of
            [] -> Nothing
            ws -> Just (T.pack (unwords ws))
      in Right CliOptions { optModel = mModel, optPrompt = mPrompt }

    go ("--model" : val : rest) _ promptWords
      | null (dropWhile isSpace val) = Left "--model requires a non-empty argument"
      | otherwise = go rest (Just (T.strip (T.pack val))) promptWords

    go ["--model"] _ _ = Left "--model requires an argument"

    go (arg : rest) mModel promptWords
      | Just val <- stripPrefix "--model=" arg =
          if null (dropWhile isSpace val)
            then Left "--model= requires a non-empty argument"
            else go rest (Just (T.strip (T.pack val))) promptWords
      | arg == "-m" =
          case rest of
            (val : rest')
              | not (null (dropWhile isSpace val)) ->
                  go rest' (Just (T.strip (T.pack val))) promptWords
              | otherwise -> Left "-m requires a non-empty argument"
            [] -> Left "-m requires an argument"
      | Just val <- stripPrefix "-m=" arg =
          if null (dropWhile isSpace val)
            then Left "-m= requires a non-empty argument"
            else go rest (Just (T.strip (T.pack val))) promptWords
      | otherwise =
          go rest mModel (promptWords ++ [arg])

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
              , let trimmed = T.strip line
              , not (T.null trimmed)
              , not (T.isPrefixOf "#" trimmed)
              , let (k, v) = T.breakOn "=" trimmed
              , not (T.null v)
              ]
  in Map.fromList pairs
  where
    clean = T.dropAround (\c -> c == '"' || c == '\'' || isSpace c)

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
  let mDotEnvMap   = fmap parseEnvContent mDotEnvContent
      mDotLines    = fmap T.lines mDotEnvContent
      mDotKey      = mDotEnvMap >>= Map.lookup "OPENROUTER_API_KEY"
      mDotLine2    = mDotLines >>= parseLineTwoModel
      mDotKeyModel = mDotEnvMap >>= Map.lookup "OPENROUTER_MODEL"

      -- API Key resolution: OS environment takes precedence over .env
      mResolvedApiKey =
        (mOsApiKey >>= nonBlank) `orFallback` (mDotKey >>= nonBlank)

      -- Model resolution: CLI flag > OS environment > line 2 of .env > OPENROUTER_MODEL in .env
      mResolvedModel =
        (mCliModel >>= nonBlank)
          `orFallback` (mOsModel >>= nonBlank)
          `orFallback` (mDotLine2 >>= nonBlank)
          `orFallback` (mDotKeyModel >>= nonBlank)
  in case (mResolvedApiKey, mResolvedModel) of
    (Just key, Just model) ->
      Right EnvConfig { envApiKey = key, envModel = model }
    (Nothing, _) ->
      Left "OPENROUTER_API_KEY is missing from both process environment and .env"
    (_, Nothing) ->
      Left "OpenRouter model not specified (use --model CLI flag, OPENROUTER_MODEL env var, or line 2 of .env)"
  where
    nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s
    orFallback (Just x) _ = Just x
    orFallback Nothing my = my

-- | Resolve configuration from process environment and optionally a .env file.
resolveEnvConfig
  :: Maybe Text       -- ^ Optional CLI model override
  -> Maybe FilePath   -- ^ Optional path to .env file
  -> IO (Either String EnvConfig)
resolveEnvConfig mCliModel mDotEnvPath = do
  mOsApiKey <- fmap (fmap T.pack) (lookupEnv "OPENROUTER_API_KEY")
  mOsModel  <- fmap (fmap T.pack) (lookupEnv "OPENROUTER_MODEL")
  mDotEnvContent <- case mDotEnvPath of
    Just path -> do
      exists <- doesFileExist path
      if exists then Just <$> TIO.readFile path else pure Nothing
    Nothing -> pure Nothing
  pure $ resolveConfigWith mCliModel mOsApiKey mOsModel mDotEnvContent

-- | Legacy helper to load configuration specifically from a .env file.
loadEnvConfig :: FilePath -> IO (Either String EnvConfig)
loadEnvConfig path = resolveEnvConfig Nothing (Just path)
