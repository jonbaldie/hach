{-# LANGUAGE OverloadedStrings #-}

module Agent.Env
  ( EnvConfig(..)
  , parseEnvContent
  , parseLineTwoModel
  , loadEnvConfig
  ) where

import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)

-- | Parsed environment configuration from .env.
data EnvConfig = EnvConfig
  { envApiKey :: !Text
  , envModel  :: !Text
  } deriving (Show, Eq)

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

-- | Load configuration from a specified .env file.
-- Guaranteed to use the model on line two, falling back to OPENROUTER_MODEL key.
loadEnvConfig :: FilePath -> IO (Either String EnvConfig)
loadEnvConfig path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left ("File not found: " <> path)
    else do
      content <- TIO.readFile path
      let rawLines = T.lines content
          envMap   = parseEnvContent content
          mKey     = Map.lookup "OPENROUTER_API_KEY" envMap
          -- First try line 2 directly as specified in prompt; then fallback to map lookup
          mModel   = parseLineTwoModel rawLines
                     `orFallback` Map.lookup "OPENROUTER_MODEL" envMap
      case (mKey, mModel) of
        (Just key, Just model)
          | T.null key -> pure $ Left "OPENROUTER_API_KEY in .env is empty"
          | T.null model -> pure $ Left "OPENROUTER_MODEL on line 2 of .env is empty"
          | otherwise -> pure $ Right EnvConfig { envApiKey = key, envModel = model }
        (Nothing, _) ->
          pure $ Left "OPENROUTER_API_KEY missing from .env"
        (_, Nothing) ->
          pure $ Left "Could not determine OPENROUTER_MODEL from line 2 of .env"
  where
    orFallback (Just x) _ = Just x
    orFallback Nothing my = my
