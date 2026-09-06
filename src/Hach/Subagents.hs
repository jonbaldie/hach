{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Hach.Subagents
  ( AgentDefinition(..)
  , builtinExploreAgent
  , builtinPlanAgent
  , builtinAgents
  , parseAgentDefinition
  , canSpawnSubagent
  , defaultMaxNestingDepth
  , defaultMaxConcurrency
  , discoverAgents
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)

-- | Definition of a subagent persona/role.
data AgentDefinition = AgentDefinition
  { adName         :: !Text
  , adDescription  :: !Text
  , adModel        :: !(Maybe Text)
  , adTools        :: ![Text]
  , adSystemPrompt :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON AgentDefinition
instance FromJSON AgentDefinition

-- | Built-in exploration subagent.
builtinExploreAgent :: AgentDefinition
builtinExploreAgent = AgentDefinition
  { adName         = "Explore"
  , adDescription  = "Fast codebase exploration, directory scanning, and pattern matching"
  , adModel        = Nothing
  , adTools        = ["read_file", "list_dir", "find_files", "grep_search", "Glob", "Grep", "WebFetch", "WebSearch"]
  , adSystemPrompt = "You are an exploration subagent. Find and inspect files, search codebase, and report your findings concisely."
  }

-- | Built-in planning subagent.
builtinPlanAgent :: AgentDefinition
builtinPlanAgent = AgentDefinition
  { adName         = "Plan"
  , adDescription  = "Read-only architectural analysis and step-by-step implementation planning"
  , adModel        = Nothing
  , adTools        = ["read_file", "list_dir", "find_files", "grep_search", "Glob", "Grep"]
  , adSystemPrompt = "You are a planning subagent. Research the problem and generate a step-by-step execution plan without modifying files."
  }

-- | Map of all built-in agent definitions.
builtinAgents :: Map Text AgentDefinition
builtinAgents = Map.fromList
  [ ("Explore", builtinExploreAgent)
  , ("Plan", builtinPlanAgent)
  ]

-- | Default nesting depth limit.
defaultMaxNestingDepth :: Int
defaultMaxNestingDepth = 3

-- | Default concurrency limit for running subagents.
defaultMaxConcurrency :: Int
defaultMaxConcurrency = 20

-- | Check whether another subagent can be spawned within depth and concurrency bounds.
canSpawnSubagent :: Int -> Int -> Int -> Bool
canSpawnSubagent currentDepth maxDepth currentRunning =
  currentDepth < maxDepth && currentRunning < defaultMaxConcurrency

-- | Parse a markdown agent definition with YAML frontmatter.
parseAgentDefinition :: FilePath -> Text -> Either String AgentDefinition
parseAgentDefinition _path raw =
  let ls = T.lines raw
  in case ls of
    (firstLine : rest) | T.strip firstLine == "---" ->
      case break (\l -> T.strip l == "---") rest of
        (fmLines, _delim : bodyLines) ->
          let fields = [ (T.strip k, T.strip (T.drop 1 v))
                       | l <- fmLines
                       , not (T.null (T.strip l))
                       , let (k, v) = T.breakOn ":" l
                       , not (T.null v)
                       ]
              mName = lookup "name" fields
              mDesc = lookup "description" fields
              mModel = lookup "model" fields
              mToolsRaw = lookup "tools" fields
              tools = case mToolsRaw of
                Just t  -> map (T.strip . cleanQuote) (T.splitOn "," t)
                Nothing -> []
              cleanQuote = T.dropAround (\c -> c == '"' || c == '\'' || isSpace c)
          in case mName of
            Nothing -> Left "Missing 'name' in agent definition frontmatter."
            Just nm -> Right AgentDefinition
              { adName         = cleanQuote nm
              , adDescription  = maybe "" cleanQuote mDesc
              , adModel        = fmap cleanQuote mModel
              , adTools        = tools
              , adSystemPrompt = T.strip (T.unlines bodyLines)
              }
        _ -> Left "Missing closing '---' frontmatter delimiter."
    _ -> Left "Agent definition file must start with '---'."

-- | Discover agents from workspace (.claude/agents/*.md or .agents/agents/*.md).
discoverAgents :: FilePath -> IO (Map Text AgentDefinition)
discoverAgents workspace = do
  claudeAgents <- scanDir (workspace </> ".claude" </> "agents")
  agentsDirAgents <- scanDir (workspace </> ".agents" </> "agents")
  let customMap = Map.fromList [ (adName a, a) | a <- claudeAgents ++ agentsDirAgents ]
  pure (Map.union customMap builtinAgents)
  where
    scanDir dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure []
        else do
          entriesRes <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
          case entriesRes of
            Left _ -> pure []
            Right entries -> do
              let mdFiles = [ dir </> e | e <- entries, takeExtension e == ".md" ]
              defs <- mapM loadOne mdFiles
              pure (catMaybes defs)

    loadOne fp = do
      res <- try (BS.readFile fp) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _ -> pure Nothing
        Right bytes ->
          let txt = TE.decodeUtf8With (\_ _ -> Just ' ') bytes
          in case parseAgentDefinition fp txt of
            Right def -> pure (Just def)
            Left _    -> pure Nothing
