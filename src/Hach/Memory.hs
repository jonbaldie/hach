{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Memory
  ( Rule(..)
  , parseRuleFile
  , ruleMatchesFiles
  , resolveMemoryImports
  , loadHierarchicalMemory
  , loadRules
  , loadFullMemory
  ) where

import Hach.Paths (resolveWorkspacePath)
import Hach.Permissions (matchGlob)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , listDirectory
  , makeAbsolute
  )
import System.FilePath
  ( (</>)
  , isAbsolute
  , makeRelative
  , splitDirectories
  , takeDirectory
  , takeExtension
  )

data Rule = Rule
  { ruleFile     :: !FilePath
  , rulePatterns :: ![Text]
  , ruleContent  :: !Text
  } deriving (Show, Eq)

-- | Parse a rule markdown file with frontmatter:
-- ---
-- paths: src/**/*.hs, test/**/*.hs
-- ---
-- <content>
parseRuleFile :: FilePath -> Text -> Maybe Rule
parseRuleFile path raw =
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
              mPaths = lookup "paths" fields
              patterns = case mPaths of
                Just p  -> map (T.strip . cleanQuote) (T.splitOn "," p)
                Nothing -> []
              cleanQuote = T.dropAround (\c -> c == '"' || c == '\'' || isSpace c)
          in Just Rule
            { ruleFile     = path
            , rulePatterns = patterns
            , ruleContent  = T.strip (T.unlines bodyLines)
            }
        _ -> Nothing
    _ -> Nothing

-- | Check whether any of the rule's glob patterns matches any of the given active file paths.
ruleMatchesFiles :: Rule -> [FilePath] -> Bool
ruleMatchesFiles Rule{..} fps
  | null rulePatterns = True
  | otherwise = any (\fp -> any (\pat -> matchGlob pat fp) rulePatterns) fps

-- | Recursively resolve @import directives up to a maximum recursion depth (default 4).
-- Imported paths are resolved relative to the importing file, then rejected
-- unless they remain inside 'baseDir' (typically the workspace root).
resolveMemoryImports :: FilePath -> Int -> FilePath -> IO Text
resolveMemoryImports baseDir maxDepth path = do
  absBase <- makeAbsolute baseDir
  absPath <- makeAbsolute path
  pathRes <- resolveWorkspacePath absBase absPath
  case pathRes of
    Left _       -> pure ""
    Right safeFp -> readAndExpand absBase maxDepth safeFp
  where
    -- 'safeFp' has already passed 'resolveWorkspacePath'.
    readAndExpand _ depth _ | depth <= 0 = pure ""
    readAndExpand base depth safeFp = do
      exists <- doesFileExist safeFp
      if not exists
        then pure ""
        else do
          res <- try (BS.readFile safeFp) :: IO (Either SomeException BS.ByteString)
          case res of
            Left _ -> pure ""
            Right bytes -> do
              let txt = TE.decodeUtf8With (\_ _ -> Just ' ') bytes
                  ls  = T.lines txt
              expandedLines <- mapM (processLine base (depth - 1) (takeDirectory safeFp)) ls
              pure (T.unlines (concat expandedLines))

    processLine base depth dir line
      | "@import " `T.isPrefixOf` T.strip line =
          if depth <= 0
            then pure ["[Max @import depth exceeded: " <> line <> "]"]
            else do
              let rawRel = T.strip (T.drop (T.length ("@import " :: T.Text)) (T.strip line))
                  cleanRel = T.unpack (T.dropAround (\c -> c == '"' || c == '\'' || isSpace c) rawRel)
                  targetFp = if isAbsolute cleanRel then cleanRel else dir </> cleanRel
              pathRes <- resolveWorkspacePath base targetFp
              case pathRes of
                Left _ ->
                  pure ["[Import denied: path escapes workspace]"]
                Right safeFp -> do
                  content <- readAndExpand base depth safeFp
                  pure (T.lines content)
      | otherwise = pure [line]

-- | Walk directories from workspace root to cwd, loading AGENTS.md, AGENT.md, or CLAUDE.md.
-- Precedence: AGENTS.md is preferred; then AGENT.md; then CLAUDE.md.
loadHierarchicalMemory :: FilePath -> FilePath -> IO [Text]
loadHierarchicalMemory root cwd = do
  let rel = makeRelative root cwd
      dirs = if rel == "." || null rel then [""] else "" : splitDirectories rel
      candidates = [ foldl (</>) root (take i dirs) | i <- [1 .. length dirs] ]
  contents <- mapM loadDirMemory candidates
  pure (catMaybes contents)
  where
    loadDirMemory dir = firstMemoryFile ["AGENTS.md", "AGENT.md", "CLAUDE.md"]
      where
        firstMemoryFile [] = pure Nothing
        firstMemoryFile (name : names) = do
          let fp = dir </> name
          exists <- doesFileExist fp
          if exists
            then Just <$> resolveMemoryImports root 4 fp
            else firstMemoryFile names

-- | Load rules matching the active files from .claude/rules/*.md and .agents/rules/*.md.
loadRules :: FilePath -> [FilePath] -> IO [Text]
loadRules root activeFiles = do
  claudeRules <- scanRules (root </> ".claude" </> "rules")
  agentRules  <- scanRules (root </> ".agents" </> "rules")
  let allRules = claudeRules ++ agentRules
      matching = filter (`ruleMatchesFiles` activeFiles) allRules
  pure [ ruleContent r | r <- matching ]
  where
    scanRules dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure []
        else do
          entriesRes <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
          case entriesRes of
            Left _ -> pure []
            Right entries -> do
              let mdFiles = [ dir </> e | e <- entries, takeExtension e == ".md" ]
              rules <- mapM loadOne mdFiles
              pure (catMaybes rules)

    loadOne fp = do
      res <- try (BS.readFile fp) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _ -> pure Nothing
        Right bytes ->
          let txt = TE.decodeUtf8With (\_ _ -> Just ' ') bytes
          in pure (parseRuleFile fp txt)

-- | Load full combined memory: global user memory + hierarchical memory + conditional rules.
loadFullMemory :: FilePath -> [FilePath] -> IO Text
loadFullMemory workspace activeFiles = do
  home <- getHomeDirectory
  mGlobalClaude <- checkFile (home </> ".claude" </> "CLAUDE.md")
  mGlobalAgent  <- checkFile (home </> ".agents" </> "AGENT.md")
  mGlobalAgents <- checkFile (home </> ".agents" </> "AGENTS.md")
  let mGlobal = mGlobalClaude `orMaybe` mGlobalAgent `orMaybe` mGlobalAgents

  hierarchical <- loadHierarchicalMemory workspace workspace
  rules <- loadRules workspace activeFiles

  let parts = catMaybes [mGlobal] ++ hierarchical ++ rules
  pure (T.strip (T.intercalate "\n\n" parts))
  where
    checkFile fp = do
      exists <- doesFileExist fp
      if exists
        then Just <$> resolveMemoryImports (takeDirectory fp) 4 fp
        else pure Nothing
    orMaybe (Just a) _ = Just a
    orMaybe Nothing b  = b
