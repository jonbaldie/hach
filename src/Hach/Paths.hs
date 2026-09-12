-- | Workspace-relative path containment.
--
-- Shared by the tool runtime, memory @import resolver, and skill
-- {{file:}} expander so every reader uses the same traversal check.
module Hach.Paths
  ( collapseLogicalPath
  , canonicalizeCandidate
   , resolveWorkspacePath
   , isProtectedPath
   , matchStarGlob
  ) where

import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  )
import System.FilePath
  ( (</>)
  , addTrailingPathSeparator
  , isAbsolute
  , joinPath
  , splitDirectories
  , takeDirectory
  , takeFileName
  )

-- | Logically collapses '.' and '..' components in a path.
collapseLogicalPath :: FilePath -> FilePath
collapseLogicalPath p =
  let dirs = splitDirectories p
      step acc d
        | d == "." || d == "./" || d == ".\\" = acc
        | d == ".." || d == "../" || d == "..\\" = case acc of
            [] -> []
            ["/"] -> ["/"]
            (_:xs) -> xs
        | otherwise = d : acc
  in joinPath (reverse (foldl step [] dirs))

-- | Canonicalize an existing path or the deepest existing parent directory
-- of a non-existing path. This resolves symlinks while preserving target filename.
canonicalizeCandidate :: FilePath -> IO FilePath
canonicalizeCandidate path = do
  existsFile <- doesFileExist path
  existsDir  <- doesDirectoryExist path
  if existsFile || existsDir
    then canonicalizePath path
    else do
      let parent = takeDirectory path
      if parent == path
        then pure path
        else do
          canonParent <- canonicalizeCandidate parent
          pure (canonParent </> takeFileName path)

-- | Resolve a target path against the workspace root.
-- Enforces that the resolved path is strictly located within the workspace root,
-- preventing directory traversal attacks via '..' or absolute paths.
resolveWorkspacePath :: FilePath -> FilePath -> IO (Either String FilePath)
resolveWorkspacePath root rawPath = do
  rootCanon <- canonicalizePath root
  let candidate = if isAbsolute rawPath
                    then rawPath
                    else rootCanon </> rawPath
      collapsed = collapseLogicalPath candidate
  finalPath <- canonicalizeCandidate collapsed
  if isUnderRootCanon rootCanon finalPath
    then pure (Right finalPath)
    else pure (Left ("Access denied: path '" <> rawPath <> "' escapes the workspace root."))

-- | True when 'finalPath' sits on or under 'rootCanon'.
isUnderRootCanon :: FilePath -> FilePath -> Bool
isUnderRootCanon rootCanon finalPath =
  let rootWithSep = addTrailingPathSeparator rootCanon
  in finalPath == rootCanon || rootWithSep `isPrefixOf` finalPath

isProtectedPath :: FilePath -> Bool
isProtectedPath fp =
  let dirs = splitDirectories fp
      cleanDirs = [ d | d <- dirs, d /= "." && d /= "./" ]
  in any (`elem` [".git", ".claude", ".agents", ".agent"]) cleanDirs

-- | Linear wildcard matching where '*' may span directory separators.
matchStarGlob :: Text -> Text -> Bool
matchStarGlob pat str = go pat str Nothing
  where
    go p s star
      | Just p' <- T.stripPrefix "*" p = go p' s (Just (p', s))
      | Just (pc, p') <- T.uncons p, Just (sc, s') <- T.uncons s, pc == sc = go p' s' star
      | Just (p', s0) <- star, not (T.null s), Just (_, s') <- T.uncons s0 = go p' s' (Just (p', s'))
      | T.null s = T.all (== '*') p
      | otherwise = False
