-- | Workspace-relative path containment.
--
-- Shared by the tool runtime, memory @import resolver, and skill
-- {{file:}} expander so every reader uses the same traversal check.
module Hach.Paths
  ( Workspace(..)
  , workspaceAt
  , workspaceRoots
  , relativeToWorkspace
  , collapseLogicalPath
  , canonicalizeCandidate
   , resolveWorkspacePath
   , isProtectedPath
   , matchStarGlob
  ) where

import Data.List (isPrefixOf, maximumBy, nub)
import Data.Ord (comparing)
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
  , makeRelative
  , splitDirectories
  , takeDirectory
  , takeFileName
  )

-- | The directories a tool call is allowed to reach: the primary workspace
-- root plus any additional roots named by @--add-dir@ or the
-- @working_directories@ setting. Relative paths always resolve against the
-- primary root; additional roots only widen the containment check.
data Workspace = Workspace
  { wsRoot       :: !FilePath
  , wsExtraRoots :: ![FilePath]
  } deriving (Show, Eq)

-- | A workspace confined to a single root.
workspaceAt :: FilePath -> Workspace
workspaceAt root = Workspace root []

-- | Canonical spellings of every root, primary first, duplicates dropped.
workspaceRoots :: Workspace -> IO [FilePath]
workspaceRoots (Workspace root extras) =
  nub <$> mapM canonicalizeCandidate (root : extras)

-- | Express a path relative to whichever workspace root contains it, so
-- callers see the same spelling regardless of which root it came from. A
-- path under no root is returned unchanged.
relativeToWorkspace :: Workspace -> FilePath -> IO FilePath
relativeToWorkspace ws path = do
  roots <- workspaceRoots ws
  pure (relativeToRoots roots path)

-- | Pure core of 'relativeToWorkspace' over already-canonical roots. Nested
-- roots are resolved in favour of the deepest one that contains the path, so
-- an added directory inside the primary root still names its own contents.
relativeToRoots :: [FilePath] -> FilePath -> FilePath
relativeToRoots roots path =
  case filter (`isUnderRootCanon` path) roots of
    []         -> path
    containing -> makeRelative (maximumBy (comparing length) containing) path

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

-- | Resolve a target path against the workspace.
-- Enforces that the resolved path is strictly located within one of the
-- workspace roots, preventing directory traversal attacks via '..' or
-- absolute paths.
resolveWorkspacePath :: Workspace -> FilePath -> IO (Either String FilePath)
resolveWorkspacePath ws rawPath = do
  rootCanon <- canonicalizeCandidate (wsRoot ws)
  roots <- workspaceRoots ws
  let candidate = if isAbsolute rawPath
                    then rawPath
                    else rootCanon </> rawPath
      collapsed = collapseLogicalPath candidate
  finalPath <- canonicalizeCandidate collapsed
  if any (`isUnderRootCanon` finalPath) roots
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
