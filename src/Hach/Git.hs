{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Git
  ( appendCoAuthor
  , parsePorcelainStatus
  , worktreePath
  , getGitStatus
  , getGitDiff
  , createWorktree
  , removeWorktree
  , isValidWorktreeName
  , createPullRequest
  ) where

import Hach.Types
import Control.Exception (SomeException, try)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (CreateProcess(cwd), proc, readCreateProcessWithExitCode)

-- | Append Co-Authored-By attribution trailer to commit message if not already present.
appendCoAuthor :: Text -> Text -> Text
appendCoAuthor msg author =
  let trailer = "Co-Authored-By: " <> author
  in if trailer `T.isInfixOf` msg
       then msg
       else if T.null (T.strip msg)
         then trailer
         else T.stripEnd msg <> "\n\n" <> trailer

-- | Compute isolated worktree directory path under .agents/worktrees/<name>.
worktreePath :: FilePath -> Text -> FilePath
worktreePath root name = root </> ".agents" </> "worktrees" </> T.unpack name

-- | Parse git status --porcelain=v1 output into GitStatusInfo.
-- Splits upstream tracking on "..." rather than "." so branch names with version dots
-- (e.g. "release-1.0", "v2.0") are not prematurely truncated.
parsePorcelainStatus :: Text -> Text -> GitStatusInfo
parsePorcelainStatus defaultBranch raw =
  let ls = T.lines raw
      (mBranch, fileLines) = case ls of
        (firstLine : rest) | "## " `T.isPrefixOf` firstLine ->
            let header = T.drop 3 firstLine
                bName = case T.breakOn "..." header of
                  (b, _) | not (T.null b) -> T.takeWhile (not . isSpace) b
                  _                       -> T.takeWhile (not . isSpace) header
                cleanB = if T.null bName || bName == "HEAD" || "No commits yet" `T.isPrefixOf` header
                           then defaultBranch
                           else bName
            in (cleanB, rest)
        _ -> (defaultBranch, ls)

      parseLine l =
        let trimmed = l
            st = T.take 2 trimmed
            fp = T.unpack (T.strip (T.drop 2 trimmed))
        in if T.null (T.strip (T.pack fp))
             then Nothing
             else Just (st, fp)

      entries = [ e | l <- fileLines, Just e <- [parseLine l] ]
      untracked = [ fp | (st, fp) <- entries, st == "??" ]
      modified  = [ fp | (st, fp) <- entries, st /= "??" ]
      isClean   = null untracked && null modified
  in GitStatusInfo
    { gsiBranch    = mBranch
    , gsiClean     = isClean
    , gsiModified  = modified
    , gsiUntracked = untracked
    }

-- | Run git command with direct argument vector (proc) to avoid shell injection.
runGit :: FilePath -> [String] -> IO (ExitCode, String, String)
runGit root args = do
  let procSpec = (proc "git" args) { cwd = Just root }
  res <- try (readCreateProcessWithExitCode procSpec "") :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Left ex -> pure (ExitFailure 1, "", show ex)
    Right (code, out, err) -> pure (code, out, err)

-- | Fetch current git status.
getGitStatus :: FilePath -> IO GitStatusInfo
getGitStatus root = do
  (code, out, _) <- runGit root ["status", "--porcelain=v1", "-b"]
  if code == ExitSuccess
    then pure (parsePorcelainStatus "HEAD" (T.pack out))
    else pure (GitStatusInfo "unknown" False [] [])

-- | Fetch current git diff.
getGitDiff :: FilePath -> IO Text
getGitDiff root = do
  (code, out, err) <- runGit root ["diff", "HEAD"]
  if code == ExitSuccess
    then pure (T.pack out)
    else pure ("Git diff error: " <> T.pack err)

-- | Validate that a worktree name is safe (alphanumeric, no traversal, no leading dashes, no path separators).
isValidWorktreeName :: Text -> Bool
isValidWorktreeName name =
  let s = T.unpack (T.strip name)
  in not (null s)
     && not ("-" `isPrefixOfText` name)
     && not (".." `isSubstringOfText` name)
     && not (any (\c -> c == '/' || c == '\\' || c == ':') s)
     && all (\c -> isAlphaNum c || c `elem` ['-', '_', '.']) s
  where
    isPrefixOfText p t = p `T.isPrefixOf` t
    isSubstringOfText sub t = sub `T.isInfixOf` t

-- | Create a git worktree for a branch or feature name.
createWorktree :: FilePath -> Text -> IO (Either Text FilePath)
createWorktree root name
  | not (isValidWorktreeName name) =
      pure (Left "Invalid worktree name: must be alphanumeric and cannot contain path separators or leading dashes.")
  | otherwise = do
      let targetPath = worktreePath root name
          args = ["worktree", "add", "-B", T.unpack name, targetPath]
      (code, out, err) <- runGit root args
      if code == ExitSuccess
        then pure (Right targetPath)
        else pure (Left ("Failed to create worktree: " <> T.pack (if null err then out else err)))

-- | Remove an active git worktree.
removeWorktree :: FilePath -> Text -> IO (Either Text ())
removeWorktree root name
  | not (isValidWorktreeName name) =
      pure (Left "Invalid worktree name: must be alphanumeric and cannot contain path separators or leading dashes.")
  | otherwise = do
      let targetPath = worktreePath root name
          args = ["worktree", "remove", "--force", targetPath]
      (code, out, err) <- runGit root args
      if code == ExitSuccess
        then pure (Right ())
        else pure (Left ("Failed to remove worktree: " <> T.pack (if null err then out else err)))

-- | Create a PR via gh CLI using direct argument vector.
createPullRequest :: FilePath -> Text -> Text -> IO (Either Text Text)
createPullRequest root title body = do
  let procSpec = (proc "gh" ["pr", "create", "--title", T.unpack title, "--body", T.unpack body]) { cwd = Just root }
  res <- try (readCreateProcessWithExitCode procSpec "") :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Left ex -> pure (Left ("PR creation failed: " <> T.pack (show ex)))
    Right (ExitSuccess, out, _) -> pure (Right (T.strip (T.pack out)))
    Right (ExitFailure _, out, err) -> pure (Left ("gh pr create error: " <> T.pack (if null err then out else err)))
