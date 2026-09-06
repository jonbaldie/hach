{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.Git
  ( appendCoAuthor
  , parsePorcelainStatus
  , worktreePath
  , getGitStatus
  , getGitDiff
  , createWorktree
  , removeWorktree
  , createPullRequest
  ) where

import Agent.Types
import Control.Exception (SomeException, try)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (CreateProcess(cwd), readCreateProcessWithExitCode, shell)

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
parsePorcelainStatus :: Text -> Text -> GitStatusInfo
parsePorcelainStatus defaultBranch raw =
  let ls = T.lines raw
      (mBranch, fileLines) = case ls of
        (firstLine : rest) | "## " `T.isPrefixOf` firstLine ->
            let bName = T.takeWhile (\c -> c /= '.' && not (isSpace c)) (T.drop 3 firstLine)
            in (if T.null bName then defaultBranch else bName, rest)
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

-- | Run git command in directory.
runGit :: FilePath -> String -> IO (ExitCode, String, String)
runGit root cmd = do
  let procSpec = (shell ("git " ++ cmd)) { cwd = Just root }
  res <- try (readCreateProcessWithExitCode procSpec "") :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Left ex -> pure (ExitFailure 1, "", show ex)
    Right (code, out, err) -> pure (code, out, err)

-- | Fetch current git status.
getGitStatus :: FilePath -> IO GitStatusInfo
getGitStatus root = do
  (code, out, _) <- runGit root "status --porcelain=v1 -b"
  if code == ExitSuccess
    then pure (parsePorcelainStatus "HEAD" (T.pack out))
    else pure (GitStatusInfo "unknown" False [] [])

-- | Fetch current git diff.
getGitDiff :: FilePath -> IO Text
getGitDiff root = do
  (code, out, err) <- runGit root "diff HEAD"
  if code == ExitSuccess
    then pure (T.pack out)
    else pure ("Git diff error: " <> T.pack err)

-- | Create a git worktree for a branch or feature name.
createWorktree :: FilePath -> Text -> IO (Either Text FilePath)
createWorktree root name = do
  let targetPath = worktreePath root name
      cmd = "worktree add -B " ++ T.unpack name ++ " " ++ targetPath
  (code, out, err) <- runGit root cmd
  if code == ExitSuccess
    then pure (Right targetPath)
    else pure (Left ("Failed to create worktree: " <> T.pack (if null err then out else err)))

-- | Remove an active git worktree.
removeWorktree :: FilePath -> Text -> IO (Either Text ())
removeWorktree root name = do
  let targetPath = worktreePath root name
      cmd = "worktree remove --force " ++ targetPath
  (code, out, err) <- runGit root cmd
  if code == ExitSuccess
    then pure (Right ())
    else pure (Left ("Failed to remove worktree: " <> T.pack (if null err then out else err)))

-- | Create a PR via gh CLI.
createPullRequest :: FilePath -> Text -> Text -> IO (Either Text Text)
createPullRequest root title body = do
  let cmd = "gh pr create --title " ++ show (T.unpack title) ++ " --body " ++ show (T.unpack body)
      procSpec = (shell cmd) { cwd = Just root }
  res <- try (readCreateProcessWithExitCode procSpec "") :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Left ex -> pure (Left ("PR creation failed: " <> T.pack (show ex)))
    Right (ExitSuccess, out, _) -> pure (Right (T.strip (T.pack out)))
    Right (ExitFailure _, out, err) -> pure (Left ("gh pr create error: " <> T.pack (if null err then out else err)))
