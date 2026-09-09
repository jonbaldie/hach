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
  , isWorktreeDirectory
  , createPullRequest
  ) where

import Hach.Types
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isAlphaNum, isOctDigit, isSpace)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (CreateProcess(cwd), proc, readCreateProcessWithExitCode)

-- | Unquote git C-style quoted path and decode escape sequences.
unquoteGitPath :: Text -> Text
unquoteGitPath t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"' =
      let inner = T.init (T.tail t)
      in decodeGitEscapes inner
  | otherwise = t

-- | Decode git C-style escape sequences (escaped quotes, slashes, whitespace, and octal UTF-8 bytes).
decodeGitEscapes :: Text -> Text
decodeGitEscapes txt =
  let bytes = BS.pack (go (T.unpack txt))
  in case TE.decodeUtf8' bytes of
       Right decoded -> decoded
       Left _        -> txt
  where
    go [] = []
    go ('\\':c:cs) = case c of
      'a'  -> 7  : go cs
      'b'  -> 8  : go cs
      't'  -> 9  : go cs
      'n'  -> 10 : go cs
      'v'  -> 11 : go cs
      'f'  -> 12 : go cs
      'r'  -> 13 : go cs
      '"'  -> 34 : go cs
      '\\' -> 92 : go cs
      o | isOctDigit o ->
          let (octDigits, rest) = takeOct 2 cs
              val = foldl (\acc d -> acc * 8 + fromIntegral (digitToInt d)) (fromIntegral (digitToInt o)) octDigits
          in val : go rest
      other -> BS.unpack (TE.encodeUtf8 (T.singleton other)) ++ go cs
    go (c:cs) =
      BS.unpack (TE.encodeUtf8 (T.singleton c)) ++ go cs

    takeOct :: Int -> String -> (String, String)
    takeOct 0 cs = ([], cs)
    takeOct n (d:cs) | isOctDigit d = let (ds, rest) = takeOct (n - 1) cs in (d:ds, rest)
    takeOct _ cs = ([], cs)

-- | Extract the destination path from a git porcelain status line.
-- For rename/copy status codes (containing 'R' or 'C'), extracts the target path
-- after the " -> " separator, handling quoted paths on either side.
-- For all other status codes, returns the single file path.
extractPath :: Text -> Text -> Text
extractPath st rawPath
  | isRenameOrCopy st =
      let target = case T.uncons rawPath of
            Just ('"', _) ->
              case findClosingQuote (T.unpack rawPath) of
                Just afterOrig ->
                  let rest = T.strip (T.pack afterOrig)
                  in case T.stripPrefix "->" rest of
                       Just dest -> T.strip dest
                       Nothing   -> case T.breakOn " -> " rawPath of
                         (_, d) | not (T.null d) -> T.strip (T.drop 4 d)
                         _                       -> rawPath
                Nothing ->
                  case T.breakOn " -> " rawPath of
                    (_, d) | not (T.null d) -> T.strip (T.drop 4 d)
                    _                       -> rawPath
            _ ->
              case T.breakOn " -> " rawPath of
                (_, d) | not (T.null d) -> T.strip (T.drop 4 d)
                _                       -> rawPath
      in unquoteGitPath (T.strip target)
  | otherwise = unquoteGitPath (T.strip rawPath)
  where
    isRenameOrCopy s = T.any (\c -> c == 'R' || c == 'C') s

    findClosingQuote :: String -> Maybe String
    findClosingQuote ('"':cs) = scan cs
      where
        scan [] = Nothing
        scan ('\\':_:rest) = scan rest
        scan ('"':rest) = Just rest
        scan (_:rest) = scan rest
    findClosingQuote _ = Nothing

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
            rawPath = T.strip (T.drop 2 trimmed)
            fp = T.unpack (extractPath st rawPath)
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
  let stripped = T.strip name
      s = T.unpack stripped
  in name == stripped
     && not (null s)
     && not ("-" `T.isPrefixOf` stripped)
     && not (".." `T.isInfixOf` stripped)
     && not (any (\c -> c == '/' || c == '\\' || c == ':') s)
     && all (\c -> isAlphaNum c || c `elem` ['-', '_', '.']) s

-- | Create a git worktree for a branch or feature name.
createWorktree :: FilePath -> Text -> IO (Either Text FilePath)
createWorktree root name
  | not (isValidWorktreeName name) =
      pure (Left "Invalid worktree name: must be alphanumeric and cannot contain path separators or leading dashes.")
  | otherwise = do
      let targetPath = worktreePath root name
          args = ["worktree", "add", "-b", T.unpack name, targetPath]
      (code, out, err) <- runGit root args
      if code == ExitSuccess
        then pure (Right targetPath)
        else pure (Left ("Failed to create worktree: " <> T.pack (if null err then out else err)))

-- | Check if a directory is an active git worktree (has a .git file pointing to worktrees).
isWorktreeDirectory :: FilePath -> IO Bool
isWorktreeDirectory dir = do
  isFile <- doesFileExist (dir </> ".git")
  if not isFile
    then pure False
    else do
      res <- try (readFile (dir </> ".git")) :: IO (Either SomeException String)
      case res of
        Left _ -> pure False
        Right content -> pure ("gitdir:" `isInfixOf` content && "worktrees" `isInfixOf` content)

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
