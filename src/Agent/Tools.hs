{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.Tools
  ( -- * Tool Definitions
    allToolDefs
  , readFileToolDef
  , writeFileToolDef
  , replaceFileContentToolDef
  , runCommandToolDef
  , listDirToolDef
  , findFilesToolDef
  , grepSearchToolDef

    -- * Argument Parsing
  , ReadFileArgs(..)
  , WriteFileArgs(..)
  , ReplaceFileContentArgs(..)
  , RunCommandArgs(..)
  , ListDirArgs(..)
  , FindFilesArgs(..)
  , GrepSearchArgs(..)
  , parseReadFileArgs
  , parseWriteFileArgs
  , parseReplaceFileContentArgs
  , parseRunCommandArgs
  , parseListDirArgs
  , parseFindFilesArgs
  , parseGrepSearchArgs

    -- * Output Truncation
  , truncateToolOutput

    -- * Execution (IO)
  , executeCodingTool
  , executeReadFile
  , executeWriteFile
  , executeReplaceFileContent
  , executeRunCommand
  , executeListDir
  , executeFindFiles
  , executeGrepSearch
  ) where

import Agent.Types
import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.Aeson
  ( FromJSON(..), (.:), (.:?), (.!=), object, (.=)
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.Exit (ExitCode(..))
import System.FilePath
  ( (</>)
  , isAbsolute
  , isPathSeparator
  , joinPath
  , makeRelative
  , pathSeparator
  , splitDirectories
  , takeDirectory
  , takeFileName
  )
import System.Process (CreateProcess(cwd), readCreateProcessWithExitCode, shell)
import System.Timeout (timeout)

--------------------------------------------------------------------------------
-- Tool Definitions (Schemas)
--------------------------------------------------------------------------------

readFileToolDef :: ToolDef
readFileToolDef = ToolDef
  { toolName = "read_file"
  , toolDescription = "Read the UTF-8 text contents of a file in the workspace."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path of the file to read" :: Text)
              ]
          ]
      , "required" .= (["path"] :: [Text])
      ]
  }

writeFileToolDef :: ToolDef
writeFileToolDef = ToolDef
  { toolName = "write_file"
  , toolDescription = "Write or overwrite a file in the workspace with UTF-8 text content."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path of the file to write" :: Text)
              ]
          , "content" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Full content to write into the file" :: Text)
              ]
          ]
      , "required" .= (["path", "content"] :: [Text])
      ]
  }

replaceFileContentToolDef :: ToolDef
replaceFileContentToolDef = ToolDef
  { toolName = "replace_file_content"
  , toolDescription = "Replace a unique contiguous block of text in a file with new content. Fails if the target content does not match uniquely."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path of the file to modify" :: Text)
              ]
          , "old_content" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Exact contiguous text block to replace" :: Text)
              ]
          , "new_content" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Replacement text" :: Text)
              ]
          ]
      , "required" .= (["path", "old_content", "new_content"] :: [Text])
      ]
  }

runCommandToolDef :: ToolDef
runCommandToolDef = ToolDef
  { toolName = "run_command"
  , toolDescription = "Execute a shell command inside the workspace directory, capturing stdout, stderr, and exit code."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "command" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The shell command to execute" :: Text)
              ]
          ]
      , "required" .= (["command"] :: [Text])
      ]
  }

listDirToolDef :: ToolDef
listDirToolDef = ToolDef
  { toolName = "list_dir"
  , toolDescription = "List files and subdirectories within a given path (defaults to current directory '.')."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Directory path to inspect (defaults to '.')" :: Text)
              ]
          ]
      ]
  }

findFilesToolDef :: ToolDef
findFilesToolDef = ToolDef
  { toolName = "find_files"
  , toolDescription = "Search for files within a directory matching a pattern or substring. Automatically ignores .git, dist-newstyle, and .env."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "pattern" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("File name pattern or substring (e.g. '*.hs', 'Spec.hs', 'README')" :: Text)
              ]
          , "path" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Directory to search (defaults to '.')" :: Text)
              ]
          ]
      , "required" .= (["pattern"] :: [Text])
      ]
  }

grepSearchToolDef :: ToolDef
grepSearchToolDef = ToolDef
  { toolName = "grep_search"
  , toolDescription = "Search file contents for an exact text pattern. Returns matching file paths, line numbers, and line contents."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "query" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Text pattern to search for" :: Text)
              ]
          , "path" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Directory or file to search (defaults to '.')" :: Text)
              ]
          , "case_sensitive" .= object
              [ "type" .= ("boolean" :: Text)
              , "description" .= ("Whether search is case-sensitive (defaults to true)" :: Text)
              ]
          ]
      , "required" .= (["query"] :: [Text])
      ]
  }

-- | Standard set of coding tools exposed to the agent.
allToolDefs :: [ToolDef]
allToolDefs =
  [ readFileToolDef
  , writeFileToolDef
  , replaceFileContentToolDef
  , runCommandToolDef
  , listDirToolDef
  , findFilesToolDef
  , grepSearchToolDef
  ]

--------------------------------------------------------------------------------
-- Argument Types & Parsers
--------------------------------------------------------------------------------

newtype ReadFileArgs = ReadFileArgs { readFilePath :: FilePath }
  deriving (Show, Eq)

instance FromJSON ReadFileArgs where
  parseJSON = Aeson.withObject "ReadFileArgs" $ \o ->
    ReadFileArgs <$> o .: "path"

data WriteFileArgs = WriteFileArgs
  { writeFilePath    :: !FilePath
  , writeFileContent :: !Text
  } deriving (Show, Eq)

instance FromJSON WriteFileArgs where
  parseJSON = Aeson.withObject "WriteFileArgs" $ \o ->
    WriteFileArgs <$> o .: "path" <*> o .: "content"

data ReplaceFileContentArgs = ReplaceFileContentArgs
  { replacePath       :: !FilePath
  , replaceOldContent :: !Text
  , replaceNewContent :: !Text
  } deriving (Show, Eq)

instance FromJSON ReplaceFileContentArgs where
  parseJSON = Aeson.withObject "ReplaceFileContentArgs" $ \o ->
    ReplaceFileContentArgs <$> o .: "path" <*> o .: "old_content" <*> o .: "new_content"

newtype RunCommandArgs = RunCommandArgs { runCommandCmd :: Text }
  deriving (Show, Eq)

instance FromJSON RunCommandArgs where
  parseJSON = Aeson.withObject "RunCommandArgs" $ \o ->
    RunCommandArgs <$> o .: "command"

newtype ListDirArgs = ListDirArgs { listDirPath :: FilePath }
  deriving (Show, Eq)

instance FromJSON ListDirArgs where
  parseJSON = Aeson.withObject "ListDirArgs" $ \o ->
    ListDirArgs <$> o .:? "path" .!= "."

data FindFilesArgs = FindFilesArgs
  { findPattern :: !Text
  , findPath    :: !FilePath
  } deriving (Show, Eq)

instance FromJSON FindFilesArgs where
  parseJSON = Aeson.withObject "FindFilesArgs" $ \o ->
    FindFilesArgs <$> o .: "pattern" <*> o .:? "path" .!= "."

data GrepSearchArgs = GrepSearchArgs
  { grepQuery         :: !Text
  , grepPath          :: !FilePath
  , grepCaseSensitive :: !Bool
  } deriving (Show, Eq)

instance FromJSON GrepSearchArgs where
  parseJSON = Aeson.withObject "GrepSearchArgs" $ \o ->
    GrepSearchArgs <$> o .: "query" <*> o .:? "path" .!= "." <*> o .:? "case_sensitive" .!= True

parseArgsWith :: (FromJSON a) => ToolCall -> Either String a
parseArgsWith tc = do
  val <- parseCallArgs tc
  AesonTypes.parseEither parseJSON val

parseReadFileArgs :: ToolCall -> Either String ReadFileArgs
parseReadFileArgs = parseArgsWith

parseWriteFileArgs :: ToolCall -> Either String WriteFileArgs
parseWriteFileArgs = parseArgsWith

parseReplaceFileContentArgs :: ToolCall -> Either String ReplaceFileContentArgs
parseReplaceFileContentArgs = parseArgsWith

parseRunCommandArgs :: ToolCall -> Either String RunCommandArgs
parseRunCommandArgs = parseArgsWith

parseListDirArgs :: ToolCall -> Either String ListDirArgs
parseListDirArgs = parseArgsWith

parseFindFilesArgs :: ToolCall -> Either String FindFilesArgs
parseFindFilesArgs = parseArgsWith

parseGrepSearchArgs :: ToolCall -> Either String GrepSearchArgs
parseGrepSearchArgs = parseArgsWith

--------------------------------------------------------------------------------
-- Output Truncation
--------------------------------------------------------------------------------

-- | Truncate excessive tool output (capped at 30,000 characters and 1,000 lines).
truncateToolOutput :: Text -> Text
truncateToolOutput raw
  | T.length raw > maxChars || length (T.lines raw) > maxLines =
      let lineSubset = take maxLines (T.lines raw)
          charSubset = T.take maxChars (T.unlines lineSubset)
          truncatedNotice = "\n\n[Output truncated: showing "
            <> T.pack (show (length (T.lines charSubset)))
            <> " lines / "
            <> T.pack (show (T.length charSubset))
            <> " characters]"
      in charSubset <> truncatedNotice
  | otherwise = raw
  where
    maxChars = 30000
    maxLines = 1000

--------------------------------------------------------------------------------
-- Tool Execution against Workspace (IO)
--------------------------------------------------------------------------------

-- | Execute any supported tool within the given workspace directory.
executeCodingTool :: FilePath -> ToolCall -> IO ToolResult
executeCodingTool root call = do
  res <- case functionName call of
    "read_file" ->
      case parseReadFileArgs call of
        Left err   -> pure $ ToolError ("Failed to parse read_file args: " <> T.pack err)
        Right args -> executeReadFile root args

    "write_file" ->
      case parseWriteFileArgs call of
        Left err   -> pure $ ToolError ("Failed to parse write_file args: " <> T.pack err)
        Right args -> executeWriteFile root args

    "replace_file_content" ->
      case parseReplaceFileContentArgs call of
        Left err   -> pure $ ToolError ("Failed to parse replace_file_content args: " <> T.pack err)
        Right args -> executeReplaceFileContent root args

    "run_command" ->
      case parseRunCommandArgs call of
        Left err   -> pure $ ToolError ("Failed to parse run_command args: " <> T.pack err)
        Right args -> executeRunCommand root args

    "list_dir" ->
      case parseListDirArgs call of
        Left err   -> pure $ ToolError ("Failed to parse list_dir args: " <> T.pack err)
        Right args -> executeListDir root args

    "find_files" ->
      case parseFindFilesArgs call of
        Left err   -> pure $ ToolError ("Failed to parse find_files args: " <> T.pack err)
        Right args -> executeFindFiles root args

    "grep_search" ->
      case parseGrepSearchArgs call of
        Left err   -> pure $ ToolError ("Failed to parse grep_search args: " <> T.pack err)
        Right args -> executeGrepSearch root args

    unknown ->
      pure $ ToolError ("Unknown tool function: " <> unknown)
  pure $ case res of
    ToolSuccess out -> ToolSuccess (truncateToolOutput out)
    err             -> err

-- | Logically collapses '.' and '..' components in an absolute path.
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
  let rootWithSep = if isPathSeparator (last rootCanon) then rootCanon else rootCanon ++ [pathSeparator]
  if finalPath == rootCanon || (rootWithSep `isPrefixOf` finalPath)
    then pure (Right finalPath)
    else pure (Left ("Access denied: path '" <> rawPath <> "' escapes the workspace root."))

executeReadFile :: FilePath -> ReadFileArgs -> IO ToolResult
executeReadFile root (ReadFileArgs path) = do
  pathRes <- resolveWorkspacePath root path
  case pathRes of
    Left err -> pure $ ToolError (T.pack err)
    Right fullPath -> do
      exists <- doesFileExist fullPath
      if not exists
        then pure $ ToolError ("File not found: " <> T.pack path)
        else do
          res <- try (BS.readFile fullPath) :: IO (Either SomeException BS.ByteString)
          case res of
            Left ex -> pure $ ToolError ("Read error: " <> T.pack (show ex))
            Right bytes ->
              let txt = TE.decodeUtf8With TE.lenientDecode bytes
              in pure $ ToolSuccess txt

executeWriteFile :: FilePath -> WriteFileArgs -> IO ToolResult
executeWriteFile root (WriteFileArgs path content) = do
  pathRes <- resolveWorkspacePath root path
  case pathRes of
    Left err -> pure $ ToolError (T.pack err)
    Right fullPath -> do
      res <- try $ do
        createDirectoryIfMissing True (takeDirectory fullPath)
        BS.writeFile fullPath (TE.encodeUtf8 content)
      case res of
        Left (ex :: SomeException) ->
          pure $ ToolError ("Write error: " <> T.pack (show ex))
        Right () ->
          pure $ ToolSuccess ("Successfully wrote " <> T.pack (show (T.length content)) <> " characters to " <> T.pack path)

executeReplaceFileContent :: FilePath -> ReplaceFileContentArgs -> IO ToolResult
executeReplaceFileContent root (ReplaceFileContentArgs path oldContent newContent) = do
  pathRes <- resolveWorkspacePath root path
  case pathRes of
    Left err -> pure $ ToolError (T.pack err)
    Right fullPath -> do
      exists <- doesFileExist fullPath
      if not exists
        then pure $ ToolError ("File not found: " <> T.pack path)
        else do
          readRes <- try (BS.readFile fullPath) :: IO (Either SomeException BS.ByteString)
          case readRes of
            Left ex -> pure $ ToolError ("Read error: " <> T.pack (show ex))
            Right bytes -> do
              let txt = TE.decodeUtf8With TE.lenientDecode bytes
                  matches = T.count oldContent txt
              if matches == 0
                then pure $ ToolError ("Target content not found in '" <> T.pack path <> "'.")
                else if matches > 1
                  then pure $ ToolError ("Target content found multiple (" <> T.pack (show matches) <> ") times in '" <> T.pack path <> "'; replacement requires a unique match.")
                  else do
                    let updated = T.replace oldContent newContent txt
                    writeRes <- try (BS.writeFile fullPath (TE.encodeUtf8 updated)) :: IO (Either SomeException ())
                    case writeRes of
                      Left ex -> pure $ ToolError ("Write error: " <> T.pack (show ex))
                      Right () -> pure $ ToolSuccess ("Successfully replaced content in " <> T.pack path <> ".")

executeRunCommand :: FilePath -> RunCommandArgs -> IO ToolResult
executeRunCommand root (RunCommandArgs cmd) = do
  let sh = (shell (T.unpack cmd)) { cwd = Just root }
  -- 60 second timeout to prevent runaway or interactive processes from hanging the harness
  res <- try (timeout (60 * 1000000) (readCreateProcessWithExitCode sh "")) :: IO (Either SomeException (Maybe (ExitCode, String, String)))
  case res of
    Left ex -> pure $ ToolError ("Process execution failed: " <> T.pack (show ex))
    Right Nothing ->
      pure $ ToolError ("Command timed out after 60 seconds: " <> cmd)
    Right (Just (exitCode, stdoutStr, stderrStr)) ->
      let codeInt = case exitCode of
            ExitSuccess   -> 0
            ExitFailure c -> c
          outTxt = T.pack stdoutStr
          errTxt = T.pack stderrStr
          summary = T.unlines
            [ "Exit Code: " <> T.pack (show codeInt)
            , "STDOUT:\n" <> if T.null outTxt then "(empty)" else outTxt
            , "STDERR:\n" <> if T.null errTxt then "(empty)" else errTxt
            ]
      in pure $ ToolSuccess summary

executeListDir :: FilePath -> ListDirArgs -> IO ToolResult
executeListDir root (ListDirArgs path) = do
  pathRes <- resolveWorkspacePath root path
  case pathRes of
    Left err -> pure $ ToolError (T.pack err)
    Right fullPath -> do
      dirExists <- doesDirectoryExist fullPath
      if not dirExists
        then pure $ ToolError ("Directory does not exist: " <> T.pack path)
        else do
          res <- try (listDirectory fullPath) :: IO (Either SomeException [FilePath])
          case res of
            Left ex -> pure $ ToolError ("List directory error: " <> T.pack (show ex))
            Right entries ->
              pure $ ToolSuccess (T.unlines (map T.pack entries))

executeFindFiles :: FilePath -> FindFilesArgs -> IO ToolResult
executeFindFiles root (FindFilesArgs pat searchPath) = do
  pathRes <- resolveWorkspacePath root searchPath
  case pathRes of
    Left err -> pure $ ToolError (T.pack err)
    Right startDir -> do
      dirExists <- doesDirectoryExist startDir
      if not dirExists
        then pure $ ToolError ("Directory not found: " <> T.pack searchPath)
        else do
          canonRoot <- canonicalizePath root
          files <- traverseDir canonRoot startDir
          let matches = filter (matchPattern pat) files
              limited = take 100 matches
              resText = if null limited
                then "No matching files found."
                else T.unlines (map T.pack limited)
          pure $ ToolSuccess resText
  where
    ignoredDirs = [".git", "dist-newstyle", ".env", ".cabal-sandbox", "node_modules"]

    traverseDir canonRoot current = do
      entriesRes <- try (listDirectory current) :: IO (Either SomeException [FilePath])
      case entriesRes of
        Left _ -> pure []
        Right entries -> do
          let validEntries = filter (`notElem` ignoredDirs) entries
          subResults <- forM validEntries $ \entry -> do
            let full = current </> entry
            isDir <- doesDirectoryExist full
            if isDir
              then traverseDir canonRoot full
              else do
                let rel = makeRelative canonRoot full
                pure [rel]
          pure (concat subResults)

    matchPattern p fp =
      let name = T.pack (takeFileName fp)
          full = T.pack fp
      in if "*" `T.isInfixOf` p
           then let parts = filter (not . T.null) (T.splitOn "*" p)
                in all (`T.isInfixOf` name) parts || all (`T.isInfixOf` full) parts
           else p `T.isInfixOf` name || p `T.isInfixOf` full

executeGrepSearch :: FilePath -> GrepSearchArgs -> IO ToolResult
executeGrepSearch root (GrepSearchArgs query searchPath caseSensitive) = do
  pathRes <- resolveWorkspacePath root searchPath
  case pathRes of
    Left err -> pure $ ToolError (T.pack err)
    Right startPath -> do
      isDir <- doesDirectoryExist startPath
      isFile <- doesFileExist startPath
      canonRoot <- canonicalizePath root
      if isFile
        then do
          let rel = makeRelative canonRoot startPath
          matches <- grepInFile rel startPath
          pure $ ToolSuccess (if null matches then "No matches found." else T.unlines matches)
        else if isDir
          then do
            files <- collectFiles canonRoot startPath
            matches <- forM files $ \(rel, full) -> grepInFile rel full
            let allMatches = concat matches
                limited = take 100 allMatches
            pure $ ToolSuccess (if null limited then "No matches found." else T.unlines limited)
          else
            pure $ ToolError ("Path does not exist: " <> T.pack searchPath)
  where
    ignoredDirs = [".git", "dist-newstyle", ".env", ".cabal-sandbox", "node_modules"]

    collectFiles canonRoot current = do
      entriesRes <- try (listDirectory current) :: IO (Either SomeException [FilePath])
      case entriesRes of
        Left _ -> pure []
        Right entries -> do
          let valid = filter (`notElem` ignoredDirs) entries
          subResults <- forM valid $ \entry -> do
            let full = current </> entry
            isDir <- doesDirectoryExist full
            if isDir
              then collectFiles canonRoot full
              else do
                let rel = makeRelative canonRoot full
                pure [(rel, full)]
          pure (concat subResults)

    grepInFile rel full = do
      res <- try (BS.readFile full) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _ -> pure []
        Right bytes ->
          if BS.any (== 0) (BS.take 1024 bytes)
            then pure []
            else do
              let txt = TE.decodeUtf8With (\_ _ -> Just ' ') bytes
                  ls = zip [1 :: Int ..] (T.lines txt)
                  checkLine (_, line) =
                    if caseSensitive
                      then query `T.isInfixOf` line
                      else T.toLower query `T.isInfixOf` T.toLower line
                  matching = filter checkLine ls
              pure [ T.pack rel <> ":" <> T.pack (show lineNum) <> ": " <> line | (lineNum, line) <- matching ]
