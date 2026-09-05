{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.Tools
  ( -- * Tool Definitions
    allToolDefs
  , readFileToolDef
  , writeFileToolDef
  , runCommandToolDef
  , listDirToolDef

    -- * Argument Parsing
  , ReadFileArgs(..)
  , WriteFileArgs(..)
  , RunCommandArgs(..)
  , ListDirArgs(..)
  , parseReadFileArgs
  , parseWriteFileArgs
  , parseRunCommandArgs
  , parseListDirArgs

    -- * Execution (IO)
  , executeCodingTool
  , executeReadFile
  , executeWriteFile
  , executeRunCommand
  , executeListDir
  ) where

import Agent.Types
import Control.Exception (SomeException, try)
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
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Process (CreateProcess(cwd), readCreateProcessWithExitCode, shell)

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

-- | Standard set of coding tools exposed to the agent.
allToolDefs :: [ToolDef]
allToolDefs = [readFileToolDef, writeFileToolDef, runCommandToolDef, listDirToolDef]

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

parseArgsWith :: (FromJSON a) => ToolCall -> Either String a
parseArgsWith tc = do
  val <- parseCallArgs tc
  AesonTypes.parseEither parseJSON val

parseReadFileArgs :: ToolCall -> Either String ReadFileArgs
parseReadFileArgs = parseArgsWith

parseWriteFileArgs :: ToolCall -> Either String WriteFileArgs
parseWriteFileArgs = parseArgsWith

parseRunCommandArgs :: ToolCall -> Either String RunCommandArgs
parseRunCommandArgs = parseArgsWith

parseListDirArgs :: ToolCall -> Either String ListDirArgs
parseListDirArgs = parseArgsWith

--------------------------------------------------------------------------------
-- Tool Execution against Workspace (IO)
--------------------------------------------------------------------------------

-- | Execute any supported tool within the given workspace directory.
executeCodingTool :: FilePath -> ToolCall -> IO ToolResult
executeCodingTool root call =
  case functionName call of
    "read_file" ->
      case parseReadFileArgs call of
        Left err   -> pure $ ToolError ("Failed to parse read_file args: " <> T.pack err)
        Right args -> executeReadFile root args

    "write_file" ->
      case parseWriteFileArgs call of
        Left err   -> pure $ ToolError ("Failed to parse write_file args: " <> T.pack err)
        Right args -> executeWriteFile root args

    "run_command" ->
      case parseRunCommandArgs call of
        Left err   -> pure $ ToolError ("Failed to parse run_command args: " <> T.pack err)
        Right args -> executeRunCommand root args

    "list_dir" ->
      case parseListDirArgs call of
        Left err   -> pure $ ToolError ("Failed to parse list_dir args: " <> T.pack err)
        Right args -> executeListDir root args

    unknown ->
      pure $ ToolError ("Unknown tool function: " <> unknown)

executeReadFile :: FilePath -> ReadFileArgs -> IO ToolResult
executeReadFile root (ReadFileArgs path) = do
  let fullPath = root </> path
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
  let fullPath = root </> path
  res <- try $ do
    createDirectoryIfMissing True (takeDirectory fullPath)
    BS.writeFile fullPath (TE.encodeUtf8 content)
  case res of
    Left (ex :: SomeException) ->
      pure $ ToolError ("Write error: " <> T.pack (show ex))
    Right () ->
      pure $ ToolSuccess ("Successfully wrote " <> T.pack (show (T.length content)) <> " characters to " <> T.pack path)

executeRunCommand :: FilePath -> RunCommandArgs -> IO ToolResult
executeRunCommand root (RunCommandArgs cmd) = do
  let sh = (shell (T.unpack cmd)) { cwd = Just root }
  res <- try (readCreateProcessWithExitCode sh "") :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Left ex -> pure $ ToolError ("Process execution failed: " <> T.pack (show ex))
    Right (exitCode, stdoutStr, stderrStr) ->
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
  let fullPath = root </> path
  dirExists <- doesDirectoryExist fullPath
  if not dirExists
    then pure $ ToolError ("Directory does not exist: " <> T.pack path)
    else do
      res <- try (listDirectory fullPath) :: IO (Either SomeException [FilePath])
      case res of
        Left ex -> pure $ ToolError ("List directory error: " <> T.pack (show ex))
        Right entries ->
          pure $ ToolSuccess (T.unlines (map T.pack entries))
