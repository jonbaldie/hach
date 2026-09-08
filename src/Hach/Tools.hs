{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Tools
  ( -- * Tool Definitions
    allToolDefs
  , readFileToolDef
  , writeFileToolDef
  , replaceFileContentToolDef
  , editToolDef
  , runCommandToolDef
  , bashToolDef
  , listDirToolDef
  , findFilesToolDef
  , globToolDef
  , grepSearchToolDef
  , grepToolDef
  , webFetchToolDef
  , webSearchToolDef
  , agentToolDef
  , todoWriteToolDef
  , skillToolDef
  , enterPlanModeToolDef
  , exitPlanModeToolDef
  , enterWorktreeToolDef
  , exitWorktreeToolDef
  , listAgentsToolDef
  , sendMessageToolDef
  , pushNotificationToolDef
  , monitorToolDef
  , taskCreateToolDef
  , taskGetToolDef
  , taskListToolDef
  , taskUpdateToolDef
  , taskStopToolDef
  , askUserQuestionToolDef
  , endConversationToolDef

    -- * Argument Parsing
  , ReadFileArgs(..)
  , WriteFileArgs(..)
  , ReplaceFileContentArgs(..)
  , EditArgs(..)
  , RunCommandArgs(..)
  , BashArgs(..)
  , ListDirArgs(..)
  , FindFilesArgs(..)
  , GlobArgs(..)
  , GrepSearchArgs(..)
  , GrepArgs(..)
  , WebFetchArgs(..)
  , WebSearchArgs(..)
  , AgentArgs(..)
  , TodoWriteArgs(..)
  , SkillToolArgs(..)
  , EnterWorktreeArgs(..)
  , SendMessageArgs(..)
  , PushNotificationArgs(..)
  , MonitorArgs(..)
  , TaskCreateArgs(..)
  , TaskGetArgs(..)
  , TaskUpdateArgs(..)
  , TaskStopArgs(..)
  , AskUserQuestionArgs(..)

  , parseReadFileArgs
  , parseWriteFileArgs
  , parseReplaceFileContentArgs
  , parseEditArgs
  , parseRunCommandArgs
  , parseBashArgs
  , parseListDirArgs
  , parseFindFilesArgs
  , parseGlobArgs
  , parseGrepSearchArgs
  , parseGrepArgs
  , parseWebFetchArgs
  , parseWebSearchArgs
  , parseAgentArgs
  , parseTodoWriteArgs
  , parseSkillToolArgs
  , parseEnterWorktreeArgs
  , parseSendMessageArgs
  , parsePushNotificationArgs
  , parseMonitorArgs
  , parseTaskCreateArgs
  , parseTaskGetArgs
  , parseTaskUpdateArgs
  , parseTaskStopArgs
  , parseAskUserQuestionArgs

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
  , matchPattern
  , executeGrepSearch
  , executeWebFetch
  , executeWebSearch
  , executeTodoWrite
  , executePushNotification
  , executeEnterWorktree
  , executeExitWorktree
  , executeSkill
  , executeTaskCreate
  , executeTaskGet
  , executeTaskList
  , executeTaskUpdate
  , executeTaskStop
  , executeMonitor
  , executeAskUserQuestion
  ) where

import Hach.Git (createWorktree, isWorktreeDirectory)
import Hach.Notifications (sendDesktopNotification)
import Hach.Paths (resolveWorkspacePath)
import Hach.Skills (discoverSkills, injectDynamicContext, skillContent, substituteArguments)
import Hach.Tasks
  ( Task(..)
  , TaskStore
  , emptyTaskStore
  , createTask
  , createTaskWithId
  , getTask
  , listTasks
  , updateTask
  , formatTaskList
  , BackgroundRegistry
  , newBackgroundRegistry
  , spawnBackgroundProcess
  , getBackgroundOutput
  , stopBackgroundProcess
  )
import Hach.Permissions (isProtectedPath, matchStarGlob)
import Hach.Types
import Control.Applicative ((<|>))
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.Aeson
  ( FromJSON(..), (.:), (.:?), (.!=), object, (.=)
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Client (Manager, Request(..), Response(..), httpLbs, newManager, parseRequest)
import Network.HTTP.Client.TLS (tlsManagerSettings)
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
  , makeRelative
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
          , "timeout" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Optional timeout in seconds" :: Text)
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

editToolDef :: ToolDef
editToolDef = ToolDef
  { toolName = "Edit"
  , toolDescription = "Replace a unique contiguous block of text in a file with new content."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object [ "type" .= ("string" :: Text), "description" .= ("Path of file" :: Text) ]
          , "old_content" .= object [ "type" .= ("string" :: Text), "description" .= ("Exact text to replace" :: Text) ]
          , "new_content" .= object [ "type" .= ("string" :: Text), "description" .= ("Replacement text" :: Text) ]
          ]
      , "required" .= (["path", "old_content", "new_content"] :: [Text])
      ]
  }

bashToolDef :: ToolDef
bashToolDef = ToolDef
  { toolName = "Bash"
  , toolDescription = "Execute a shell command inside the workspace directory."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "command" .= object [ "type" .= ("string" :: Text), "description" .= ("Shell command" :: Text) ]
          , "timeout" .= object [ "type" .= ("integer" :: Text), "description" .= ("Timeout in seconds" :: Text) ]
          ]
      , "required" .= (["command"] :: [Text])
      ]
  }

globToolDef :: ToolDef
globToolDef = ToolDef
  { toolName = "Glob"
  , toolDescription = "Fast file pattern matching across the workspace directory."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "pattern" .= object [ "type" .= ("string" :: Text), "description" .= ("File glob pattern" :: Text) ]
          , "path" .= object [ "type" .= ("string" :: Text), "description" .= ("Directory to search" :: Text) ]
          ]
      , "required" .= (["pattern"] :: [Text])
      ]
  }

grepToolDef :: ToolDef
grepToolDef = ToolDef
  { toolName = "Grep"
  , toolDescription = "Search file contents for an exact text pattern or regex."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "query" .= object [ "type" .= ("string" :: Text), "description" .= ("Search query" :: Text) ]
          , "path" .= object [ "type" .= ("string" :: Text), "description" .= ("Search directory or file" :: Text) ]
          , "case_sensitive" .= object [ "type" .= ("boolean" :: Text), "description" .= ("Case sensitivity" :: Text) ]
          ]
      , "required" .= (["query"] :: [Text])
      ]
  }

webFetchToolDef :: ToolDef
webFetchToolDef = ToolDef
  { toolName = "WebFetch"
  , toolDescription = "Fetch web page content via HTTP/HTTPS GET."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "url" .= object [ "type" .= ("string" :: Text), "description" .= ("URL to fetch" :: Text) ]
          ]
      , "required" .= (["url"] :: [Text])
      ]
  }

webSearchToolDef :: ToolDef
webSearchToolDef = ToolDef
  { toolName = "WebSearch"
  , toolDescription = "Perform a web search query."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "query" .= object [ "type" .= ("string" :: Text), "description" .= ("Search query" :: Text) ]
          ]
      , "required" .= (["query"] :: [Text])
      ]
  }

agentToolDef :: ToolDef
agentToolDef = ToolDef
  { toolName = "Agent"
  , toolDescription = "Spawn a delegated subagent with custom prompt and role."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object [ "type" .= ("string" :: Text), "description" .= ("Agent persona or name" :: Text) ]
          , "prompt" .= object [ "type" .= ("string" :: Text), "description" .= ("Task prompt for subagent" :: Text) ]
          ]
      , "required" .= (["name", "prompt"] :: [Text])
      ]
  }

todoWriteToolDef :: ToolDef
todoWriteToolDef = ToolDef
  { toolName = "TodoWrite"
  , toolDescription = "Save and track the active todo checklist."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "tasks" .= object [ "type" .= ("array" :: Text), "description" .= ("List of todo items" :: Text) ]
          ]
      , "required" .= (["tasks"] :: [Text])
      ]
  }

skillToolDef :: ToolDef
skillToolDef = ToolDef
  { toolName = "Skill"
  , toolDescription = "Invoke a discovered skill by name with arguments."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object [ "type" .= ("string" :: Text), "description" .= ("Skill name" :: Text) ]
          , "args" .= object [ "type" .= ("string" :: Text), "description" .= ("Arguments string" :: Text) ]
          ]
      , "required" .= (["name"] :: [Text])
      ]
  }

enterPlanModeToolDef :: ToolDef
enterPlanModeToolDef = ToolDef
  { toolName = "EnterPlanMode"
  , toolDescription = "Enter read-only planning mode."
  , toolParameters = object [ "type" .= ("object" :: Text), "properties" .= object [] ]
  }

exitPlanModeToolDef :: ToolDef
exitPlanModeToolDef = ToolDef
  { toolName = "ExitPlanMode"
  , toolDescription = "Exit planning mode back to normal execution."
  , toolParameters = object [ "type" .= ("object" :: Text), "properties" .= object [] ]
  }

enterWorktreeToolDef :: ToolDef
enterWorktreeToolDef = ToolDef
  { toolName = "EnterWorktree"
  , toolDescription = "Switch workspace into an isolated git worktree."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object [ "type" .= ("string" :: Text), "description" .= ("Worktree branch name" :: Text) ]
          ]
      , "required" .= (["name"] :: [Text])
      ]
  }

exitWorktreeToolDef :: ToolDef
exitWorktreeToolDef = ToolDef
  { toolName = "ExitWorktree"
  , toolDescription = "Exit worktree and restore workspace root."
  , toolParameters = object [ "type" .= ("object" :: Text), "properties" .= object [] ]
  }

listAgentsToolDef :: ToolDef
listAgentsToolDef = ToolDef
  { toolName = "ListAgents"
  , toolDescription = "List available and running agents."
  , toolParameters = object [ "type" .= ("object" :: Text), "properties" .= object [] ]
  }

sendMessageToolDef :: ToolDef
sendMessageToolDef = ToolDef
  { toolName = "SendMessage"
  , toolDescription = "Send a message to another agent by ID."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "agent_id" .= object [ "type" .= ("string" :: Text), "description" .= ("Recipient agent ID" :: Text) ]
          , "message" .= object [ "type" .= ("string" :: Text), "description" .= ("Message content" :: Text) ]
          ]
      , "required" .= (["agent_id", "message"] :: [Text])
      ]
  }

pushNotificationToolDef :: ToolDef
pushNotificationToolDef = ToolDef
  { toolName = "PushNotification"
  , toolDescription = "Dispatch a desktop notification to the user."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "title" .= object [ "type" .= ("string" :: Text), "description" .= ("Notification title" :: Text) ]
          , "message" .= object [ "type" .= ("string" :: Text), "description" .= ("Notification message body" :: Text) ]
          ]
      , "required" .= (["message"] :: [Text])
      ]
  }

monitorToolDef :: ToolDef
monitorToolDef = ToolDef
  { toolName = "Monitor"
  , toolDescription = "Check output and status of a running task."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "task_id" .= object [ "type" .= ("string" :: Text), "description" .= ("Task ID to monitor" :: Text) ]
          ]
      , "required" .= (["task_id"] :: [Text])
      ]
  }

taskCreateToolDef :: ToolDef
taskCreateToolDef = ToolDef
  { toolName = "TaskCreate"
  , toolDescription = "Create a background task or track an item."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object [ "type" .= ("string" :: Text), "description" .= ("Task name" :: Text) ]
          , "command" .= object [ "type" .= ("string" :: Text), "description" .= ("Shell command to run" :: Text) ]
          ]
      , "required" .= (["name"] :: [Text])
      ]
  }

taskGetToolDef :: ToolDef
taskGetToolDef = ToolDef
  { toolName = "TaskGet"
  , toolDescription = "Retrieve task details by ID."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "task_id" .= object [ "type" .= ("string" :: Text), "description" .= ("Task ID" :: Text) ]
          ]
      , "required" .= (["task_id"] :: [Text])
      ]
  }

taskListToolDef :: ToolDef
taskListToolDef = ToolDef
  { toolName = "TaskList"
  , toolDescription = "List all tracked background tasks."
  , toolParameters = object [ "type" .= ("object" :: Text), "properties" .= object [] ]
  }

taskUpdateToolDef :: ToolDef
taskUpdateToolDef = ToolDef
  { toolName = "TaskUpdate"
  , toolDescription = "Update a task's status."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "task_id" .= object [ "type" .= ("string" :: Text), "description" .= ("Task ID" :: Text) ]
          , "status" .= object [ "type" .= ("string" :: Text), "description" .= ("Status (pending, in_progress, completed, failed)" :: Text) ]
          ]
      , "required" .= (["task_id", "status"] :: [Text])
      ]
  }

taskStopToolDef :: ToolDef
taskStopToolDef = ToolDef
  { toolName = "TaskStop"
  , toolDescription = "Stop a running background task."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "task_id" .= object [ "type" .= ("string" :: Text), "description" .= ("Task ID to stop" :: Text) ]
          ]
      , "required" .= (["task_id"] :: [Text])
      ]
  }

askUserQuestionToolDef :: ToolDef
askUserQuestionToolDef = ToolDef
  { toolName = "AskUserQuestion"
  , toolDescription = "Ask the user a structured question with optional choices."
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "question" .= object [ "type" .= ("string" :: Text), "description" .= ("Question prompt" :: Text) ]
          , "options" .= object [ "type" .= ("array" :: Text), "description" .= ("Selectable options" :: Text) ]
          ]
      , "required" .= (["question"] :: [Text])
      ]
  }

endConversationToolDef :: ToolDef
endConversationToolDef = ToolDef
  { toolName = "EndConversation"
  , toolDescription = "End the active conversation."
  , toolParameters = object [ "type" .= ("object" :: Text), "properties" .= object [] ]
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
  , editToolDef
  , bashToolDef
  , globToolDef
  , grepToolDef
  , webFetchToolDef
  , webSearchToolDef
  , agentToolDef
  , todoWriteToolDef
  , skillToolDef
  , enterPlanModeToolDef
  , exitPlanModeToolDef
  , enterWorktreeToolDef
  , exitWorktreeToolDef
  , listAgentsToolDef
  , sendMessageToolDef
  , pushNotificationToolDef
  , monitorToolDef
  , taskCreateToolDef
  , taskGetToolDef
  , taskListToolDef
  , taskUpdateToolDef
  , taskStopToolDef
  , askUserQuestionToolDef
  , endConversationToolDef
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

data RunCommandArgs = RunCommandArgs
  { runCommandCmd     :: !Text
  , runCommandTimeout :: !(Maybe Int)
  } deriving (Show, Eq)

instance FromJSON RunCommandArgs where
  parseJSON = Aeson.withObject "RunCommandArgs" $ \o ->
    RunCommandArgs <$> o .: "command" <*> o .:? "timeout"

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

data EditArgs = EditArgs
  { editPath       :: !FilePath
  , editOldContent :: !Text
  , editNewContent :: !Text
  } deriving (Show, Eq)

instance FromJSON EditArgs where
  parseJSON = Aeson.withObject "EditArgs" $ \o -> do
    editPath <- o .: "path"
    editOldContent <- o .: "old_content" <|> o .: "oldText" <|> o .: "old_string"
    editNewContent <- o .: "new_content" <|> o .: "newText" <|> o .: "new_string"
    pure EditArgs{..}

data BashArgs = BashArgs
  { bashCommand :: !Text
  , bashTimeout :: !(Maybe Int)
  } deriving (Show, Eq)

instance FromJSON BashArgs where
  parseJSON = Aeson.withObject "BashArgs" $ \o -> do
    bashCommand <- o .: "command"
    bashTimeout <- o .:? "timeout"
    pure BashArgs{..}

data GlobArgs = GlobArgs
  { globPattern :: !Text
  , globPath    :: !FilePath
  } deriving (Show, Eq)

instance FromJSON GlobArgs where
  parseJSON = Aeson.withObject "GlobArgs" $ \o -> do
    globPattern <- o .: "pattern"
    globPath <- o .:? "path" .!= "."
    pure GlobArgs{..}

data GrepArgs = GrepArgs
  { grepQueryText        :: !Text
  , grepPathText         :: !FilePath
  , grepArgCaseSensitive :: !Bool
  } deriving (Show, Eq)

instance FromJSON GrepArgs where
  parseJSON = Aeson.withObject "GrepArgs" $ \o -> do
    grepQueryText <- o .: "query" <|> o .: "pattern"
    grepPathText <- o .:? "path" .!= "."
    grepArgCaseSensitive <- o .:? "case_sensitive" .!= True
    pure GrepArgs{..}

newtype WebFetchArgs = WebFetchArgs { webFetchUrl :: Text }
  deriving (Show, Eq)

instance FromJSON WebFetchArgs where
  parseJSON = Aeson.withObject "WebFetchArgs" $ \o ->
    WebFetchArgs <$> o .: "url"

newtype WebSearchArgs = WebSearchArgs { webSearchQuery :: Text }
  deriving (Show, Eq)

instance FromJSON WebSearchArgs where
  parseJSON = Aeson.withObject "WebSearchArgs" $ \o ->
    WebSearchArgs <$> o .: "query"

data AgentArgs = AgentArgs
  { agentArgName   :: !Text
  , agentArgPrompt :: !Text
  } deriving (Show, Eq)

instance FromJSON AgentArgs where
  parseJSON = Aeson.withObject "AgentArgs" $ \o -> do
    agentArgName <- o .: "name"
    agentArgPrompt <- o .: "prompt"
    pure AgentArgs{..}

newtype TodoWriteArgs = TodoWriteArgs { todoTasks :: [Text] }
  deriving (Show, Eq)

instance FromJSON TodoWriteArgs where
  parseJSON = Aeson.withObject "TodoWriteArgs" $ \o ->
    TodoWriteArgs <$> o .: "tasks"

data SkillToolArgs = SkillToolArgs
  { skillToolName :: !Text
  , skillToolArgs :: !(Maybe Text)
  } deriving (Show, Eq)

instance FromJSON SkillToolArgs where
  parseJSON = Aeson.withObject "SkillToolArgs" $ \o -> do
    skillToolName <- o .: "name"
    skillToolArgs <- o .:? "args" <|> o .:? "arguments"
    pure SkillToolArgs{..}

newtype EnterWorktreeArgs = EnterWorktreeArgs { worktreeName :: Text }
  deriving (Show, Eq)

instance FromJSON EnterWorktreeArgs where
  parseJSON = Aeson.withObject "EnterWorktreeArgs" $ \o ->
    EnterWorktreeArgs <$> o .: "name"

data SendMessageArgs = SendMessageArgs
  { sendMsgRecipient :: !AgentId
  , sendMsgContent   :: !Text
  } deriving (Show, Eq)

instance FromJSON SendMessageArgs where
  parseJSON = Aeson.withObject "SendMessageArgs" $ \o -> do
    sendMsgRecipient <- o .: "agent_id"
    sendMsgContent <- o .: "message"
    pure SendMessageArgs{..}

data PushNotificationArgs = PushNotificationArgs
  { pushTitle   :: !Text
  , pushMessage :: !Text
  } deriving (Show, Eq)

instance FromJSON PushNotificationArgs where
  parseJSON = Aeson.withObject "PushNotificationArgs" $ \o -> do
    pushTitle <- o .:? "title" .!= "Agent Notification"
    pushMessage <- o .: "message"
    pure PushNotificationArgs{..}

newtype MonitorArgs = MonitorArgs { monitorTaskId :: TaskId }
  deriving (Show, Eq)

instance FromJSON MonitorArgs where
  parseJSON = Aeson.withObject "MonitorArgs" $ \o ->
    MonitorArgs <$> o .: "task_id"

data TaskCreateArgs = TaskCreateArgs
  { taskCreateName    :: !Text
  , taskCreateCommand :: !(Maybe Text)
  } deriving (Show, Eq)

instance FromJSON TaskCreateArgs where
  parseJSON = Aeson.withObject "TaskCreateArgs" $ \o -> do
    taskCreateName <- o .: "name"
    taskCreateCommand <- o .:? "command"
    pure TaskCreateArgs{..}

newtype TaskGetArgs = TaskGetArgs { taskGetId :: TaskId }
  deriving (Show, Eq)

instance FromJSON TaskGetArgs where
  parseJSON = Aeson.withObject "TaskGetArgs" $ \o ->
    TaskGetArgs <$> o .: "task_id"

data TaskUpdateArgs = TaskUpdateArgs
  { taskUpdateId     :: !TaskId
  , taskUpdateStatus :: !Text
  } deriving (Show, Eq)

instance FromJSON TaskUpdateArgs where
  parseJSON = Aeson.withObject "TaskUpdateArgs" $ \o -> do
    taskUpdateId <- o .: "task_id"
    taskUpdateStatus <- o .: "status"
    pure TaskUpdateArgs{..}

newtype TaskStopArgs = TaskStopArgs { taskStopId :: TaskId }
  deriving (Show, Eq)

instance FromJSON TaskStopArgs where
  parseJSON = Aeson.withObject "TaskStopArgs" $ \o ->
    TaskStopArgs <$> o .: "task_id"

data AskUserQuestionArgs = AskUserQuestionArgs
  { askQuestionText    :: !Text
  , askQuestionOptions :: ![Text]
  } deriving (Show, Eq)

instance FromJSON AskUserQuestionArgs where
  parseJSON = Aeson.withObject "AskUserQuestionArgs" $ \o -> do
    askQuestionText <- o .: "question"
    askQuestionOptions <- o .:? "options" .!= []
    pure AskUserQuestionArgs{..}

parseEditArgs :: ToolCall -> Either String EditArgs
parseEditArgs = parseArgsWith

parseBashArgs :: ToolCall -> Either String BashArgs
parseBashArgs = parseArgsWith

parseGlobArgs :: ToolCall -> Either String GlobArgs
parseGlobArgs = parseArgsWith

parseGrepArgs :: ToolCall -> Either String GrepArgs
parseGrepArgs = parseArgsWith

parseWebFetchArgs :: ToolCall -> Either String WebFetchArgs
parseWebFetchArgs = parseArgsWith

parseWebSearchArgs :: ToolCall -> Either String WebSearchArgs
parseWebSearchArgs = parseArgsWith

parseAgentArgs :: ToolCall -> Either String AgentArgs
parseAgentArgs = parseArgsWith

parseTodoWriteArgs :: ToolCall -> Either String TodoWriteArgs
parseTodoWriteArgs = parseArgsWith

parseSkillToolArgs :: ToolCall -> Either String SkillToolArgs
parseSkillToolArgs = parseArgsWith

parseEnterWorktreeArgs :: ToolCall -> Either String EnterWorktreeArgs
parseEnterWorktreeArgs = parseArgsWith

parseSendMessageArgs :: ToolCall -> Either String SendMessageArgs
parseSendMessageArgs = parseArgsWith

parsePushNotificationArgs :: ToolCall -> Either String PushNotificationArgs
parsePushNotificationArgs = parseArgsWith

parseMonitorArgs :: ToolCall -> Either String MonitorArgs
parseMonitorArgs = parseArgsWith

parseTaskCreateArgs :: ToolCall -> Either String TaskCreateArgs
parseTaskCreateArgs = parseArgsWith

parseTaskGetArgs :: ToolCall -> Either String TaskGetArgs
parseTaskGetArgs = parseArgsWith

parseTaskUpdateArgs :: ToolCall -> Either String TaskUpdateArgs
parseTaskUpdateArgs = parseArgsWith

parseTaskStopArgs :: ToolCall -> Either String TaskStopArgs
parseTaskStopArgs = parseArgsWith

parseAskUserQuestionArgs :: ToolCall -> Either String AskUserQuestionArgs
parseAskUserQuestionArgs = parseArgsWith

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

    name | name `elem` ["replace_file_content", "Edit", "edit"] ->
      case parseEditArgs call of
        Left err   -> pure $ ToolError ("Failed to parse Edit args: " <> T.pack err)
        Right args -> executeReplaceFileContent root (ReplaceFileContentArgs (editPath args) (editOldContent args) (editNewContent args))

    name | name `elem` ["run_command", "Bash", "bash"] ->
      case parseBashArgs call of
        Left err   -> pure $ ToolError ("Failed to parse Bash args: " <> T.pack err)
        Right args -> executeRunCommand root (RunCommandArgs (bashCommand args) (bashTimeout args))

    name | name `elem` ["list_dir", "ListDir"] ->
      case parseListDirArgs call of
        Left err   -> pure $ ToolError ("Failed to parse list_dir args: " <> T.pack err)
        Right args -> executeListDir root args

    name | name `elem` ["find_files", "Glob", "glob"] ->
      case parseGlobArgs call of
        Left err   -> pure $ ToolError ("Failed to parse " <> name <> " args: " <> T.pack err)
        Right args -> executeFindFiles root (FindFilesArgs (globPattern args) (globPath args))

    name | name `elem` ["grep_search", "Grep", "grep"] ->
      case parseGrepArgs call of
        Left err   -> pure $ ToolError ("Failed to parse " <> name <> " args: " <> T.pack err)
        Right args -> executeGrepSearch root (GrepSearchArgs (grepQueryText args) (grepPathText args) (grepArgCaseSensitive args))

    name | name `elem` ["WebFetch", "web_fetch"] ->
      case parseWebFetchArgs call of
        Left err   -> pure $ ToolError ("Failed to parse WebFetch args: " <> T.pack err)
        Right args -> executeWebFetch args

    name | name `elem` ["WebSearch", "web_search"] ->
      case parseWebSearchArgs call of
        Left err   -> pure $ ToolError ("Failed to parse WebSearch args: " <> T.pack err)
        Right args -> executeWebSearch args

    name | name `elem` ["Agent", "agent"] ->
      case parseAgentArgs call of
        Left err   -> pure $ ToolError ("Failed to parse Agent args: " <> T.pack err)
        Right args -> pure $ ToolSuccess ("Spawned subagent '" <> agentArgName args <> "' with prompt: " <> agentArgPrompt args)

    name | name `elem` ["TodoWrite", "todo_write"] ->
      case parseTodoWriteArgs call of
        Left err   -> pure $ ToolError ("Failed to parse TodoWrite args: " <> T.pack err)
        Right args -> executeTodoWrite root args

    name | name `elem` ["Skill", "skill"] ->
      case parseSkillToolArgs call of
        Left err   -> pure $ ToolError ("Failed to parse Skill args: " <> T.pack err)
        Right args -> executeSkill root args

    name | name `elem` ["EnterPlanMode", "enter_plan_mode"] ->
      pure $ ToolSuccess "Entered plan mode. The agent is now in read-only planning mode."

    name | name `elem` ["ExitPlanMode", "exit_plan_mode"] ->
      pure $ ToolSuccess "Exited plan mode. The agent is now in standard execution mode."

    name | name `elem` ["EnterWorktree", "enter_worktree"] ->
      case parseEnterWorktreeArgs call of
        Left err   -> pure $ ToolError ("Failed to parse EnterWorktree args: " <> T.pack err)
        Right args -> executeEnterWorktree root args

    name | name `elem` ["ExitWorktree", "exit_worktree"] ->
      executeExitWorktree root

    name | name `elem` ["ListAgents", "list_agents"] ->
      pure $ ToolSuccess "Available subagents: explore, plan."

    name | name `elem` ["SendMessage", "send_message"] ->
      case parseSendMessageArgs call of
        Left err   -> pure $ ToolError ("Failed to parse SendMessage args: " <> T.pack err)
        Right args -> pure $ ToolSuccess ("Message sent to agent " <> unAgentId (sendMsgRecipient args) <> ": " <> sendMsgContent args)

    name | name `elem` ["PushNotification", "push_notification"] ->
      case parsePushNotificationArgs call of
        Left err   -> pure $ ToolError ("Failed to parse PushNotification args: " <> T.pack err)
        Right args -> executePushNotification args

    name | name `elem` ["Monitor", "monitor"] ->
      case parseMonitorArgs call of
        Left err   -> pure $ ToolError ("Failed to parse Monitor args: " <> T.pack err)
        Right args -> executeMonitor args

    name | name `elem` ["TaskCreate", "task_create"] ->
      case parseTaskCreateArgs call of
        Left err   -> pure $ ToolError ("Failed to parse TaskCreate args: " <> T.pack err)
        Right args -> executeTaskCreate root args

    name | name `elem` ["TaskGet", "task_get"] ->
      case parseTaskGetArgs call of
        Left err   -> pure $ ToolError ("Failed to parse TaskGet args: " <> T.pack err)
        Right args -> executeTaskGet args

    name | name `elem` ["TaskList", "task_list"] ->
      executeTaskList

    name | name `elem` ["TaskUpdate", "task_update"] ->
      case parseTaskUpdateArgs call of
        Left err   -> pure $ ToolError ("Failed to parse TaskUpdate args: " <> T.pack err)
        Right args -> executeTaskUpdate args

    name | name `elem` ["TaskStop", "task_stop"] ->
      case parseTaskStopArgs call of
        Left err   -> pure $ ToolError ("Failed to parse TaskStop args: " <> T.pack err)
        Right args -> executeTaskStop args

    name | name `elem` ["AskUserQuestion", "ask_user_question"] ->
      case parseAskUserQuestionArgs call of
        Left err   -> pure $ ToolError ("Failed to parse AskUserQuestion args: " <> T.pack err)
        Right args -> executeAskUserQuestion args

    name | name `elem` ["EndConversation", "end_conversation"] ->
      pure $ ToolSuccess "Conversation completed by agent."

    unknown ->
      pure $ ToolError ("Unknown tool function: " <> unknown)
  pure $ case res of
    ToolSuccess out -> ToolSuccess (truncateToolOutput out)
    err             -> err

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
executeWriteFile root (WriteFileArgs path content)
  | isProtectedPath path = pure $ ToolError ("Protected path: write denied to " <> T.pack path)
  | otherwise = do
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
executeReplaceFileContent root (ReplaceFileContentArgs path oldContent newContent)
  | T.null oldContent = pure $ ToolError "The 'old_content' parameter cannot be empty."
  | isProtectedPath path = pure $ ToolError ("Protected path: edit denied to " <> T.pack path)
  | otherwise = do
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
executeRunCommand root (RunCommandArgs cmd mTimeout) = do
  let secs = maybe 60 (max 1) mTimeout
      sh = (shell (T.unpack cmd)) { cwd = Just root }
  -- Timeout to prevent runaway or interactive processes from hanging the harness
  res <- try (timeout (secs * 1000000) (readCreateProcessWithExitCode sh "")) :: IO (Either SomeException (Maybe (ExitCode, String, String)))
  case res of
    Left ex -> pure $ ToolError ("Process execution failed: " <> T.pack (show ex))
    Right Nothing ->
      pure $ ToolError ("Command timed out after " <> T.pack (show secs) <> " seconds: " <> cmd)
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

-- | Pattern matcher for find_files: '*' wildcards match across path separators
-- in linear time via 'matchStarGlob', while non-wildcard patterns match as
-- substrings of either the filename or the full relative path.
matchPattern :: Text -> FilePath -> Bool
matchPattern p fp =
  let name = T.pack (takeFileName fp)
      full = T.pack fp
  in if "*" `T.isInfixOf` p
       then matchStarGlob p name || matchStarGlob p full
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

executeWebFetch :: WebFetchArgs -> IO ToolResult
executeWebFetch (WebFetchArgs url) = do
  mgrRes <- try (newManager tlsManagerSettings) :: IO (Either SomeException Manager)
  case mgrRes of
    Left ex -> pure $ ToolError ("Failed to create HTTP manager: " <> T.pack (show ex))
    Right mgr -> do
      reqRes <- try (parseRequest (T.unpack url)) :: IO (Either SomeException Request)
      case reqRes of
        Left ex -> pure $ ToolError ("Invalid URL '" <> url <> "': " <> T.pack (show ex))
        Right req -> do
          let req' = req { requestHeaders = [("User-Agent", "hach/0.1.5.0")] }
          respRes <- try (httpLbs req' mgr) :: IO (Either SomeException (Response BSL.ByteString))
          case respRes of
            Left ex -> pure $ ToolError ("HTTP fetch error: " <> T.pack (show ex))
            Right resp -> do
              let body = responseBody resp
                  txt = TE.decodeUtf8With TE.lenientDecode (BSL.toStrict body)
              pure $ ToolSuccess (truncateToolOutput txt)

executeWebSearch :: WebSearchArgs -> IO ToolResult
executeWebSearch (WebSearchArgs query) =
  pure $ ToolSuccess ("Search query recorded: '" <> query <> "'. No external search provider configured; use WebFetch to retrieve URLs.")

executeTodoWrite :: FilePath -> TodoWriteArgs -> IO ToolResult
executeTodoWrite root (TodoWriteArgs tasks) = do
  let todoDir = root </> ".claude"
      todoFile = todoDir </> "todos.json"
  createDirectoryIfMissing True todoDir
  BS.writeFile todoFile (BSL.toStrict (Aeson.encode tasks))
  pure $ ToolSuccess ("Saved " <> T.pack (show (length tasks)) <> " todo items to .claude/todos.json.")

executePushNotification :: PushNotificationArgs -> IO ToolResult
executePushNotification (PushNotificationArgs title msg) = do
  res <- sendDesktopNotification title msg
  case res of
    Just err -> pure $ ToolError ("Notification failed: " <> err)
    Nothing  -> pure $ ToolSuccess ("Dispatched notification: " <> title <> " - " <> msg)

executeEnterWorktree :: FilePath -> EnterWorktreeArgs -> IO ToolResult
executeEnterWorktree root (EnterWorktreeArgs name) = do
  res <- createWorktree root name
  case res of
    Left err -> pure $ ToolError err
    Right wtPath -> pure $ ToolSuccess ("Created and entered worktree: " <> T.pack wtPath)

executeExitWorktree :: FilePath -> IO ToolResult
executeExitWorktree root = do
  isWt <- isWorktreeDirectory root
  if isWt
    then pure $ ToolSuccess "Exited worktree and restored workspace root."
    else pure $ ToolError "Not currently inside a worktree."

executeSkill :: FilePath -> SkillToolArgs -> IO ToolResult
executeSkill root (SkillToolArgs name mArgs) = do
  catalog <- discoverSkills root
  case Map.lookup name catalog of
    Nothing -> pure $ ToolError ("Skill not found: " <> name)
    Just sk -> do
      let content = case mArgs of
            Just args -> substituteArguments args (skillContent sk)
            Nothing   -> skillContent sk
      expanded <- injectDynamicContext root content
      pure $ ToolSuccess ("Skill '" <> name <> "' content:\n" <> expanded)

globalBgRegistry :: BackgroundRegistry
globalBgRegistry = unsafePerformIO newBackgroundRegistry
{-# NOINLINE globalBgRegistry #-}

globalTaskStore :: TVar TaskStore
globalTaskStore = unsafePerformIO (newTVarIO emptyTaskStore)
{-# NOINLINE globalTaskStore #-}

executeTaskCreate :: FilePath -> TaskCreateArgs -> IO ToolResult
executeTaskCreate root (TaskCreateArgs name mCmd) = do
  case mCmd of
    Just cmd | not (T.null (T.strip cmd)) -> do
      tid <- spawnBackgroundProcess globalBgRegistry root cmd
      atomically $ modifyTVar' globalTaskStore (\s -> snd (createTaskWithId s (unTaskId tid) name))
      pure $ ToolSuccess ("Created background task " <> unTaskId tid <> " running: " <> cmd)
    _ -> do
      t <- atomically $ do
        s <- readTVar globalTaskStore
        let (newTask, s') = createTask s name
        writeTVar globalTaskStore s'
        pure newTask
      pure $ ToolSuccess ("Created task " <> taskId t <> ": " <> taskTitle t)

executeTaskGet :: TaskGetArgs -> IO ToolResult
executeTaskGet (TaskGetArgs (TaskId tid)) = do
  store <- readTVarIO globalTaskStore
  case getTask store tid of
    Nothing -> pure $ ToolError ("Task not found: " <> tid)
    Just t  -> pure $ ToolSuccess (formatTaskList [t])

executeTaskList :: IO ToolResult
executeTaskList = do
  store <- readTVarIO globalTaskStore
  let ts = listTasks store
  pure $ ToolSuccess (formatTaskList ts)

executeTaskUpdate :: TaskUpdateArgs -> IO ToolResult
executeTaskUpdate (TaskUpdateArgs (TaskId tid) st) = do
  res <- atomically $ do
    s <- readTVar globalTaskStore
    case getTask s tid of
      Nothing -> pure (Left ("Task not found: " <> tid))
      Just _  -> do
        writeTVar globalTaskStore (updateTask s tid st)
        pure (Right ())
  case res of
    Left err -> pure $ ToolError err
    Right () -> pure $ ToolSuccess ("Updated task " <> tid <> " status to " <> st)

executeTaskStop :: TaskStopArgs -> IO ToolResult
executeTaskStop (TaskStopArgs tid) = do
  ok <- stopBackgroundProcess globalBgRegistry tid
  if ok
    then do
      atomically $ modifyTVar' globalTaskStore (\s -> updateTask s (unTaskId tid) "stopped")
      pure $ ToolSuccess ("Stopped task " <> unTaskId tid)
    else pure $ ToolError ("Failed to stop task " <> unTaskId tid)

executeMonitor :: MonitorArgs -> IO ToolResult
executeMonitor (MonitorArgs tid) = do
  mRes <- getBackgroundOutput globalBgRegistry tid
  case mRes of
    ToolSuccess out ->
      pure $ ToolSuccess ("Task " <> unTaskId tid <> " output:\n" <> if T.null out then "(no output recorded yet)" else out)
    err -> pure err

executeAskUserQuestion :: AskUserQuestionArgs -> IO ToolResult
executeAskUserQuestion (AskUserQuestionArgs q opts) = do
  let optsTxt = if null opts then "" else "\nOptions:\n" <> T.unlines (map (\o -> "- " <> o) opts)
  pure $ ToolSuccess ("Prompted user: " <> q <> optsTxt)


