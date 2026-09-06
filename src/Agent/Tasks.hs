{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Agent.Tasks
  ( Task(..)
  , TaskStore
  , emptyTaskStore
  , createTask
  , getTask
  , listTasks
  , updateTask
  , formatTaskList
  , BackgroundRegistry
  , newBackgroundRegistry
  , spawnBackgroundProcess
  , getBackgroundOutput
  , stopBackgroundProcess
  ) where

import Agent.Types
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.IO (Handle, hIsEOF)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , shell
  , terminateProcess
  )

-- | Model task item for TodoWrite / TaskCreate / TaskList.
data Task = Task
  { taskId     :: !Text
  , taskTitle  :: !Text
  , taskStatus :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON Task
instance FromJSON Task

type TaskStore = Map Text Task

emptyTaskStore :: TaskStore
emptyTaskStore = Map.empty

createTask :: TaskStore -> Text -> (Task, TaskStore)
createTask store title =
  let nextId = "task-" <> T.pack (show (Map.size store + 1))
      newTask = Task nextId title "pending"
  in (newTask, Map.insert nextId newTask store)

getTask :: TaskStore -> Text -> Maybe Task
getTask store tid = Map.lookup tid store

listTasks :: TaskStore -> [Task]
listTasks = Map.elems

updateTask :: TaskStore -> Text -> Text -> TaskStore
updateTask store tid newStatus =
  Map.adjust (\t -> t { taskStatus = newStatus }) tid store

formatTaskList :: [Task] -> Text
formatTaskList [] = "No active tasks."
formatTaskList ts = T.unlines
  [ "- [" <> taskStatus t <> "] #" <> taskId t <> ": " <> taskTitle t
  | t <- ts
  ]

--------------------------------------------------------------------------------
-- Background Process Execution
--------------------------------------------------------------------------------

data BgProcess = BgProcess
  { bpHandle  :: !ProcessHandle
  , bpOutput  :: !(TVar Text)
  , bpRunning :: !(TVar Bool)
  }

type BackgroundRegistry = TVar (Map TaskId BgProcess)

newBackgroundRegistry :: IO BackgroundRegistry
newBackgroundRegistry = newTVarIO Map.empty

spawnBackgroundProcess :: BackgroundRegistry -> FilePath -> Text -> IO TaskId
spawnBackgroundProcess reg root cmd = do
  let procSpec = (shell (T.unpack cmd))
        { cwd = Just root
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  (_, mOut, mErr, pHandle) <- createProcess procSpec
  outVar <- newTVarIO ""
  runVar <- newTVarIO True

  case (mOut, mErr) of
    (Just hOut, Just hErr) -> do
      _ <- forkIO (readStream hOut outVar)
      _ <- forkIO (readStream hErr outVar)
      pure ()
    _ -> pure ()

  tId <- atomically $ do
    m <- readTVar reg
    let tid = TaskId ("bg-" <> T.pack (show (Map.size m + 1)))
        bp = BgProcess pHandle outVar runVar
    writeTVar reg (Map.insert tid bp m)
    pure tid
  pure tId
  where
    readStream :: Handle -> TVar Text -> IO ()
    readStream h var = go
      where
        go = do
          eof <- hIsEOF h
          if eof
            then pure ()
            else do
              lineRes <- try (TIO.hGetLine h) :: IO (Either SomeException Text)
              case lineRes of
                Left _ -> pure ()
                Right line -> do
                  atomically $ modifyTVar' var (\cur -> cur <> line <> "\n")
                  go

getBackgroundOutput :: BackgroundRegistry -> TaskId -> IO ToolResult
getBackgroundOutput reg tid = do
  mBp <- atomically $ do
    m <- readTVar reg
    pure (Map.lookup tid m)
  case mBp of
    Nothing -> pure (ToolError ("No running background task with ID " <> unTaskId tid))
    Just bp -> do
      txt <- readTVarIO (bpOutput bp)
      pure (ToolSuccess (if T.null txt then "(no output yet)" else txt))

stopBackgroundProcess :: BackgroundRegistry -> TaskId -> IO Bool
stopBackgroundProcess reg tid = do
  mBp <- atomically $ do
    m <- readTVar reg
    pure (Map.lookup tid m)
  case mBp of
    Nothing -> pure False
    Just bp -> do
      res <- try (terminateProcess (bpHandle bp)) :: IO (Either SomeException ())
      atomically $ writeTVar (bpRunning bp) False
      pure (case res of Right () -> True; Left _ -> False)
