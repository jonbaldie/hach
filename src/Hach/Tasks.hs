{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Hach.Tasks
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
  , stopAllBackgroundProcesses
  , signalGroup
  ) where

import Hach.Types
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import System.Exit (ExitCode)
import System.IO (Handle, hIsEOF)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , getPid
  , getProcessExitCode
  , shell
  )
import System.Posix.Signals (Signal, nullSignal, sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Types (ProcessGroupID)

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

createTaskWithId :: TaskStore -> Text -> Text -> (Task, TaskStore)
createTaskWithId store customId title =
  let newTask = Task customId title "pending"
  in (newTask, Map.insert customId newTask store)

createTask :: TaskStore -> Text -> (Task, TaskStore)
createTask store title =
  let nextId = unusedTaskId store 1
  in createTaskWithId store nextId title

-- | First unused 'task-N' identifier. Size-based allocation collides when
-- a custom id such as @task-2@ already occupies the generated namespace.
unusedTaskId :: TaskStore -> Int -> Text
unusedTaskId store n =
  let cid = "task-" <> T.pack (show n)
  in if Map.member cid store
       then unusedTaskId store (n + 1)
       else cid

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
  , bpGroup   :: !(TVar (Maybe ProcessGroupID))
    -- ^ The group the task's shell leads, captured at spawn: once the shell
    -- is reaped its handle forgets the pid, but its children remain. Cleared
    -- once the group is gone, so a reused id is never signalled.
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
        , create_group = True
        }
  (_, mOut, mErr, pHandle) <- createProcess procSpec
  groupVar <- newTVarIO =<< getPid pHandle
  outVar <- newTVarIO ""
  runVar <- newTVarIO True

  case (mOut, mErr) of
    (Just hOut, Just hErr) -> do
      _ <- forkIO (readStream hOut outVar)
      _ <- forkIO (readStream hErr outVar)
      pure ()
    _ -> pure ()

  _ <- forkIO $ do
    awaitExit pHandle
    atomically $ writeTVar runVar False

  tId <- atomically $ do
    m <- readTVar reg
    let tid = TaskId ("bg-" <> T.pack (show (Map.size m + 1)))
        bp = BgProcess pHandle groupVar outVar runVar
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

-- | Wait for a process to exit by polling. 'waitForProcess' is a blocking
-- foreign call that stalls every thread under the non-threaded RTS.
awaitExit :: ProcessHandle -> IO ()
awaitExit ph = do
  res <- try (getProcessExitCode ph) :: IO (Either SomeException (Maybe ExitCode))
  case res of
    Right Nothing -> threadDelay 50000 >> awaitExit ph
    _ -> pure ()

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

-- | Stop a background task and everything it started. Succeeds only once its
-- shell is reaped and no process in its group is left; otherwise says what
-- survived.
stopBackgroundProcess :: BackgroundRegistry -> TaskId -> IO (Either Text ())
stopBackgroundProcess reg tid = do
  mBp <- Map.lookup tid <$> readTVarIO reg
  case mBp of
    Nothing -> pure (Left ("No background task with ID " <> unTaskId tid))
    Just bp -> do
      mGroup <- readTVarIO (bpGroup bp)
      gone <- maybe (pure True) (stopGroup (bpHandle bp)) mGroup
      if gone
        then do
          atomically $ do
            writeTVar (bpGroup bp) Nothing
            writeTVar (bpRunning bp) False
          pure (Right ())
        else pure (Left ("Processes in group " <> maybe "?" (T.pack . show) mGroup
                         <> " of task " <> unTaskId tid <> " survived SIGKILL"))

-- | Terminate the group a task's shell leads, then reap the shell. A child
-- the shell forked while the group was being killed can outlive the sweep,
-- so the group is checked again once the shell is gone.
stopGroup :: ProcessHandle -> ProcessGroupID -> IO Bool
stopGroup ph pgid = do
  _ <- terminateProcessGroup pgid
  _ <- within gracePeriod (reaped ph)
  alive <- groupAlive pgid
  if alive then killProcessGroup pgid else pure True

-- | Stop every background task, as Hach does on exit.
stopAllBackgroundProcesses :: BackgroundRegistry -> IO ()
stopAllBackgroundProcesses reg = do
  tids <- Map.keys <$> readTVarIO reg
  mapM_ (stopBackgroundProcess reg) tids

-- | Send a signal to a whole process group, ignoring a group that has gone.
signalGroup :: Signal -> ProcessGroupID -> IO ()
signalGroup sig pgid = do
  _ <- try (signalProcessGroup sig pgid) :: IO (Either SomeException ())
  pure ()

-- | SIGTERM a process group, then SIGKILL it if it outlives a grace period.
-- True once the group has no members left.
terminateProcessGroup :: ProcessGroupID -> IO Bool
terminateProcessGroup pgid = do
  signalGroup sigTERM pgid
  termed <- within gracePeriod (not <$> groupAlive pgid)
  if termed then pure True else killProcessGroup pgid

-- | SIGKILL a process group until it has no members left, re-sending the
-- signal so that processes forked during the sweep are caught too.
killProcessGroup :: ProcessGroupID -> IO Bool
killProcessGroup pgid =
  within gracePeriod (signalGroup sigKILL pgid >> not <$> groupAlive pgid)

-- | Whether any process remains in a group. The kernel skips zombies here,
-- so a dead but unreaped group leader does not count.
groupAlive :: ProcessGroupID -> IO Bool
groupAlive pgid = either (const False) (const True)
  <$> (try (signalProcessGroup nullSignal pgid) :: IO (Either SomeException ()))

-- | Whether the process has exited and been reaped.
reaped :: ProcessHandle -> IO Bool
reaped ph = either (const True) (maybe False (const True))
  <$> (try (getProcessExitCode ph) :: IO (Either SomeException (Maybe ExitCode)))

-- | Seconds a task gets to exit after each signal.
gracePeriod :: Double
gracePeriod = 2

-- | Poll a check until it holds or the given number of seconds has passed.
within :: Double -> IO Bool -> IO Bool
within secs holds = getMonotonicTime >>= poll . (+ secs)
  where
    poll deadline = do
      ok <- holds
      now <- getMonotonicTime
      if ok || now >= deadline
        then pure ok
        else threadDelay 50000 >> poll deadline
