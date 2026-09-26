{-# LANGUAGE OverloadedStrings #-}

module Hach.TasksSpec (spec) where

import Hach.Tasks
import Hach.Types (TaskId, ToolResult(..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec

-- | Run an action in a fresh scratch directory that is removed afterwards.
withScratch :: String -> (FilePath -> IO a) -> IO a
withScratch name action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> ("hach-tasks-spec-" <> name)
      discard = try (removeDirectoryRecursive dir) :: IO (Either SomeException ())
  bracket (discard >> createDirectoryIfMissing True dir >> pure dir) (const discard) action

-- | Poll until the command has written its pid file, then read it.
awaitPid :: FilePath -> IO String
awaitPid path = go (50 :: Int)
  where
    go 0 = expectationFailure ("pid file never appeared: " <> path) >> pure ""
    go n = do
      exists <- doesFileExist path
      contents <- if exists then readFile path else pure ""
      case words contents of
        [pid] -> pure pid
        _ -> threadDelay 100000 >> go (n - 1)

-- | Stop a task, failing rather than hanging if the stop never returns.
stopWithin :: BackgroundRegistry -> TaskId -> IO (Either T.Text ())
stopWithin reg tid = do
  res <- timeout 10000000 (stopBackgroundProcess reg tid)
  case res of
    Nothing -> expectationFailure "stopBackgroundProcess did not return within 10s" >> pure (Left "timeout")
    Just ok -> pure ok

-- | Whether a process with this pid still exists.
isAlive :: String -> IO Bool
isAlive pid = do
  (code, _, _) <- readProcessWithExitCode "kill" ["-0", pid] ""
  pure (code == ExitSuccess)

spec :: Spec
spec = describe "Hach.Tasks" $ do
  describe "Shared task list management" $ do
    it "creates, retrieves, updates, and lists tasks" $ do
      let store0 = emptyTaskStore
          (t1, store1) = createTask store0 "Implement streaming"
          (_t2, store2) = createTask store1 "Add notifications"
      taskTitle t1 `shouldBe` "Implement streaming"
      taskStatus t1 `shouldBe` "pending"
      length (listTasks store2) `shouldBe` 2

      let store3 = updateTask store2 (taskId t1) "in_progress"
      case getTask store3 (taskId t1) of
        Nothing -> expectationFailure "Expected to find task t1"
        Just updated -> taskStatus updated `shouldBe` "in_progress"

    it "creates tasks with custom task ID using createTaskWithId" $ do
      let store0 = emptyTaskStore
          (t1, store1) = createTaskWithId store0 "bg-1" "Background compile"
      taskId t1 `shouldBe` "bg-1"
      taskTitle t1 `shouldBe` "Background compile"
      taskStatus t1 `shouldBe` "pending"
      case getTask store1 "bg-1" of
        Nothing -> expectationFailure "Expected to find task bg-1"
        Just found -> taskId found `shouldBe` "bg-1"

  describe "Task serialization" $ do
    it "formats task list into readable summary" $ do
      let store0 = emptyTaskStore
          (_t1, store1) = createTask store0 "Test task"
          summary = formatTaskList (listTasks store1)
      T.unpack summary `shouldContain` "Test task"
      T.unpack summary `shouldContain` "pending"

  describe "Background process lifecycle and reaping (BUG-9)" $ do
    it "spawns background process, captures output, and reaps upon exit" $ do
      reg <- newBackgroundRegistry
      tid <- spawnBackgroundProcess reg "." "echo hello-bg-task"
      threadDelay 100000 -- 100ms to allow command to finish and be reaped
      outRes <- getBackgroundOutput reg tid
      case outRes of
        ToolSuccess out -> T.unpack out `shouldContain` "hello-bg-task"
        ToolError err   -> expectationFailure ("Unexpected error: " ++ T.unpack err)

    it "stops running background process cleanly" $ do
      reg <- newBackgroundRegistry
      tid <- spawnBackgroundProcess reg "." "sleep 10"
      stopBackgroundProcess reg tid `shouldReturn` Right ()

  describe "Stopping kills the whole process tree (issue #220)" $ do
    it "kills processes the task's shell started" $ withScratch "grandchild" $ \dir -> do
      reg <- newBackgroundRegistry
      tid <- spawnBackgroundProcess reg dir "sleep 30 & echo $! > pid; wait"
      pid <- awaitPid (dir </> "pid")
      stopWithin reg tid `shouldReturn` Right ()
      isAlive pid `shouldReturn` False

    it "kills a task that ignores SIGTERM after the grace period" $ withScratch "trap" $ \dir -> do
      reg <- newBackgroundRegistry
      tid <- spawnBackgroundProcess reg dir "trap '' TERM; echo $$ > pid; while :; do sleep 1; done"
      pid <- awaitPid (dir </> "pid")
      stopWithin reg tid `shouldReturn` Right ()
      isAlive pid `shouldReturn` False

    it "stops every running task in the registry" $ withScratch "stop-all" $ \dir -> do
      reg <- newBackgroundRegistry
      _ <- spawnBackgroundProcess reg dir "sleep 30 & echo $! > pid1; wait"
      _ <- spawnBackgroundProcess reg dir "sleep 30 & echo $! > pid2; wait"
      pids <- mapM (awaitPid . (dir </>)) ["pid1", "pid2"]
      stopAllBackgroundProcesses reg
      mapM isAlive pids `shouldReturn` [False, False]
