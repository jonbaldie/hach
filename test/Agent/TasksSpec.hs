{-# LANGUAGE OverloadedStrings #-}

module Agent.TasksSpec (spec) where

import Agent.Tasks
import Agent.Types (TaskId(..), ToolResult(..))
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tasks" $ do
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
      stopped <- stopBackgroundProcess reg tid
      stopped `shouldBe` True
