{-# LANGUAGE OverloadedStrings #-}

module Agent.TasksSpec (spec) where

import Agent.Tasks
import Agent.Types
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

  describe "Task serialization" $ do
    it "formats task list into readable summary" $ do
      let store0 = emptyTaskStore
          (_t1, store1) = createTask store0 "Test task"
          summary = formatTaskList (listTasks store1)
      T.unpack summary `shouldContain` "Test task"
      T.unpack summary `shouldContain` "pending"
