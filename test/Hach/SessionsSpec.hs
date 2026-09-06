{-# LANGUAGE OverloadedStrings #-}

module Hach.SessionsSpec (spec) where

import Hach.Sessions
import Hach.Types
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import Test.Hspec

spec :: Spec
spec = describe "Hach.Sessions" $ do
  let testDir = "dist-newstyle/test-sessions"
  around_ (\action -> do
    createDirectoryIfMissing True testDir
    action
    removeDirectoryRecursive testDir) $ do

    it "saves and reloads a session transcript faithfully" $ do
      let sInfo = SessionInfo
            { siId = "test-session-123"
            , siCreatedAt = "2026-09-06T10:00:00Z"
            , siModel = "anthropic/claude-3-opus"
            , siTurns = 2
            , siCostUsd = 0.05
            }
          history =
            [ SystemMsg "You are an assistant."
            , UserMsg "Hello world"
            , AssistantMsg (Just "Greetings!") []
            ]
      saveSession testDir sInfo history
      mLoaded <- loadSession testDir "test-session-123"
      case mLoaded of
        Nothing -> expectationFailure "Failed to reload saved session"
        Just (loadedInfo, loadedHistory) -> do
          siId loadedInfo `shouldBe` "test-session-123"
          siModel loadedInfo `shouldBe` "anthropic/claude-3-opus"
          loadedHistory `shouldBe` history

    it "lists recorded sessions ordered by recency" $ do
      let s1 = SessionInfo "s1" "2026-09-06T10:00:00Z" "model-a" 1 0.01
          s2 = SessionInfo "s2" "2026-09-06T11:00:00Z" "model-a" 2 0.02
      saveSession testDir s1 [UserMsg "First"]
      saveSession testDir s2 [UserMsg "Second"]
      latestId <- getLatestSessionId testDir
      latestId `shouldBe` Just "s2"

  describe "Compaction" $ do
    it "replaces history with summary while preserving system prompt" $ do
      let orig =
            [ SystemMsg "System instructions"
            , UserMsg "Do task 1"
            , AssistantMsg (Just "Done task 1") []
            , UserMsg "Do task 2"
            , AssistantMsg (Just "Done task 2") []
            ]
          compacted = makeCompactedHistory (Just "System instructions") "Summary of tasks 1 and 2 completed."
      compacted `shouldBe`
        [ SystemMsg "System instructions"
        , UserMsg "[Context summary of earlier turns]:\nSummary of tasks 1 and 2 completed."
        ]

  describe "Cost estimation (BUG-7)" $ do
    it "correctly prices gpt-4o-mini without shadowing from gpt-4o" $ do
      -- 1M prompt ($0.15) + 1M completion ($0.60) = $0.75
      estimateCostUsd "openai/gpt-4o-mini" 1000000 1000000 `shouldBe` 0.75
      -- 1M prompt ($5.00) + 1M completion ($15.00) = $20.00
      estimateCostUsd "openai/gpt-4o" 1000000 1000000 `shouldBe` 20.0
