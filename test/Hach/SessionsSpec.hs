{-# LANGUAGE OverloadedStrings #-}

module Hach.SessionsSpec (spec) where

import Hach.Core (AgentAlgebra(..))
import Hach.Interpreter.IO (ioAlgebra, newIOEnv)
import Hach.Permissions (isProtectedPath)
import Hach.Sessions
import Hach.Types
import Control.Exception (SomeException, try)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

import Control.Monad (when)

spec :: Spec
spec = describe "Hach.Sessions" $ do
  let testDir = "dist-newstyle/test-sessions"
  around_ (\action -> do
    exists <- doesDirectoryExist testDir
    when exists (removeDirectoryRecursive testDir)
    createDirectoryIfMissing True testDir
    action
    existsAfter <- doesDirectoryExist testDir
    when existsAfter (removeDirectoryRecursive testDir)) $ do

    it "uses protected .agents directory instead of .agent for session persistence in ioAlgebra" $ do
      env <- newIOEnv "dummy-key" "test-model" testDir False
      let alg = ioAlgebra env
          sInfo = SessionInfo "session-prot-1" "2026-09-06T12:00:00Z" "model-a" 1 0.01
      _ <- interpSaveSession alg sInfo
      let agentsDir = testDir </> ".agents" </> "sessions"
          agentDir  = testDir </> ".agent" </> "sessions"
      agentsSaved <- doesFileExist (agentsDir </> "session-prot-1.meta.json")
      agentSaved  <- doesDirectoryExist agentDir
      agentsSaved `shouldBe` True
      agentSaved `shouldBe` False
      isProtectedPath (agentsDir </> "session-prot-1.meta.json") `shouldBe` True
      -- Also verify loading via algebra
      mLoaded <- interpLoadSession alg "session-prot-1"
      case mLoaded of
        Just loadedInfo -> siId loadedInfo `shouldBe` "session-prot-1"
        Nothing -> expectationFailure "Expected session to load from .agents"

    it "keeps the interpreter alive when session persistence fails" $ do
      let blockedWorkspace = testDir </> "blocked-workspace"
          sInfo = SessionInfo "session-unwritable-1" "2026-09-06T12:00:00Z" "model-a" 1 0.01
      writeFile blockedWorkspace "blocked"
      env <- newIOEnv "dummy-key" "test-model" blockedWorkspace False
      result <- try (interpSaveSession (ioAlgebra env) sInfo) :: IO (Either SomeException Text)
      case result of
        Left ex -> expectationFailure ("Session persistence threw: " <> show ex)
        Right sid -> sid `shouldBe` siId sInfo


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
      let compacted = makeCompactedHistory (Just "System instructions") "Summary of tasks 1 and 2 completed."
      compacted `shouldBe`
        [ SystemMsg "System instructions"
        , UserMsg "[Context summary of earlier turns]:\nSummary of tasks 1 and 2 completed."
        ]

  describe "Workspace session management (Issue #151)" $ do
    let wsDir = "dist-newstyle/test-workspace-sessions"
    around_ (\action -> do
      exists <- doesDirectoryExist wsDir
      when exists (removeDirectoryRecursive wsDir)
      createDirectoryIfMissing True wsDir
      action
      existsAfter <- doesDirectoryExist wsDir
      when existsAfter (removeDirectoryRecursive wsDir)) $ do

      it "resolves session target from CLI flags correctly" $ do
        resolveSessionTarget False False Nothing `shouldBe` SessionNone
        resolveSessionTarget True False Nothing `shouldBe` SessionContinue
        resolveSessionTarget False True Nothing `shouldBe` SessionContinue
        resolveSessionTarget False False (Just "s-1") `shouldBe` SessionSpecific "s-1"
        resolveSessionTarget True False (Just "s-1") `shouldBe` SessionSpecific "s-1"

      it "reports failure when continuing without stored sessions in workspace" $ do
        res <- resolveSessionLoad wsDir SessionContinue
        res `shouldBe` Left "No stored session found in workspace."

      it "reports failure when loading non-existent specific session ID" $ do
        res <- resolveSessionLoad wsDir (SessionSpecific "ghost")
        res `shouldBe` Left "No stored session found for session ID: ghost"

      it "saves and loads workspace sessions end-to-end" $ do
        let sInfo = SessionInfo "ws-sess-1" "2026-09-18T10:00:00Z" "model-x" 1 0.01
            msgs = [UserMsg "First question", AssistantMsg (Just "First answer") []]
        saveWorkspaceSession wsDir sInfo msgs
        latestId <- getLatestWorkspaceSessionId wsDir
        latestId `shouldBe` Just "ws-sess-1"

        loadRes <- resolveSessionLoad wsDir SessionContinue
        case loadRes of
          Left err -> expectationFailure ("Failed to load session: " <> err)
          Right Nothing -> expectationFailure "Expected Just session"
          Right (Just (loadedInfo, loadedMsgs)) -> do
            siId loadedInfo `shouldBe` "ws-sess-1"
            loadedMsgs `shouldBe` msgs

      it "builds session history combining system prompt, prior turns, and new prompt" $ do
        let prior = [SystemMsg "old sys", UserMsg "q1", AssistantMsg (Just "a1") []]
            built = buildSessionHistory "new sys" (Just prior) "q2"
        built `shouldBe`
          [ SystemMsg "new sys"
          , UserMsg "q1"
          , AssistantMsg (Just "a1") []
          , UserMsg "q2"
          ]

      it "drops trailing non-assistant messages from prior history when building new turn" $ do
        let prior = [SystemMsg "old sys", UserMsg "q1", AssistantMsg (Just "a1") [], UserMsg "unanswered"]
            built = buildSessionHistory "new sys" (Just prior) "q2"
        built `shouldBe`
          [ SystemMsg "new sys"
          , UserMsg "q1"
          , AssistantMsg (Just "a1") []
          , UserMsg "q2"
          ]

  describe "Cost estimation (BUG-7)" $ do
    it "correctly prices gpt-4o-mini without shadowing from gpt-4o" $ do
      -- 1M prompt ($0.15) + 1M completion ($0.60) = $0.75
      estimateCostUsd "openai/gpt-4o-mini" 1000000 1000000 `shouldBe` 0.75
      -- 1M prompt ($5.00) + 1M completion ($15.00) = $20.00
      estimateCostUsd "openai/gpt-4o" 1000000 1000000 `shouldBe` 20.0

    it "prices generic gpt-4 without shadowing gpt-4o families" $ do
      -- 1M prompt ($2.50) + 1M completion ($10.00) = $12.50
      estimateCostUsd "openai/gpt-4" 1000000 1000000 `shouldBe` 12.5
      estimateCostUsd "openai/gpt-4-turbo" 1000000 1000000 `shouldBe` 12.5
      estimateCostUsd "openai/gpt-4o-mini" 1000000 1000000 `shouldBe` 0.75
      estimateCostUsd "openai/gpt-4o" 1000000 1000000 `shouldBe` 20.0
