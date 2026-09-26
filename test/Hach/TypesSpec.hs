{-# LANGUAGE OverloadedStrings #-}

module Hach.TypesSpec (spec) where

import Control.Monad (forM_)
import Hach.Types
import Test.Hspec

spec :: Spec
spec = describe "classifyError" $ do
  it "classifies transient timeout errors as transient" $ do
    let transientErrors =
          [ "504 Gateway Timeout: upstream request took too long"
          , "Temporary network error: connection took too long to establish"
          , "too long"
          ]
    forM_ transientErrors $ \err ->
      classifyError err `shouldBe` GoalErrTransient

  it "does not treat diagnostic context metadata as unrecoverable" $ do
    let transientErrors =
          [ "429 Rate limit exceeded: retry after 3s (context: tier-1 quota)"
          , "context: tier-1 provider"
          , "context"
          ]
    forM_ transientErrors $ \err ->
      classifyError err `shouldBe` GoalErrTransient

  it "retains unrecoverable classification for context overflow errors" $ do
    let unrecoverableErrors =
          [ "context length exceeded"
          , "context window exceeded"
          , "context overflow"
          , "maximum context length exceeded"
          , "prompt is too long"
          , "token limit exceeded"
          ]
    forM_ unrecoverableErrors $ \err ->
      classifyError err `shouldBe` GoalErrUnrecoverable

  it "does not treat unrelated overflow messages as unrecoverable" $ do
    classifyError "buffer overflow while reading response" `shouldBe` GoalErrTransient

  describe "headlessAskDeniedReason" $ do
    it "recommends acceptEdits for file writes in default mode" $ do
      let reason = headlessAskDeniedReason ModeDefault AuthorityWorkspaceWrite
      reason `shouldBe`
        "No interactive approval available in --no-tui. Re-run with --permission-mode acceptEdits to allow writes and commands."
      isHeadlessAskDeniedReason reason `shouldBe` True

    it "recommends dontAsk and not acceptEdits for commands in default mode" $ do
      let reason = headlessAskDeniedReason ModeDefault AuthorityCommand
      reason `shouldBe`
        "No interactive approval available in --no-tui. Re-run with --permission-mode dontAsk or add a matching permissions.allow rule."
      isHeadlessAskDeniedReason reason `shouldBe` True

    it "recommends dontAsk and not acceptEdits for commands in acceptEdits mode" $ do
      let reason = headlessAskDeniedReason ModeAcceptEdits AuthorityCommand
      reason `shouldBe`
        "No interactive approval available in --no-tui. Re-run with --permission-mode dontAsk or add a matching permissions.allow rule."
      isHeadlessAskDeniedReason reason `shouldBe` True

    it "never recommends acceptEdits when already in acceptEdits mode" $ do
      let reason = headlessAskDeniedReason ModeAcceptEdits AuthorityWorkspaceWrite
      reason `shouldBe`
        "No interactive approval available in --no-tui. Re-run with --permission-mode dontAsk or add a matching permissions.allow rule."
      isHeadlessAskDeniedReason reason `shouldBe` True

    it "recommends dontAsk for interaction authority" $ do
      let reason = headlessAskDeniedReason ModeDefault AuthorityInteraction
      reason `shouldBe`
        "No interactive approval available in --no-tui. Re-run with --permission-mode dontAsk or add a matching permissions.allow rule."

  describe "headlessAskBlockedMessage" $ do
    it "recommends acceptEdits when all denials were file writes in default mode" $ do
      headlessAskBlockedMessage ModeDefault [AuthorityWorkspaceWrite] `shouldBe`
        "Task blocked: every write and command was denied because --no-tui cannot prompt for approval. Re-run with --permission-mode acceptEdits."

    it "recommends dontAsk when a command was denied in default mode" $ do
      headlessAskBlockedMessage ModeDefault [AuthorityCommand] `shouldBe`
        "Task blocked: every write and command was denied because --no-tui cannot prompt for approval. Re-run with --permission-mode dontAsk or add a matching permissions.allow rule."

    it "recommends dontAsk for mixed write and command denials in default mode" $ do
      headlessAskBlockedMessage ModeDefault [AuthorityWorkspaceWrite, AuthorityCommand] `shouldBe`
        "Task blocked: every write and command was denied because --no-tui cannot prompt for approval. Re-run with --permission-mode dontAsk or add a matching permissions.allow rule."

    it "never recommends acceptEdits when already running in acceptEdits mode" $ do
      headlessAskBlockedMessage ModeAcceptEdits [AuthorityWorkspaceWrite] `shouldBe`
        "Task blocked: every write and command was denied because --no-tui cannot prompt for approval. Re-run with --permission-mode dontAsk or add a matching permissions.allow rule."
      headlessAskBlockedMessage ModeAcceptEdits [AuthorityCommand] `shouldBe`
        "Task blocked: every write and command was denied because --no-tui cannot prompt for approval. Re-run with --permission-mode dontAsk or add a matching permissions.allow rule."
      headlessAskBlockedMessage ModeAcceptEdits [AuthorityWorkspaceWrite, AuthorityCommand] `shouldBe`
        "Task blocked: every write and command was denied because --no-tui cannot prompt for approval. Re-run with --permission-mode dontAsk or add a matching permissions.allow rule."
