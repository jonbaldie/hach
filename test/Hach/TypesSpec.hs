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
