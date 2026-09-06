{-# LANGUAGE OverloadedStrings #-}

module Hach.HooksSpec (spec) where

import Hach.Hooks
import Hach.Types
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = describe "Hach.Hooks" $ do
  describe "Matcher filtering" $ do
    let h1 = HookHandler (HookCommand "echo 1") (Just "run_command") False
        h2 = HookHandler (HookCommand "echo 2") (Just "*") False
        h3 = HookHandler (HookCommand "echo 3") (Just "read_file") False
        handlers = [h1, h2, h3]

    it "selects matching handlers for a tool" $ do
      let matched = filterMatchingHandlers (Just "run_command") handlers
      map (hhType) matched `shouldBe` [HookCommand "echo 1", HookCommand "echo 2"]

    it "includes wildcard matcher for any tool" $ do
      let matched = filterMatchingHandlers (Just "write_file") handlers
      map (hhType) matched `shouldBe` [HookCommand "echo 2"]

  describe "Exit code protocol parsing" $ do
    it "exit code 0 returns pass with no modifications" $ do
      let res = parseHookOutput 0 "All checks passed"
      hrDecision res `shouldBe` Nothing
      hrAdditionalContext res `shouldBe` Nothing
      hrError res `shouldBe` Nothing

    it "exit code 2 parses structured JSON decision, context, and modified input" $ do
      let rawJson = "{\n\
        \  \"permissionDecision\": {\"decision\": \"deny\", \"reason\": \"Lint check failed\"},\n\
        \  \"additionalContext\": \"Fix lints first\",\n\
        \  \"modifiedToolInput\": {\"clean\": true}\n\
        \}"
          res = parseHookOutput 2 rawJson
      hrDecision res `shouldBe` Just (PermDeny "Lint check failed")
      hrAdditionalContext res `shouldBe` Just "Fix lints first"
      hrModifiedInput res `shouldBe` Just (object ["clean" .= True])

    it "non-0/2 exit code returns non-blocking error" $ do
      let res = parseHookOutput 1 "Command crashed"
      hrError res `shouldBe` Just "Command crashed"
      hrDecision res `shouldBe` Nothing
