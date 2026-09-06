{-# LANGUAGE OverloadedStrings #-}

module Agent.SubagentsSpec (spec) where

import Agent.Subagents
import Agent.Types
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = describe "Agent.Subagents" $ do
  describe "Built-in agents" $ do
    it "provides built-in Explore agent" $ do
      let explore = builtinExploreAgent
      adName explore `shouldBe` "Explore"
      adTools explore `shouldContain` ["read_file"]
      adTools explore `shouldContain` ["grep_search"]

    it "provides built-in Plan agent" $ do
      let plan = builtinPlanAgent
      adName plan `shouldBe` "Plan"
      adTools plan `shouldContain` ["read_file"]

  describe "Agent definition frontmatter parsing" $ do
    it "parses custom agent markdown definition" $ do
      let raw = "---\n\
        \name: CodeReviewer\n\
        \description: Specialist in code reviews\n\
        \model: anthropic/claude-3-opus\n\
        \tools: read_file, grep_search\n\
        \---\n\
        \Review all code changes thoroughly and check for bugs.\n"
      case parseAgentDefinition "review.md" raw of
        Left err -> expectationFailure ("Failed to parse agent definition: " <> err)
        Right def -> do
          adName def `shouldBe` "CodeReviewer"
          adDescription def `shouldBe` "Specialist in code reviews"
          adModel def `shouldBe` Just "anthropic/claude-3-opus"
          adTools def `shouldBe` ["read_file", "grep_search"]
          adSystemPrompt def `shouldBe` "Review all code changes thoroughly and check for bugs."

  describe "Nesting and concurrency guards" $ do
    it "checks nesting depth limit" $ do
      canSpawnSubagent 3 3 0 `shouldBe` False
      canSpawnSubagent 2 3 0 `shouldBe` True

    it "checks concurrency limit" $ do
      canSpawnSubagent 1 3 20 `shouldBe` False
      canSpawnSubagent 1 3 19 `shouldBe` True
