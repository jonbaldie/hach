{-# LANGUAGE OverloadedStrings #-}

module Agent.ToolsSpec (spec) where

import Agent.Tools
import Agent.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "Tool argument parsing" $ do
    it "parses valid read_file arguments" $ do
      let call = ToolCall "c1" "read_file" "{\"path\":\"src/Main.hs\"}"
      parseReadFileArgs call `shouldBe` Right (ReadFileArgs "src/Main.hs")

    it "rejects read_file arguments missing 'path'" $ do
      let call = ToolCall "c1" "read_file" "{\"wrong\":\"value\"}"
      case parseReadFileArgs call of
        Left _  -> pure ()
        Right _ -> expectationFailure "Expected parse failure when 'path' is missing"

    it "parses valid write_file arguments" $ do
      let call = ToolCall "c2" "write_file" "{\"path\":\"test.txt\",\"content\":\"hello\"}"
      parseWriteFileArgs call `shouldBe` Right (WriteFileArgs "test.txt" "hello")

    it "parses valid run_command arguments" $ do
      let call = ToolCall "c3" "run_command" "{\"command\":\"echo 42\"}"
      parseRunCommandArgs call `shouldBe` Right (RunCommandArgs "echo 42")

    it "parses list_dir with explicit path" $ do
      let call = ToolCall "c4" "list_dir" "{\"path\":\"src\"}"
      parseListDirArgs call `shouldBe` Right (ListDirArgs "src")

    it "parses list_dir defaulting to '.'" $ do
      let call = ToolCall "c4" "list_dir" "{}"
      parseListDirArgs call `shouldBe` Right (ListDirArgs ".")

  describe "allToolDefs" $ do
    it "contains all four coding tools" $ do
      let names = map toolName allToolDefs
      names `shouldContain` ["read_file", "write_file", "run_command", "list_dir"]
