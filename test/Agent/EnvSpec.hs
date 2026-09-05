{-# LANGUAGE OverloadedStrings #-}

module Agent.EnvSpec (spec) where

import Agent.Env
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "parseEnvContent" $ do
    it "parses standard key-value pairs" $ do
      let content = "KEY1=value1\nKEY2=value2\n"
          res = parseEnvContent content
      Map.lookup "KEY1" res `shouldBe` Just "value1"
      Map.lookup "KEY2" res `shouldBe` Just "value2"

    it "ignores comments and empty lines" $ do
      let content = "# comment\n\nKEY1=value1\n   # another comment\nKEY2=value2\n"
          res = parseEnvContent content
      Map.lookup "KEY1" res `shouldBe` Just "value1"
      Map.lookup "KEY2" res `shouldBe` Just "value2"
      Map.size res `shouldBe` 2

    it "strips quotes from values" $ do
      let content = "FOO=\"quoted value\"\nBAR='single quoted'\n"
          res = parseEnvContent content
      Map.lookup "FOO" res `shouldBe` Just "quoted value"
      Map.lookup "BAR" res `shouldBe` Just "single quoted"

  describe "parseLineTwoModel" $ do
    it "extracts the model from line two of .env" $ do
      let ls = [ "OPENROUTER_API_KEY=sk-test"
               , "OPENROUTER_MODEL=meta/muse-glimmer-30b"
               ]
      parseLineTwoModel ls `shouldBe` Just "meta/muse-glimmer-30b"

    it "handles whitespace and quotes on line two" $ do
      let ls = [ "OPENROUTER_API_KEY=sk-test"
               , "  OPENROUTER_MODEL=\"anthropic/claude-3\"  "
               ]
      parseLineTwoModel ls `shouldBe` Just "anthropic/claude-3"

    it "handles bare model string on line two" $ do
      let ls = [ "OPENROUTER_API_KEY=sk-test"
               , "openai/gpt-4o"
               ]
      parseLineTwoModel ls `shouldBe` Just "openai/gpt-4o"

    it "returns Nothing if fewer than two lines" $ do
      parseLineTwoModel ["OPENROUTER_API_KEY=sk-test"] `shouldBe` Nothing
