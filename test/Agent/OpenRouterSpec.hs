{-# LANGUAGE OverloadedStrings #-}

module Agent.OpenRouterSpec (spec) where

import Agent.OpenRouter
import Agent.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec

spec :: Spec
spec = do
  describe "parseChatResponse" $ do
    it "parses standard text assistant completion" $ do
      let rawJson = "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"Hello there!\"}}]}"
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp -> do
          respContent resp `shouldBe` Just "Hello there!"
          respToolCalls resp `shouldBe` []

    it "parses tool call response envelope" $ do
      let rawJson = LBS.concat
            [ "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,"
            , "\"tool_calls\":[{\"id\":\"call_abc\",\"type\":\"function\","
            , "\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"Main.hs\\\"}\"}}]}}]}"
            ]
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp -> do
          respContent resp `shouldBe` Nothing
          case respToolCalls resp of
            (tc : _) -> do
              callId tc `shouldBe` "call_abc"
              functionName tc `shouldBe` "read_file"
              callArgsRaw tc `shouldBe` "{\"path\":\"Main.hs\"}"
            [] -> expectationFailure "Expected at least one tool call"

    it "extracts error message from API error envelope" $ do
      let rawJson = "{\"error\":{\"message\":\"Invalid API key provided\",\"code\":401}}"
      case parseChatResponse rawJson of
        Left err -> err `shouldBe` "OpenRouter API error: Invalid API key provided"
        Right _  -> expectationFailure "Expected parseChatResponse to fail on API error"

  describe "ChatRequest serialization" $ do
    it "omits tools when list is empty" $ do
      let req = ChatRequest "test-model" [UserMsg "Hello"] [] Nothing
          jsonVal = Aeson.toJSON req
      case jsonVal of
        Aeson.Object o ->
          case KeyMap.lookup "tools" o of
            Just _  -> expectationFailure "tools should be omitted when empty"
            Nothing -> pure ()
        _ -> expectationFailure "Expected Object"
