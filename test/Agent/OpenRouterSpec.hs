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

    it "parses token usage metadata from response envelope" $ do
      let rawJson = LBS.concat
            [ "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"Hello there!\"}}],"
            , "\"usage\":{\"prompt_tokens\":42,\"completion_tokens\":18,\"total_tokens\":60}}"
            ]
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp -> do
          respContent resp `shouldBe` Just "Hello there!"
          respUsage resp `shouldBe` Just (mkTokenUsage 42 18 60)

    it "parses cached tokens and total_cost when present" $ do
      let rawJson = LBS.concat
            [ "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"Cache hit!\"}}],"
            , "\"usage\":{\"prompt_tokens\":100,\"completion_tokens\":20,\"total_tokens\":120,"
            , "\"prompt_tokens_details\":{\"cached_tokens\":80},\"total_cost\":0.0015}}"
            ]
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp ->
          respUsage resp `shouldBe` Just (TokenUsage 100 20 120 80 (Just 0.0015))

    it "parses cache_read_input_tokens and direct cost when present" $ do
      let rawJson = LBS.concat
            [ "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"Anthropic cache!\"}}],"
            , "\"usage\":{\"prompt_tokens\":200,\"completion_tokens\":50,\"total_tokens\":250,"
            , "\"cache_read_input_tokens\":150,\"cost\":0.003}}"
            ]
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp ->
          respUsage resp `shouldBe` Just (TokenUsage 200 50 250 150 (Just 0.003))

    it "sets respUsage to Nothing when usage field is absent" $ do
      let rawJson = "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"Hello there!\"}}]}"
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp -> do
          respUsage resp `shouldBe` Nothing

    it "rejects empty JSON object as empty success" $ do
      case parseChatResponse "{}" of
        Left _  -> pure ()
        Right r -> expectationFailure ("Expected Left for empty JSON, got Right " <> show r)

    it "rejects arbitrary non-envelope JSON as empty success" $ do
      case parseChatResponse "{\"foo\":\"bar\"}" of
        Left _  -> pure ()
        Right r -> expectationFailure ("Expected Left for non-envelope JSON, got Right " <> show r)

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
