{-# LANGUAGE OverloadedStrings #-}

module Hach.OpenRouterSpec (spec) where

import Hach.OpenRouter
import Hach.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
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

    it "extracts error details when error envelope lacks message field" $ do
      let rawJson1 = "{\"error\":{\"code\":429,\"metadata\":{\"raw\":\"Rate limit exceeded\"}}}"
      case parseChatResponse rawJson1 of
        Left err -> ("OpenRouter API error:" `T.isPrefixOf` err) `shouldBe` True
        Right _  -> expectationFailure "Expected parseChatResponse to fail on API error"

      let rawJson2 = "{\"error\":{\"code\":503,\"detail\":\"Service unavailable\"}}"
      case parseChatResponse rawJson2 of
        Left err -> err `shouldBe` "OpenRouter API error: Service unavailable"
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

    it "parses actual cost from cost_details.upstream_inference_cost when cost is absent" $ do
      let rawJson = LBS.concat
            [ "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"BYOK cost!\"}}],"
            , "\"usage\":{\"prompt_tokens\":100,\"completion_tokens\":25,\"total_tokens\":125,"
            , "\"cost_details\":{\"upstream_inference_cost\":0.00045}}}"
            ]
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp ->
          respUsage resp `shouldBe` Just (TokenUsage 100 25 125 0 (Just 0.00045))

    it "parses actual cost from prompt and completion cost_details when cost is absent" $ do
      let rawJson = LBS.concat
            [ "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"Detailed cost!\"}}],"
            , "\"usage\":{\"prompt_tokens\":100,\"completion_tokens\":25,\"total_tokens\":125,"
            , "\"cost_details\":{\"upstream_inference_prompt_cost\":0.0003,\"upstream_inference_completions_cost\":0.00015}}}"
            ]
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp ->
          respUsage resp `shouldBe` Just (TokenUsage 100 25 125 0 (Just 0.00045))

    it "parses root-level cost when usage object has no cost" $ do
      let rawJson = LBS.concat
            [ "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"Root cost!\"}}],"
            , "\"usage\":{\"prompt_tokens\":50,\"completion_tokens\":10,\"total_tokens\":60},"
            , "\"cost\":0.00075}"
            ]
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp ->
          respUsage resp `shouldBe` Just (TokenUsage 50 10 60 0 (Just 0.00075))

    it "parses root-level total_cost when usage object has no cost" $ do
      let rawJson = LBS.concat
            [ "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"Root total cost!\"}}],"
            , "\"usage\":{\"prompt_tokens\":50,\"completion_tokens\":10,\"total_tokens\":60},"
            , "\"total_cost\":0.0009}"
            ]
      case parseChatResponse rawJson of
        Left err -> expectationFailure ("Failed to parse response: " <> show err)
        Right resp ->
          respUsage resp `shouldBe` Just (TokenUsage 50 10 60 0 (Just 0.0009))

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
