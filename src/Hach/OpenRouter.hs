{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.OpenRouter
  ( ChatRequest(..)
  , sendChatCompletion
  , parseChatResponse
  ) where

import Hach.Types
import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value, object, withObject, (.:), (.:?), (.=)
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , Request(..)
  , RequestBody(RequestBodyLBS)
  , Response(..)
  , httpLbs
  , parseRequest
  )
import Network.HTTP.Types.Header (hAuthorization, hContentType)

-- | Outgoing chat completion request payload.
data ChatRequest = ChatRequest
  { reqModel      :: !Text
  , reqMessages   :: ![Message]
  , reqTools      :: ![ToolDef]
  , reqToolChoice :: !(Maybe Text)
  , reqEffort     :: !(Maybe EffortLevel)
  } deriving (Show, Eq)

instance ToJSON ChatRequest where
  toJSON ChatRequest{..} =
    let base = [ "model" .= reqModel
               , "messages" .= reqMessages
               ]
        toolsPart =
          if null reqTools
            then []
            else [ "tools" .= reqTools
                 , "tool_choice" .= (case reqToolChoice of
                                       Just tc -> tc
                                       Nothing -> "auto")
                 ]
        effortPart =
          case reqEffort of
            Just effort -> [ "reasoning" .= object ["effort" .= effort] ]
            Nothing     -> []
    in object (base ++ toolsPart ++ effortPart)

-- Helper wire types for OpenRouter response envelope
newtype ChoiceWire = ChoiceWire AssistantResponse

instance FromJSON ChoiceWire where
  parseJSON = withObject "ChoiceWire" $ \o ->
    ChoiceWire <$> o .: "message"

data OpenRouterEnvelope = OpenRouterEnvelope ![ChoiceWire] !(Maybe TokenUsage)

instance FromJSON OpenRouterEnvelope where
  parseJSON = withObject "OpenRouterEnvelope" $ \o -> do
    choices <- o .: "choices"
    mUsage <- o .:? "usage"
    mCost <- o .:? "cost"
    mTotalCost <- o .:? "total_cost"
    let mRootCost = mCost <|> mTotalCost
        finalUsage = case (mUsage, mRootCost) of
          (Just u, Just rc)
            | Nothing <- tuCost u -> Just u { tuCost = Just rc }
          (Just u, _) -> Just u
          (Nothing, Just rc) -> Just (TokenUsage 0 0 0 0 (Just rc))
          (Nothing, Nothing) -> Nothing
    pure (OpenRouterEnvelope choices finalUsage)

-- | Parse the response body from OpenRouter.
parseChatResponse :: LBS.ByteString -> Either Text AssistantResponse
parseChatResponse body =
  -- 1. Check if payload contains an OpenRouter error message
  case Aeson.decode body :: Maybe Value of
    Just (Aeson.Object o)
      | Just errMsg <- parseErrorPayload o ->
          Left ("OpenRouter API error: " <> errMsg)
    _ ->
      -- 2. Try parsing envelope
      case Aeson.eitherDecode body :: Either String OpenRouterEnvelope of
        Right (OpenRouterEnvelope (ChoiceWire msg : _) mUsage) ->
          Right msg { respUsage = mUsage }
        Right (OpenRouterEnvelope [] _) ->
          Left "OpenRouter returned empty choices array."
        Left envelopeErr ->
          -- 3. Fallback: try parsing directly as message
          case Aeson.eitherDecode body :: Either String AssistantResponse of
            Right directMsg
              | isNonEmptyResponse directMsg -> Right directMsg
              | otherwise -> Left ("JSON parse failure: " <> T.pack envelopeErr)
            Left _ -> Left ("JSON parse failure: " <> T.pack envelopeErr)
  where
    isNonEmptyResponse (AssistantResponse mContent calls mUsage) =
      maybe False (not . T.null . T.strip) mContent
        || not (null calls)
        || maybe False (const True) mUsage

    parseErrorPayload o =
      case AesonTypes.parseMaybe (\obj -> obj .: "error") o of
        Just (Aeson.String msg) -> Just msg
        Just (Aeson.Object errObj) ->
          AesonTypes.parseMaybe (\obj -> obj .: "message") errObj
            <|> AesonTypes.parseMaybe (\obj -> obj .: "detail") errObj
            <|> (fmap (\c -> "Error code " <> T.pack (show (c :: Int))) (AesonTypes.parseMaybe (\obj -> obj .: "code") errObj))
            <|> Just (TE.decodeUtf8 (LBS.toStrict (Aeson.encode errObj)))
        _ ->
          AesonTypes.parseMaybe (\obj -> obj .: "message") o

-- | Send an inference request to OpenRouter API.
sendChatCompletion
  :: Manager
  -> Text         -- ^ OpenRouter API Key
  -> ChatRequest
  -> IO (Either Text AssistantResponse)
sendChatCompletion mgr apiKey chatReq = do
  initReq <- parseRequest "https://openrouter.ai/api/v1/chat/completions"
  let bodyBytes = Aeson.encode chatReq
      req = initReq
        { method = "POST"
        , requestHeaders =
            [ (hAuthorization, "Bearer " <> TE.encodeUtf8 apiKey)
            , (hContentType, "application/json")
            , ("HTTP-Referer", "https://github.com/jonbaldie/agent")
            , ("X-Title", "Haskell Agentic Coding Harness")
            ]
        , requestBody = RequestBodyLBS bodyBytes
        }

  res <- try (httpLbs req mgr) :: IO (Either SomeException (Response LBS.ByteString))
  case res of
    Left ex -> pure $ Left ("HTTP request failed: " <> T.pack (show ex))
    Right response ->
      let body = responseBody response
      in pure (parseChatResponse body)
