{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The single inference boundary. Hach speaks one wire protocol, the
-- non-streaming Chat Completions contract, in two dialects: OpenRouter's
-- (the default) and the plain OpenAI-compatible one. A connection, resolved
-- once at startup, names the dialect, the endpoint, and an optional key.
module Hach.Inference
  ( -- * Interfaces and connections
    InferenceInterface(..)
  , interfaceName
  , interfaceLabel
  , parseInferenceInterface
  , InferenceConnection(..)
  , openRouterBaseUrl
  , openAIBaseUrl
  , mkInferenceConnection
  , openRouterConnection
  , chatCompletionsEndpoint
  , validateApiKey
  , CostPolicy(..)
  , connectionCostPolicy
    -- * Requests and responses
  , ChatRequest(..)
  , encodeChatRequest
  , parseChatResponse
  , parseCompletion
    -- * Transport
  , newInferenceManager
  , sendInference
  , diagnosticExcerpt
  , maxDiagnosticLength
  ) where

import Hach.Types
import Control.Applicative ((<|>))
import Control.Exception
  ( SomeAsyncException, SomeException, displayException, fromException
  , throwIO, toException, try
  )
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value, object, withObject, (.:), (.:?), (.=)
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isAscii, isControl, isDigit, isHexDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Network.HTTP.Client
  ( HttpException(..)
  , HttpExceptionContent(..)
  , Manager
  , Request(..)
  , RequestBody(RequestBodyLBS)
  , Response(..)
  , httpLbs
  , newManager
  , parseRequest
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.HTTP.Types.Status (statusCode, statusMessage)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Interfaces and connections
--------------------------------------------------------------------------------

-- | The request dialect Hach speaks to its inference endpoint.
data InferenceInterface
  = InterfaceOpenRouter
  | InterfaceOpenAICompatible
  deriving (Show, Eq, Enum, Bounded)

-- | Canonical configuration name, as accepted by @--provider@.
interfaceName :: InferenceInterface -> Text
interfaceName InterfaceOpenRouter       = "openrouter"
interfaceName InterfaceOpenAICompatible = "openai-compatible"

-- | Human-readable name used in diagnostics.
interfaceLabel :: InferenceInterface -> Text
interfaceLabel InterfaceOpenRouter       = "OpenRouter"
interfaceLabel InterfaceOpenAICompatible = "OpenAI-compatible API"

-- | Parse a configured interface name. Surrounding whitespace is ignored;
-- anything but a canonical name, including a blank value, is rejected.
parseInferenceInterface :: Text -> Either String InferenceInterface
parseInferenceInterface raw =
  case lookup (T.strip raw) [ (interfaceName i, i) | i <- [minBound ..] ] of
    Just iface -> Right iface
    Nothing
      | T.null (T.strip raw) -> Left ("provider is blank. " <> supported)
      | otherwise -> Left ("unknown provider " <> show (T.strip raw) <> ". " <> supported)
  where
    supported = "Supported providers: openrouter, openai-compatible."

-- | Everything the transport needs to reach one endpoint. The key is never
-- shown.
data InferenceConnection = InferenceConnection
  { icInterface :: !InferenceInterface
  , icEndpoint  :: !Text          -- ^ Complete Chat Completions URL.
  , icApiKey    :: !(Maybe Text)  -- ^ Bearer token; 'Nothing' sends none.
  } deriving (Eq)

instance Show InferenceConnection where
  show InferenceConnection{..} =
    "InferenceConnection " <> show icInterface <> " " <> show icEndpoint
      <> maybe " <no key>" (const " <key redacted>") icApiKey

-- | Default OpenRouter API root.
openRouterBaseUrl :: Text
openRouterBaseUrl = "https://openrouter.ai/api/v1"

-- | Default API root in OpenAI-compatible mode.
openAIBaseUrl :: Text
openAIBaseUrl = "https://api.openai.com/v1"

-- | Build a connection from an API root and optional key, validating both
-- before any request can be made.
mkInferenceConnection
  :: InferenceInterface -> Text -> Maybe Text -> Either String InferenceConnection
mkInferenceConnection iface baseUrl mKey =
  InferenceConnection iface
    <$> chatCompletionsEndpoint baseUrl
    <*> traverse validateApiKey mKey

-- | The default OpenRouter connection for a key.
openRouterConnection :: Text -> InferenceConnection
openRouterConnection key =
  InferenceConnection InterfaceOpenRouter (openRouterBaseUrl <> "/chat/completions") (Just key)

-- | A key goes into a header verbatim, so a line break would split it.
validateApiKey :: Text -> Either String Text
validateApiKey key
  | T.any (`elem` ("\r\n" :: String)) key = Left "the API key contains a line break."
  | otherwise = Right key

-- | Turn an API root into its Chat Completions URL. The root must be an
-- absolute http(s) URL with a host and nothing that could smuggle
-- credentials or change the route: no userinfo, query, or fragment. Trailing
-- slashes are dropped and @/chat/completions@ is appended; any version or
-- gateway prefix is kept as given.
chatCompletionsEndpoint :: Text -> Either String Text
chatCompletionsEndpoint raw = do
  let url = T.strip raw
  when' (T.null url) "the base URL is blank."
  when' (T.any (\c -> isSpace c || isControl c) url)
    "the base URL contains whitespace or control characters."
  (scheme, rest) <- splitScheme url
  let (authority, path) = T.break (== '/') rest
  when' (T.any (== '?') rest) "the base URL must not contain a query string."
  when' (T.any (== '#') rest) "the base URL must not contain a fragment."
  when' (T.any (== '@') authority) "the base URL must not contain user credentials."
  validateAuthority authority
  validatePath path
  let root = T.dropWhileEnd (== '/') path
  when' ("/chat/completions" `T.isSuffixOf` T.toLower root)
    "give the API root (for example http://localhost:8080/v1), not the /chat/completions route; hach appends it."
  let endpoint = scheme <> "://" <> authority <> root <> "/chat/completions"
  case parseRequest (T.unpack endpoint) :: Maybe Request of
    Just _  -> Right endpoint
    Nothing -> Left "the base URL is not a valid URL."
  where
    when' cond msg = if cond then Left (baseUrlError msg) else Right ()
    baseUrlError msg = "Invalid base URL " <> show (T.strip raw) <> ": " <> msg

    splitScheme url =
      case T.breakOn "://" url of
        (s, r) | not (T.null r), T.toLower s `elem` ["http", "https"] ->
          Right (T.toLower s, T.drop 3 r)
        _ -> Left (baseUrlError "it must be an absolute http:// or https:// URL.")

    validateAuthority authority = do
      (host, portPart) <- case T.uncons authority of
        Just ('[', v6) -> case T.breakOn "]" v6 of
          (addr, close)
            | not (T.null close), not (T.null addr)
            , T.all (\c -> isHexDigit c || c `elem` (":." :: String)) addr ->
                Right (addr, T.drop 1 close)
          _ -> Left (baseUrlError "the IPv6 host is malformed.")
        _ -> do
          let (h, p) = T.break (== ':') authority
          when' (T.any (not . isHostChar) h) "the host contains invalid characters."
          Right (h, p)
      when' (T.null host) "the base URL has no host."
      case T.uncons portPart of
        Nothing -> Right ()
        Just (':', port)
          | not (T.null port), T.all isDigit port
          , Just n <- readMaybe (T.unpack port) :: Maybe Int
          , n >= 1, n <= 65535 -> Right ()
        _ -> Left (baseUrlError "the port is malformed.")

    isHostChar c = isAscii c && (isAlphaNum c || c `elem` ("-._~" :: String))

    validatePath path = go (T.unpack path)
      where
        go [] = Right ()
        go ('%' : a : b : cs) | isHexDigit a && isHexDigit b = go cs
        go ('%' : _) = Left (baseUrlError "the path contains a malformed percent escape.")
        go (c : cs)
          | isAscii c && (isAlphaNum c || c `elem` ("/-._~!$&'()*+,;=:@" :: String)) = go cs
          | otherwise = Left (baseUrlError "the path contains characters that must be percent-encoded.")

-- | How the TUI accounts for cost the endpoint does not report. OpenRouter
-- prices are estimated from the model name; an arbitrary compatible endpoint
-- has no known price, so a missing cost stays unknown.
data CostPolicy
  = CostEstimateFromModel
  | CostReportedOnly
  deriving (Show, Eq)

connectionCostPolicy :: InferenceConnection -> CostPolicy
connectionCostPolicy conn = case icInterface conn of
  InterfaceOpenRouter       -> CostEstimateFromModel
  InterfaceOpenAICompatible -> CostReportedOnly

--------------------------------------------------------------------------------
-- Requests
--------------------------------------------------------------------------------

-- | Outgoing chat completion request payload.
data ChatRequest = ChatRequest
  { reqModel      :: !Text
  , reqMessages   :: ![Message]
  , reqTools      :: ![ToolDef]
  , reqToolChoice :: !(Maybe Text)
  , reqEffort     :: !(Maybe EffortLevel)
  } deriving (Show, Eq)

-- | The OpenRouter encoding, kept as the default instance.
instance ToJSON ChatRequest where
  toJSON = encodeChatRequest InterfaceOpenRouter

-- | Encode a request in the interface's dialect. The dialects differ only in
-- how a configured effort is spelled; an unset effort sends neither form.
encodeChatRequest :: InferenceInterface -> ChatRequest -> Value
encodeChatRequest iface ChatRequest{..} =
  object (base ++ toolsPart ++ effortPart)
  where
    base = [ "model" .= reqModel, "messages" .= reqMessages ]
    toolsPart
      | null reqTools = []
      | otherwise =
          [ "tools" .= reqTools
          , "tool_choice" .= maybe "auto" id reqToolChoice
          ]
    effortPart = case (iface, reqEffort) of
      (_, Nothing) -> []
      (InterfaceOpenRouter, Just effort) -> [ "reasoning" .= object ["effort" .= effort] ]
      (InterfaceOpenAICompatible, Just effort) -> [ "reasoning_effort" .= effort ]

--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

-- Helper wire types for the Chat Completions response envelope.
data ChoiceWire = ChoiceWire !AssistantResponse !(Maybe Text) !(Maybe Text)

instance FromJSON ChoiceWire where
  parseJSON = withObject "ChoiceWire" $ \o -> do
    msg <- o .: "message"
    refusal <- withObject "message" (\m -> m .:? "refusal") msg
    ChoiceWire <$> parseJSON msg <*> pure refusal <*> o .:? "finish_reason"

data CompletionEnvelope = CompletionEnvelope ![ChoiceWire] !(Maybe TokenUsage)

instance FromJSON CompletionEnvelope where
  parseJSON = withObject "CompletionEnvelope" $ \o -> do
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
    pure (CompletionEnvelope choices finalUsage)

-- | Parse an OpenRouter response body.
parseChatResponse :: LBS.ByteString -> Either Text AssistantResponse
parseChatResponse = parseCompletion InterfaceOpenRouter

-- | Parse a successful (2xx) response body in the interface's dialect. An
-- error envelope is a failure whatever the status. OpenRouter keeps its
-- tolerated standalone-message fallback; compatible mode requires the
-- standard choices envelope and a first message that says something.
parseCompletion :: InferenceInterface -> LBS.ByteString -> Either Text AssistantResponse
parseCompletion iface body =
  case Aeson.decode body :: Maybe Value of
    Just (Aeson.Object o)
      | Just errMsg <- errorPayloadMessage o ->
          Left (apiErrorPrefix iface <> ": " <> errMsg)
    _ -> case Aeson.eitherDecode body of
      Right (CompletionEnvelope (ChoiceWire msg refusal finish : _) mUsage) ->
        let resp = msg { respUsage = mUsage }
        in case iface of
             InterfaceOpenRouter -> Right resp
             InterfaceOpenAICompatible -> usableCompletion resp refusal finish
      Right (CompletionEnvelope [] _) ->
        Left (interfaceLabel iface <> " returned empty choices array.")
      Left envelopeErr -> case iface of
        InterfaceOpenRouter
          | Right directMsg <- Aeson.eitherDecode body
          , isNonEmptyResponse directMsg -> Right directMsg
        _ -> Left (interfaceLabel iface <> " response JSON parse failure: " <> T.pack envelopeErr)
  where
    isNonEmptyResponse (AssistantResponse mContent calls mUsage) =
      hasText mContent || not (null calls) || maybe False (const True) mUsage

    hasText = maybe False (not . T.null . T.strip)

    usableCompletion resp refusal finish
      | hasText (respContent resp) || not (null (respToolCalls resp)) = Right resp
      | Just r <- refusal, not (T.null (T.strip r)) =
          Left (interfaceLabel iface <> " refused the request: " <> T.strip r)
      | finish == Just "content_filter" =
          Left (interfaceLabel iface <> " blocked the completion with a content filter.")
      | otherwise =
          Left (interfaceLabel iface <> " returned a completion with no text or tool calls.")

apiErrorPrefix :: InferenceInterface -> Text
apiErrorPrefix InterfaceOpenRouter       = "OpenRouter API error"
apiErrorPrefix InterfaceOpenAICompatible = "OpenAI-compatible API error"

-- | The useful message in an error envelope, if the body is one.
errorPayloadMessage :: Aeson.Object -> Maybe Text
errorPayloadMessage o =
  case AesonTypes.parseMaybe (.: "error") o of
    Just (Aeson.String msg) -> Just msg
    Just (Aeson.Object errObj) ->
      let mMsg = AesonTypes.parseMaybe (.: "message") errObj
          mDetail = AesonTypes.parseMaybe (.: "detail") errObj
          mRaw = fmap innerErrorText $
            AesonTypes.parseMaybe (\obj -> obj .: "metadata" >>= (.: "raw")) errObj
          mCode = fmap (\c -> "Error code " <> T.pack (show (c :: Int)))
            (AesonTypes.parseMaybe (.: "code") errObj)
      in case (mMsg, mRaw) of
           (Just msg, Just inner)
             | not (T.null inner) && inner /= msg -> Just (msg <> ": " <> inner)
           (Just msg, _) -> Just msg
           (Nothing, Just inner) | not (T.null inner) -> Just inner
           _ -> mDetail <|> mCode <|> Just (TE.decodeUtf8 (LBS.toStrict (Aeson.encode errObj)))
    _ -> AesonTypes.parseMaybe (.: "message") o
  where
    innerErrorText raw =
      case Aeson.decode (LBS.fromStrict (TE.encodeUtf8 raw)) :: Maybe Value of
        Just (Aeson.Object innerObj) ->
          case AesonTypes.parseMaybe nestedErrorMessage innerObj of
            Just nested | not (T.null nested) -> nested
            _ -> raw
        _ -> raw

    nestedErrorMessage obj =
      (do
          err <- obj .: "error"
          case err of
            Aeson.String m -> pure m
            Aeson.Object e -> e .: "message"
            _ -> fail "no nested error message")
      <|> obj .: "message"

--------------------------------------------------------------------------------
-- Transport
--------------------------------------------------------------------------------

-- | The HTTP manager shared by every inference request.
newInferenceManager :: IO Manager
newInferenceManager = newManager tlsManagerSettings

-- | Send one request over the connection. There is exactly one attempt: no
-- redirect is followed, nothing is retried, and nothing falls back to
-- another endpoint. Every diagnostic is bounded, stripped of terminal
-- controls, and has the connection's key redacted. Asynchronous exceptions
-- (cancellation) propagate rather than becoming an API error.
sendInference :: Manager -> InferenceConnection -> ChatRequest -> IO (Either Text AssistantResponse)
sendInference mgr InferenceConnection{..} chatReq = do
  initReq <- parseRequest (T.unpack icEndpoint)
  let req = initReq
        { method = "POST"
        , requestHeaders = authHeader ++ (hContentType, "application/json") : attributionHeaders
        , requestBody = RequestBodyLBS (Aeson.encode (encodeChatRequest icInterface chatReq))
        , redirectCount = 0
        , checkResponse = \_ _ -> pure ()
        }
  res <- try (httpLbs req mgr)
  case res of
    Left ex
      | Just (_ :: SomeAsyncException) <- fromException ex -> throwIO ex
      | otherwise -> pure (Left (safe (requestFailure ex)))
    Right response -> pure (either (Left . safe) Right (classify response))
  where
    authHeader = [ (hAuthorization, "Bearer " <> TE.encodeUtf8 k) | Just k <- [icApiKey] ]
    attributionHeaders = case icInterface of
      InterfaceOpenRouter ->
        [ ("HTTP-Referer", "https://github.com/jonbaldie/agent")
        , ("X-Title", "Haskell Agentic Coding Harness")
        ]
      InterfaceOpenAICompatible -> []

    safe = sanitizeWith icApiKey
    label = interfaceLabel icInterface

    classify response
      | code >= 200 && code < 300 = parseCompletion icInterface body
      | code >= 300 && code < 400 =
          Left (statusPrefix <> ": redirect refused; inference requests are never redirected. Check the base URL.")
      | otherwise = Left (statusPrefix <> ": " <> errorDetail)
      where
        status = responseStatus response
        code = statusCode status
        body = responseBody response
        statusPrefix = apiErrorPrefix icInterface <> " (HTTP " <> T.pack (show code) <> ")"
        errorDetail = case Aeson.decode body of
          Just (Aeson.Object o) | Just msg <- errorPayloadMessage o -> msg
          _ | LBS.null body -> decodeLenient (LBS.fromStrict (statusMessage status))
            | otherwise -> diagnosticExcerpt icApiKey (decodeLenient body)

    requestFailure :: SomeException -> Text
    requestFailure ex = label <> " request failed: " <> case fromException ex of
      Just (HttpExceptionRequest _ content) -> case content of
        ConnectionFailure inner -> "could not connect to " <> icEndpoint <> " (" <> T.pack (displayException inner) <> ")"
        ConnectionTimeout -> "timed out connecting to " <> icEndpoint
        ResponseTimeout -> "timed out waiting for a response from " <> icEndpoint
        TooManyRedirects _ -> "redirect refused; inference requests are never redirected. Check the base URL."
        other -> T.pack (takeWhile (/= ' ') (show other)) <> " while contacting " <> icEndpoint
      Just (InvalidUrlException _ reason) -> "invalid URL (" <> T.pack reason <> ")"
      Nothing -> T.pack (displayException (toException ex))

-- | Longest transport-generated diagnostic.
maxDiagnosticLength :: Int
maxDiagnosticLength = 2048

-- | Make text from the network safe to show and store: redact the key first
-- (so truncation cannot leave a fragment of it), then neutralise terminal
-- control characters, then bound the length.
diagnosticExcerpt :: Maybe Text -> Text -> Text
diagnosticExcerpt = sanitizeWith

sanitizeWith :: Maybe Text -> Text -> Text
sanitizeWith mKey = bound . T.map neutralise . redact
  where
    redact t = case mKey of
      Just k | not (T.null k) -> T.replace k "[REDACTED]" t
      _ -> t
    neutralise c = if isControl c then ' ' else c
    marker = " …[truncated]"
    bound t
      | T.length t <= maxDiagnosticLength = t
      | otherwise = T.take (maxDiagnosticLength - T.length marker) t <> marker

decodeLenient :: LBS.ByteString -> Text
decodeLenient = TE.decodeUtf8With TEE.lenientDecode . LBS.toStrict
