{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Agent.Types
  ( -- * Roles and Messages
    Role(..)
  , ToolCall(..)
  , Message(..)
  , AssistantResponse(..)
  , parseCallArgs

    -- * Tools & Schemas
  , ToolDef(..)
  , ToolResult(..)
  , toolResultToText

    -- * Agent Configuration & Results
  , AgentConfig(..)
  , AgentResult(..)
  , AgentEvent(..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value, object, withObject, (.:), (.:?), (.!=), (.=)
  )
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Roles in the conversation.
data Role = RoleSystem | RoleUser | RoleAssistant | RoleTool
  deriving (Show, Eq, Enum, Bounded, Generic)

-- | A tool call emitted by the model.
data ToolCall = ToolCall
  { callId       :: !Text
  , functionName :: !Text
  , callArgsRaw  :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON ToolCall{..} = object
    [ "id" .= callId
    , "type" .= ("function" :: Text)
    , "function" .= object
        [ "name" .= functionName
        , "arguments" .= callArgsRaw
        ]
    ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \o -> do
    cid <- o .: "id"
    fObj <- o .: "function"
    fname <- fObj .: "name"
    rawArgs <- fObj .: "arguments"
    pure ToolCall
      { callId = cid
      , functionName = fname
      , callArgsRaw = rawArgs
      }

-- | Safely parse raw JSON string arguments into an Aeson 'Value'.
parseCallArgs :: ToolCall -> Either String Value
parseCallArgs tc = Aeson.eitherDecodeStrict (TE.encodeUtf8 (callArgsRaw tc))

-- | Individual message in the dialogue.
data Message
  = SystemMsg !Text
  | UserMsg !Text
  | AssistantMsg !(Maybe Text) ![ToolCall]
  | ToolMsg !Text !Text !Text -- callId, toolName, content
  deriving (Show, Eq, Generic)

instance ToJSON Message where
  toJSON = \case
    SystemMsg c -> object
      [ "role" .= ("system" :: Text)
      , "content" .= c
      ]
    UserMsg c -> object
      [ "role" .= ("user" :: Text)
      , "content" .= c
      ]
    AssistantMsg mContent calls ->
      if null calls
        then object
          [ "role" .= ("assistant" :: Text)
          , "content" .= mContent
          ]
        else object
          [ "role" .= ("assistant" :: Text)
          , "content" .= mContent
          , "tool_calls" .= calls
          ]
    ToolMsg cid name c -> object
      [ "role" .= ("tool" :: Text)
      , "tool_call_id" .= cid
      , "name" .= name
      , "content" .= c
      ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    role <- o .: "role"
    case (role :: Text) of
      "system" -> SystemMsg <$> o .: "content"
      "user" -> UserMsg <$> o .: "content"
      "assistant" -> AssistantMsg <$> o .:? "content" <*> (o .:? "tool_calls" .!= [])
      "tool" -> ToolMsg <$> o .: "tool_call_id" <*> (o .:? "name" .!= "") <*> o .: "content"
      other -> fail ("Unknown message role: " <> show other)

-- | The model's response for a turn.
data AssistantResponse = AssistantResponse
  { respContent   :: !(Maybe Text)
  , respToolCalls :: ![ToolCall]
  } deriving (Show, Eq, Generic)

instance ToJSON AssistantResponse where
  toJSON AssistantResponse{..} = object
    [ "content" .= respContent
    , "tool_calls" .= respToolCalls
    ]

instance FromJSON AssistantResponse where
  parseJSON = withObject "AssistantResponse" $ \o ->
    AssistantResponse
      <$> o .:? "content"
      <*> (o .:? "tool_calls" .!= [])

-- | Definition of a tool exposed to the model.
data ToolDef = ToolDef
  { toolName        :: !Text
  , toolDescription :: !Text
  , toolParameters  :: !Value
  } deriving (Show, Eq, Generic)

instance ToJSON ToolDef where
  toJSON ToolDef{..} = object
    [ "type" .= ("function" :: Text)
    , "function" .= object
        [ "name" .= toolName
        , "description" .= toolDescription
        , "parameters" .= toolParameters
        ]
    ]

instance FromJSON ToolDef where
  parseJSON = withObject "ToolDef" $ \o -> do
    fObj <- o .: "function"
    ToolDef
      <$> fObj .: "name"
      <*> fObj .: "description"
      <*> fObj .: "parameters"

-- | Result of executing a tool.
data ToolResult
  = ToolSuccess !Text
  | ToolError !Text
  deriving (Show, Eq, Generic)

instance ToJSON ToolResult where
  toJSON (ToolSuccess t) = object ["status" .= ("success" :: Text), "output" .= t]
  toJSON (ToolError err) = object ["status" .= ("error" :: Text), "error" .= err]

instance FromJSON ToolResult where
  parseJSON = withObject "ToolResult" $ \o -> do
    status <- o .: "status"
    case (status :: Text) of
      "success" -> ToolSuccess <$> o .: "output"
      "error"   -> ToolError <$> o .: "error"
      _         -> fail "Invalid ToolResult status"

-- | Render a tool result as text suitable for message content.
toolResultToText :: ToolResult -> Text
toolResultToText (ToolSuccess t) = t
toolResultToText (ToolError err) = "Error: " <> err

-- | Configuration parameters for the agent.
data AgentConfig = AgentConfig
  { cfgModel        :: !Text
  , cfgSystemPrompt :: !(Maybe Text)
  , cfgMaxTurns     :: !Int
  } deriving (Show, Eq)

-- | Final result of running the agent harness.
data AgentResult
  = AgentCompleted !Text
  | AgentMaxTurnsReached !Int
  | AgentFailed !Text
  deriving (Show, Eq)

-- | Events emitted during harness execution for telemetry / UI rendering.
data AgentEvent
  = EvTurnStart !Int
  | EvPromptingLLM !Int
  | EvLLMResponse !(Maybe Text) ![ToolCall]
  | EvToolCall !Text !Text
  | EvToolResult !Text !ToolResult
  | EvTurnComplete !Int
  | EvDone !Text
  | EvError !Text
  deriving (Show, Eq)
