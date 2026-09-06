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
  , TokenUsage(..)

    -- * Tools & Schemas
  , ToolDef(..)
  , ToolResult(..)
  , toolResultToText

    -- * Goal Evaluation
  , GoalVerdict(..)
  , GoalEvaluation(..)
  , GoalStatus(..)
  , GoalState(..)
  , initialGoalState
  , GoalErrorKind(..)
  , classifyCompletion
  , classifyError
  , unrecoverableKeywords

    -- * /goal Command Argument Parsing
  , goalClearAliases
  , maxGoalConditionLength
  , goalArgIsClear

    -- * Agent Configuration & Results
  , AgentConfig(..)
  , AgentResult(..)
  , AgentEvent(..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value, object, withObject, (.:), (.:?), (.!=), (.=)
  )
import qualified Data.Aeson as Aeson
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
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

-- | Token usage metadata for an inference turn.
data TokenUsage = TokenUsage
  { tuPromptTokens     :: !Int
  , tuCompletionTokens :: !Int
  , tuTotalTokens      :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON TokenUsage where
  toJSON TokenUsage{..} = object
    [ "prompt_tokens"     .= tuPromptTokens
    , "completion_tokens" .= tuCompletionTokens
    , "total_tokens"      .= tuTotalTokens
    ]

instance FromJSON TokenUsage where
  parseJSON = withObject "TokenUsage" $ \o ->
    TokenUsage
      <$> o .:? "prompt_tokens"     .!= 0
      <*> o .:? "completion_tokens" .!= 0
      <*> o .:? "total_tokens"      .!= 0

-- | The model's response for a turn.
data AssistantResponse = AssistantResponse
  { respContent   :: !(Maybe Text)
  , respToolCalls :: ![ToolCall]
  , respUsage     :: !(Maybe TokenUsage)
  } deriving (Show, Eq, Generic)

instance ToJSON AssistantResponse where
  toJSON AssistantResponse{..} = object
    ( [ "content" .= respContent
      , "tool_calls" .= respToolCalls
      ]
      ++ maybe [] (\u -> ["usage" .= u]) respUsage
    )

instance FromJSON AssistantResponse where
  parseJSON = withObject "AssistantResponse" $ \o ->
    AssistantResponse
      <$> o .:? "content"
      <*> (o .:? "tool_calls" .!= [])
      <*> o .:? "usage"

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

-- | Verdict returned by the goal evaluator LLM.
data GoalVerdict
  = GoalMet
  | GoalNotYetMet
  | GoalImpossible
  deriving (Show, Eq, Generic)

instance ToJSON GoalVerdict where
  toJSON = \case
    GoalMet         -> "met"
    GoalNotYetMet   -> "not_yet_met"
    GoalImpossible  -> "impossible"

instance FromJSON GoalVerdict where
  parseJSON = Aeson.withText "GoalVerdict" $ \v ->
    case v of
      "met"          -> pure GoalMet
      "not_yet_met"  -> pure GoalNotYetMet
      "impossible"   -> pure GoalImpossible
      other          -> fail ("Unknown goal verdict: " <> show other)

-- | Full evaluation result: verdict plus a short reason.
data GoalEvaluation = GoalEvaluation
  { geVerdict :: !GoalVerdict
  , geReason  :: !Text
  } deriving (Show, Eq, Generic)

instance FromJSON GoalEvaluation where
  parseJSON = withObject "GoalEvaluation" $ \o ->
    GoalEvaluation
      <$> o .: "verdict"
      <*> o .:? "reason" .!= ""

-- | Lifecycle status of a session goal.
data GoalStatus
  = GoalActive
  | GoalAchieved
  | GoalFailed
  | GoalCleared
  deriving (Show, Eq)

-- | Per-session goal condition store.
-- Holds one active condition at a time, plus evaluation metadata.
data GoalState = GoalState
  { gsCondition       :: !Text
  , gsStatus          :: !GoalStatus
  , gsTurnCount       :: !Int
  , gsLastReason      :: !(Maybe Text)
  , gsLastVerdict     :: !(Maybe GoalVerdict)
  , gsNoProgressCount :: !Int
  } deriving (Show, Eq)

-- | Construct an active goal state from a condition.
initialGoalState :: Text -> GoalState
initialGoalState cond = GoalState
  { gsCondition       = cond
  , gsStatus          = GoalActive
  , gsTurnCount       = 0
  , gsLastReason      = Nothing
  , gsLastVerdict     = Nothing
  , gsNoProgressCount = 0
  }

-- | Classification of a completed turn's content for goal error handling.
data GoalErrorKind = GoalErrUnrecoverable | GoalErrTransient | GoalNoError
  deriving (Show, Eq)

-- | Classify the text returned by a completed agent turn.
-- Returns 'GoalErrUnrecoverable' for auth, credit, context-overflow,
-- and model-unavailable errors; 'GoalErrTransient' for other errors;
-- 'GoalNoError' for normal completions.
--
-- Only text carrying the explicit @"[API Error]: "@ prefix is treated as an
-- error, so normal model output that happens to mention words like "model"
-- or "credit" is left as 'GoalNoError'.
classifyCompletion :: Text -> GoalErrorKind
classifyCompletion content
  | not (errPrefix `T.isPrefixOf` content) = GoalNoError
  | otherwise = classifyError (T.drop (T.length errPrefix) content)
  where
    errPrefix = "[API Error]: "

-- | Classify an error message that arrived via 'AgentFailed' (i.e. from the
-- IO interpreter).  Unlike 'classifyCompletion', the text is always an error,
-- so we search the whole (lowercased) message for unrecoverable keywords
-- without requiring any prefix — the real interpreter emits errors such as
-- @"OpenRouter API error: 401 Unauthorized"@ which carry no @"[API Error]: "@
-- prefix.  Returns 'GoalErrUnrecoverable' for auth, credit, context-overflow,
-- and model-unavailable errors; 'GoalErrTransient' otherwise.
classifyError :: Text -> GoalErrorKind
classifyError err =
  let lower = T.toLower err
      hasAny = any (`T.isInfixOf` lower)
  in if hasAny unrecoverableKeywords
       then GoalErrUnrecoverable
       else GoalErrTransient

-- | Keywords that mark an API error as unrecoverable (the goal should be
-- failed rather than left active for retry).
unrecoverableKeywords :: [Text]
unrecoverableKeywords = concat
  [ ["401", "unauthorized", "authentication", "api key"]
  , ["402", "payment", "credit", "balance", "quota", "billing"]
  , ["context", "overflow", "too long", "maximum context", "token limit"]
  , ["404", "model", "not found", "unavailable", "does not exist"]
  ]

-- | Maximum length of a goal condition text.
maxGoalConditionLength :: Int
maxGoalConditionLength = 4000

-- | Aliases for clearing the goal via @/goal <alias>@.
goalClearAliases :: [Text]
goalClearAliases = ["clear", "stop", "off", "reset", "none", "cancel"]

-- | True when the @/goal@ argument is a bare clear-alias with no trailing
-- text, e.g. @/goal clear@ or @/goal stop@.  A condition whose first word
-- happens to be a clear alias — @/goal stop the server@ — is NOT a clear
-- command, because text follows the alias word.  The argument is the text
-- after the @\"/goal \"@ prefix (optionally with leading\/trailing spaces).
goalArgIsClear :: Text -> Bool
goalArgIsClear argText =
  let argWord = T.toLower (T.takeWhile (not . isSpace) argText)
      rest    = T.strip (T.dropWhile (not . isSpace) argText)
  in argWord `elem` goalClearAliases && T.null rest

-- | Configuration parameters for the agent.
data AgentConfig = AgentConfig
  { cfgModel        :: !Text
  , cfgSystemPrompt :: !(Maybe Text)
  -- | Maximum number of agent turns. 'Nothing' means unlimited.
  , cfgMaxTurns     :: !(Maybe Int)
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
  | EvLLMResponse !(Maybe Text) ![ToolCall] !(Maybe TokenUsage)
  | EvToolCall !Text !Text
  | EvToolResult !Text !ToolResult
  | EvTurnComplete !Int
  | EvDone !Text
  | EvError !Text
  | EvGoalSet !Text
  | EvGoalEvaluated !GoalVerdict !Text
  | EvGoalAchieved !Text
  | EvGoalFailed !Text !Text
  | EvGoalCleared !Text
  | EvGoalBlocked !Text
  deriving (Show, Eq)
