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

    -- * Agent Configuration & Results
  , AgentConfig(..)
  , AgentResult(..)
  , AgentEvent(..)

    -- * Permissions
  , PermissionMode(..)
  , PermissionDecision(..)
  , RuleAction(..)
  , PermissionRule(..)

    -- * Sessions
  , SessionId
  , SessionInfo(..)
  , SessionEvent(..)

    -- * Hooks
  , HookEvent(..)
  , HookHandlerType(..)
  , HookHandler(..)
  , HookResult(..)
  , defaultHookResult

    -- * Subagents
  , AgentId(..)
  , AgentInfo(..)

    -- * Background Tasks
  , TaskId(..)
  , TaskInfo(..)

    -- * Git
  , GitStatusInfo(..)

    -- * Output Styles
  , OutputStyle(..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), FromJSONKey(..), ToJSONKey(..), Value, object, withObject, (.:), (.:?), (.!=), (.=)
  )
import qualified Data.Aeson as Aeson
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
classifyCompletion :: Text -> GoalErrorKind
classifyCompletion content
  | not (errPrefix `T.isPrefixOf` content) = GoalNoError
  | otherwise =
      let lower = T.toLower (T.drop (T.length errPrefix) content)
          hasAny = any (`T.isInfixOf` lower)
      in if hasAny ["401", "unauthorized", "authentication", "api key"]
           then GoalErrUnrecoverable
         else if hasAny ["402", "payment", "credit", "balance", "quota", "billing"]
           then GoalErrUnrecoverable
         else if hasAny ["context", "overflow", "too long", "maximum context", "token limit"]
           then GoalErrUnrecoverable
         else if hasAny ["404", "model", "not found", "unavailable", "does not exist"]
           then GoalErrUnrecoverable
         else GoalErrTransient
  where
    errPrefix = "[API Error]: "

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
  | EvLLMResponse !(Maybe Text) ![ToolCall] !(Maybe TokenUsage)
  | EvPartialResponse !Text
  | EvToolCallDelta !Text
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
  | EvPermissionDenied !Text !Text
  | EvHookTriggered !Text !Text
  | EvSessionSaved !Text
  | EvNotificationSent !Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Permissions
--------------------------------------------------------------------------------

-- | Six operating modes for the permission subsystem.
data PermissionMode
  = ModeDefault           -- ^ Ask for writes/commands
  | ModeAcceptEdits       -- ^ Auto-approve file edits, ask for commands
  | ModePlan              -- ^ Read-only, block all writes/commands
  | ModeAuto              -- ^ Classifier decides
  | ModeDontAsk           -- ^ Auto-approve all
  | ModeBypassPermissions -- ^ Skip all permission checks
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON PermissionMode where
  toJSON = \case
    ModeDefault           -> "default"
    ModeAcceptEdits       -> "acceptEdits"
    ModePlan              -> "plan"
    ModeAuto              -> "auto"
    ModeDontAsk           -> "dontAsk"
    ModeBypassPermissions -> "bypassPermissions"

instance FromJSON PermissionMode where
  parseJSON = Aeson.withText "PermissionMode" $ \case
    "default"           -> pure ModeDefault
    "acceptEdits"       -> pure ModeAcceptEdits
    "plan"              -> pure ModePlan
    "auto"              -> pure ModeAuto
    "dontAsk"           -> pure ModeDontAsk
    "bypassPermissions" -> pure ModeBypassPermissions
    other               -> fail ("Unknown permission mode: " <> T.unpack other)

-- | Outcome of evaluating permission for an action.
data PermissionDecision
  = PermAllow
  | PermAsk !Text
  | PermDeny !Text
  deriving (Show, Eq, Generic)

instance ToJSON PermissionDecision where
  toJSON = \case
    PermAllow    -> object ["decision" .= ("allow" :: Text)]
    PermAsk msg  -> object ["decision" .= ("ask" :: Text), "reason" .= msg]
    PermDeny msg -> object ["decision" .= ("deny" :: Text), "reason" .= msg]

instance FromJSON PermissionDecision where
  parseJSON = withObject "PermissionDecision" $ \o -> do
    dec <- o .: "decision"
    case (dec :: Text) of
      "allow" -> pure PermAllow
      "ask"   -> PermAsk <$> (o .:? "reason" .!= "")
      "deny"  -> PermDeny <$> (o .:? "reason" .!= "")
      other   -> fail ("Unknown permission decision: " <> T.unpack other)

-- | Action in a permission rule.
data RuleAction = RuleAllow | RuleAsk | RuleDeny
  deriving (Show, Eq, Generic)

instance ToJSON RuleAction where
  toJSON = \case
    RuleAllow -> "allow"
    RuleAsk   -> "ask"
    RuleDeny  -> "deny"

instance FromJSON RuleAction where
  parseJSON = Aeson.withText "RuleAction" $ \case
    "allow" -> pure RuleAllow
    "ask"   -> pure RuleAsk
    "deny"  -> pure RuleDeny
    other   -> fail ("Unknown rule action: " <> T.unpack other)

-- | Permission rule matching by tool and optional path glob.
data PermissionRule = PermissionRule
  { prAction   :: !RuleAction
  , prTool     :: !(Maybe Text)
  , prPathGlob :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON PermissionRule where
  toJSON PermissionRule{..} = object
    [ "action" .= prAction
    , "tool" .= prTool
    , "path" .= prPathGlob
    ]

instance FromJSON PermissionRule where
  parseJSON = withObject "PermissionRule" $ \o ->
    PermissionRule
      <$> o .: "action"
      <*> o .:? "tool"
      <*> (o .:? "path" .!= Nothing)

--------------------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------------------

-- | Unique identifier for a session.
type SessionId = Text

-- | Persistent metadata describing a recorded session.
data SessionInfo = SessionInfo
  { siId        :: !SessionId
  , siCreatedAt :: !Text
  , siModel     :: !Text
  , siTurns     :: !Int
  , siCostUsd   :: !Double
  } deriving (Show, Eq, Generic)

instance ToJSON SessionInfo where
  toJSON SessionInfo{..} = object
    [ "id" .= siId
    , "created_at" .= siCreatedAt
    , "model" .= siModel
    , "turns" .= siTurns
    , "cost_usd" .= siCostUsd
    ]

instance FromJSON SessionInfo where
  parseJSON = withObject "SessionInfo" $ \o ->
    SessionInfo
      <$> o .: "id"
      <*> o .: "created_at"
      <*> o .: "model"
      <*> o .:? "turns" .!= 0
      <*> o .:? "cost_usd" .!= 0.0

-- | Event item stored in session JSONL lines.
data SessionEvent
  = SeMessage !Message
  | SeEvent !AgentEvent
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Hooks
--------------------------------------------------------------------------------

-- | Hook event points in the agent lifecycle.
data HookEvent
  = HookPreToolUse
  | HookPostToolUse
  | HookUserPromptSubmit
  | HookStop
  | HookSessionStart
  | HookNotification
  | HookPreCompact
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON HookEvent where
  toJSON = \case
    HookPreToolUse       -> "pre_tool_use"
    HookPostToolUse      -> "post_tool_use"
    HookUserPromptSubmit -> "user_prompt_submit"
    HookStop             -> "stop"
    HookSessionStart     -> "session_start"
    HookNotification     -> "notification"
    HookPreCompact       -> "pre_compact"

instance FromJSON HookEvent where
  parseJSON = Aeson.withText "HookEvent" $ \case
    "pre_tool_use"       -> pure HookPreToolUse
    "post_tool_use"      -> pure HookPostToolUse
    "user_prompt_submit" -> pure HookUserPromptSubmit
    "stop"               -> pure HookStop
    "session_start"      -> pure HookSessionStart
    "notification"       -> pure HookNotification
    "pre_compact"        -> pure HookPreCompact
    other                -> fail ("Unknown hook event: " <> T.unpack other)

instance ToJSONKey HookEvent
instance FromJSONKey HookEvent

data HookHandlerType
  = HookCommand !Text
  | HookHttp !Text
  | HookMcp !Text !Text
  deriving (Show, Eq, Generic)

instance ToJSON HookHandlerType where
  toJSON = \case
    HookCommand cmd   -> object ["type" .= ("command" :: Text), "command" .= cmd]
    HookHttp url      -> object ["type" .= ("http" :: Text), "url" .= url]
    HookMcp srv tool  -> object ["type" .= ("mcp" :: Text), "server" .= srv, "tool" .= tool]

instance FromJSON HookHandlerType where
  parseJSON = withObject "HookHandlerType" $ \o -> do
    t <- o .: "type"
    case (t :: Text) of
      "command" -> HookCommand <$> o .: "command"
      "http"    -> HookHttp <$> o .: "url"
      "mcp"     -> HookMcp <$> o .: "server" <*> o .: "tool"
      other     -> fail ("Unknown hook handler type: " <> T.unpack other)

data HookHandler = HookHandler
  { hhType    :: !HookHandlerType
  , hhMatcher :: !(Maybe Text)  -- ^ Optional tool name matcher
  , hhAsync   :: !Bool
  } deriving (Show, Eq, Generic)

instance ToJSON HookHandler where
  toJSON HookHandler{..} = object
    [ "handler" .= hhType
    , "matcher" .= hhMatcher
    , "async"   .= hhAsync
    ]

instance FromJSON HookHandler where
  parseJSON = withObject "HookHandler" $ \o ->
    HookHandler
      <$> o .: "handler"
      <*> o .:? "matcher"
      <*> o .:? "async" .!= False

data HookResult = HookResult
  { hrDecision          :: !(Maybe PermissionDecision)
  , hrAdditionalContext :: !(Maybe Text)
  , hrModifiedInput     :: !(Maybe Value)
  , hrError             :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

defaultHookResult :: HookResult
defaultHookResult = HookResult Nothing Nothing Nothing Nothing

instance ToJSON HookResult where
  toJSON HookResult{..} = object
    [ "permissionDecision" .= hrDecision
    , "additionalContext"  .= hrAdditionalContext
    , "modifiedToolInput"  .= hrModifiedInput
    , "error"              .= hrError
    ]

instance FromJSON HookResult where
  parseJSON = withObject "HookResult" $ \o ->
    HookResult
      <$> o .:? "permissionDecision"
      <*> o .:? "additionalContext"
      <*> o .:? "modifiedToolInput"
      <*> o .:? "error"

--------------------------------------------------------------------------------
-- Subagents
--------------------------------------------------------------------------------

newtype AgentId = AgentId { unAgentId :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON AgentId where
  toJSON (AgentId t) = toJSON t

instance FromJSON AgentId where
  parseJSON v = AgentId <$> parseJSON v

data AgentInfo = AgentInfo
  { aiId     :: !AgentId
  , aiName   :: !Text
  , aiModel  :: !Text
  , aiStatus :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON AgentInfo where
  toJSON AgentInfo{..} = object
    [ "id" .= aiId
    , "name" .= aiName
    , "model" .= aiModel
    , "status" .= aiStatus
    ]

instance FromJSON AgentInfo where
  parseJSON = withObject "AgentInfo" $ \o ->
    AgentInfo
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "model"
      <*> o .: "status"

--------------------------------------------------------------------------------
-- Background Tasks
--------------------------------------------------------------------------------

newtype TaskId = TaskId { unTaskId :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TaskId where
  toJSON (TaskId t) = toJSON t

instance FromJSON TaskId where
  parseJSON v = TaskId <$> parseJSON v

data TaskInfo = TaskInfo
  { tiTaskId  :: !TaskId
  , tiCommand :: !Text
  , tiStatus  :: !Text
  , tiOutput  :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON TaskInfo where
  toJSON TaskInfo{..} = object
    [ "id" .= tiTaskId
    , "command" .= tiCommand
    , "status" .= tiStatus
    , "output" .= tiOutput
    ]

instance FromJSON TaskInfo where
  parseJSON = withObject "TaskInfo" $ \o ->
    TaskInfo
      <$> o .: "id"
      <*> o .: "command"
      <*> o .: "status"
      <*> o .:? "output" .!= ""

--------------------------------------------------------------------------------
-- Git
--------------------------------------------------------------------------------

data GitStatusInfo = GitStatusInfo
  { gsiBranch    :: !Text
  , gsiClean     :: !Bool
  , gsiModified  :: ![FilePath]
  , gsiUntracked :: ![FilePath]
  } deriving (Show, Eq, Generic)

instance ToJSON GitStatusInfo where
  toJSON GitStatusInfo{..} = object
    [ "branch" .= gsiBranch
    , "clean" .= gsiClean
    , "modified" .= gsiModified
    , "untracked" .= gsiUntracked
    ]

instance FromJSON GitStatusInfo where
  parseJSON = withObject "GitStatusInfo" $ \o ->
    GitStatusInfo
      <$> o .: "branch"
      <*> o .: "clean"
      <*> o .:? "modified" .!= []
      <*> o .:? "untracked" .!= []

--------------------------------------------------------------------------------
-- Output Styles
--------------------------------------------------------------------------------

data OutputStyle
  = StyleDefault
  | StyleConcise
  | StyleExplanatory
  | StyleCodeOnly
  | StyleCustom !Text
  deriving (Show, Eq, Generic)

instance ToJSON OutputStyle where
  toJSON = \case
    StyleDefault     -> "default"
    StyleConcise     -> "concise"
    StyleExplanatory -> "explanatory"
    StyleCodeOnly    -> "code_only"
    StyleCustom t    -> toJSON t

instance FromJSON OutputStyle where
  parseJSON = Aeson.withText "OutputStyle" $ \case
    "default"     -> pure StyleDefault
    "concise"     -> pure StyleConcise
    "explanatory" -> pure StyleExplanatory
    "code_only"   -> pure StyleCodeOnly
    other         -> pure (StyleCustom other)

