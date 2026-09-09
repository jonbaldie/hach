{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Hach.Types
  ( -- * Roles and Messages
    Role(..)
  , ToolCall(..)
  , Message(..)
  , AssistantResponse(..)
  , parseCallArgs
  , TokenUsage(..)
  , mkTokenUsage
  , SessionTokenUsage(..)
  , initialSessionTokenUsage
  , addUsageToSession
  , modelContextLimit
  , contextSaturationPercent

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

    -- * Permissions
  , PermissionMode(..)
  , permissionModeName
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

    -- * Reasoning effort
  , EffortLevel(..)
  , effortLevelName
  , supportedEffortLevels
  , parseEffortLevel
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), FromJSONKey(..), ToJSONKey(..), Value, object, withObject, (.:), (.:?), (.!=), (.=)
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
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
    rawArgs <- fObj .: "arguments" >>= parseToolArguments
    pure ToolCall
      { callId = cid
      , functionName = fname
      , callArgsRaw = rawArgs
      }

parseToolArguments :: Value -> Parser Text
parseToolArguments (Aeson.String t) = pure t
parseToolArguments v@(Aeson.Object _) = pure (encodeJsonText v)
parseToolArguments v@(Aeson.Array _) = pure (encodeJsonText v)
parseToolArguments _ = fail "expected arguments String, Object, or Array"

encodeJsonText :: Value -> Text
encodeJsonText = TE.decodeUtf8 . LBS.toStrict . Aeson.encode

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
      "system" -> SystemMsg <$> (o .: "content" >>= parseRequiredContent)
      "user" -> UserMsg <$> (o .: "content" >>= parseRequiredContent)
      "assistant" -> AssistantMsg <$> (o .:? "content" >>= parseOptionalContent) <*> (o .:? "tool_calls" .!= [])
      "tool" -> ToolMsg <$> o .: "tool_call_id" <*> (o .:? "name" .!= "") <*> (o .: "content" >>= parseRequiredContent)
      other -> fail ("Unknown message role: " <> show other)

parseOptionalContent :: Maybe Value -> Parser (Maybe Text)
parseOptionalContent Nothing = pure Nothing
parseOptionalContent (Just v) = Just <$> parseRequiredContent v

parseRequiredContent :: Value -> Parser Text
parseRequiredContent (Aeson.String t) = pure t
parseRequiredContent (Aeson.Array parts) = pure (flattenContentParts parts)
parseRequiredContent _ = fail "expected content String or Array of parts"

flattenContentParts :: Aeson.Array -> Text
flattenContentParts = T.concat . mapMaybe contentPartText . toList

contentPartText :: Value -> Maybe Text
contentPartText (Aeson.String t) = Just t
contentPartText (Aeson.Object o) = parseMaybe (.: "text") o
contentPartText _ = Nothing

-- | Token usage metadata for an inference turn.
data TokenUsage = TokenUsage
  { tuPromptTokens     :: !Int
  , tuCompletionTokens :: !Int
  , tuTotalTokens      :: !Int
  , tuCachedTokens     :: !Int
  , tuCost             :: !(Maybe Double)
  } deriving (Show, Eq, Generic)

-- | Smart constructor for simple token usage without cache or cost.
mkTokenUsage :: Int -> Int -> Int -> TokenUsage
mkTokenUsage p c t = TokenUsage p c t 0 Nothing

instance ToJSON TokenUsage where
  toJSON TokenUsage{..} = object $
    [ "prompt_tokens"     .= tuPromptTokens
    , "completion_tokens" .= tuCompletionTokens
    , "total_tokens"      .= tuTotalTokens
    , "cached_tokens"     .= tuCachedTokens
    ] ++ maybe [] (\c -> ["cost" .= c]) tuCost

instance FromJSON TokenUsage where
  parseJSON = withObject "TokenUsage" $ \o -> do
    p <- o .:? "prompt_tokens"     .!= 0
    c <- o .:? "completion_tokens" .!= 0
    t <- o .:? "total_tokens"      .!= (p + c)
    mDetails <- o .:? "prompt_tokens_details"
    cachedFromDetails <- case mDetails of
      Just (Aeson.Object d) -> d .:? "cached_tokens" .!= 0
      _                     -> pure 0
    cachedDirect <- o .:? "cached_tokens" .!= 0
    cachedRead <- o .:? "cache_read_input_tokens" .!= 0
    let cached = max cachedFromDetails (max cachedDirect cachedRead)
    mTotalCost <- o .:? "total_cost"
    costVal <- case mTotalCost of
      Just cost -> pure (Just cost)
      Nothing   -> do
        mDirectCost <- o .:? "cost"
        case mDirectCost of
          Just cost -> pure (Just cost)
          Nothing   -> do
            mDetailsObj <- o .:? "cost_details"
            case mDetailsObj of
              Just (Aeson.Object cd) -> do
                mUpstream <- cd .:? "upstream_inference_cost"
                case mUpstream of
                  Just cost -> pure (Just cost)
                  Nothing   -> do
                    mPromptCost <- cd .:? "upstream_inference_prompt_cost"
                    mCompCost   <- cd .:? "upstream_inference_completions_cost"
                    case (mPromptCost, mCompCost) of
                      (Just pc, Just cc) -> pure (Just (pc + cc))
                      (Just pc, Nothing) -> pure (Just pc)
                      (Nothing, Just cc) -> pure (Just cc)
                      (Nothing, Nothing) -> pure Nothing
              _ -> pure Nothing
    pure TokenUsage
      { tuPromptTokens     = p
      , tuCompletionTokens = c
      , tuTotalTokens      = t
      , tuCachedTokens     = cached
      , tuCost             = costVal
      }

-- | Cumulative session token usage across multiple turns and operations.
data SessionTokenUsage = SessionTokenUsage
  { stuPromptTokens     :: !Int
  , stuCompletionTokens :: !Int
  , stuTotalTokens      :: !Int
  , stuCachedTokens     :: !Int
  , stuEvaluationTokens :: !Int
  , stuTotalCost        :: !(Maybe Double)
  } deriving (Show, Eq, Generic)

instance ToJSON SessionTokenUsage where
  toJSON SessionTokenUsage{..} = object $
    [ "prompt_tokens"     .= stuPromptTokens
    , "completion_tokens" .= stuCompletionTokens
    , "total_tokens"      .= stuTotalTokens
    , "cached_tokens"     .= stuCachedTokens
    , "evaluation_tokens" .= stuEvaluationTokens
    ] ++ maybe [] (\c -> ["total_cost" .= c]) stuTotalCost

instance FromJSON SessionTokenUsage where
  parseJSON = withObject "SessionTokenUsage" $ \o ->
    SessionTokenUsage
      <$> o .:? "prompt_tokens"     .!= 0
      <*> o .:? "completion_tokens" .!= 0
      <*> o .:? "total_tokens"      .!= 0
      <*> o .:? "cached_tokens"     .!= 0
      <*> o .:? "evaluation_tokens" .!= 0
      <*> o .:? "total_cost"

-- | Clean initial session usage.
initialSessionTokenUsage :: SessionTokenUsage
initialSessionTokenUsage = SessionTokenUsage 0 0 0 0 0 Nothing

-- | Accumulate turn usage into session totals.
addUsageToSession :: TokenUsage -> Bool -> SessionTokenUsage -> SessionTokenUsage
addUsageToSession u isEval s =
  let promptInc = tuPromptTokens u
      compInc   = tuCompletionTokens u
      totalInc  = tuTotalTokens u
      cacheInc  = tuCachedTokens u
      evalInc   = if isEval then totalInc else 0
      costInc   = case (stuTotalCost s, tuCost u) of
                    (Just c1, Just c2) -> Just (c1 + c2)
                    (Just c1, Nothing) -> Just c1
                    (Nothing, Just c2) -> Just c2
                    (Nothing, Nothing) -> Nothing
  in s
       { stuPromptTokens     = stuPromptTokens s + promptInc
       , stuCompletionTokens = stuCompletionTokens s + compInc
       , stuTotalTokens      = stuTotalTokens s + totalInc
       , stuCachedTokens     = stuCachedTokens s + cacheInc
       , stuEvaluationTokens = stuEvaluationTokens s + evalInc
       , stuTotalCost        = costInc
       }

-- | Standard context limits for known model families.
modelContextLimit :: Text -> Int
modelContextLimit m
  | "claude" `T.isInfixOf` lower = 200000
  | "gpt-4" `T.isInfixOf` lower = 128000
  | "gpt-3.5" `T.isInfixOf` lower = 16384
  | "llama-3" `T.isInfixOf` lower = 128000
  | "deepseek" `T.isInfixOf` lower = 128000
  | "qwen" `T.isInfixOf` lower = 128000
  | "gemini" `T.isInfixOf` lower = 1000000
  | "mistral" `T.isInfixOf` lower = 128000
  | "muse" `T.isInfixOf` lower = 128000
  | otherwise = 128000
  where
    lower = T.toLower m

-- | Calculate context window saturation percentage (clamped to 0..100).
contextSaturationPercent :: Int -> Text -> Int
contextSaturationPercent tokens model =
  let limit = modelContextLimit model
  in if limit <= 0 then 0 else max 0 (min 100 ((tokens * 100) `div` limit))

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
      <$> (o .:? "content" >>= parseOptionalContent)
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
  , ["404", "model not found", "model unavailable", "model does not exist", "unknown model"]
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
  let clean   = T.stripStart argText
      argWord = T.toLower (T.takeWhile (not . isSpace) clean)
      rest    = T.strip (T.dropWhile (not . isSpace) clean)
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
  | EvPartialResponse !Text
  | EvToolCallDelta !Text
  | EvToolCall !Text !Text
  | EvToolResult !Text !ToolResult
  | EvTurnComplete !Int
  | EvDone !Text
  | EvError !Text
  | EvGoalSet !Text
  | EvGoalEvaluated !GoalVerdict !Text
  | EvGoalEvaluationUsage !TokenUsage
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

-- | Canonical lowercase name of a permission mode, matching the spelling
-- used in settings JSON and on the command line.
permissionModeName :: PermissionMode -> Text
permissionModeName = \case
  ModeDefault           -> "default"
  ModeAcceptEdits       -> "acceptEdits"
  ModePlan              -> "plan"
  ModeAuto              -> "auto"
  ModeDontAsk           -> "dontAsk"
  ModeBypassPermissions -> "bypassPermissions"

instance ToJSON PermissionMode where
  toJSON = Aeson.String . permissionModeName

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
-- Reasoning effort
--------------------------------------------------------------------------------

-- | OpenRouter `reasoning.effort` values.
data EffortLevel
  = EffortMax
  | EffortXHigh
  | EffortHigh
  | EffortMedium
  | EffortLow
  | EffortMinimal
  | EffortNone
  deriving (Show, Eq, Enum, Bounded, Generic)

-- | Canonical lowercase name matching OpenRouter's effort vocabulary.
effortLevelName :: EffortLevel -> Text
effortLevelName = \case
  EffortMax     -> "max"
  EffortXHigh   -> "xhigh"
  EffortHigh    -> "high"
  EffortMedium  -> "medium"
  EffortLow     -> "low"
  EffortMinimal -> "minimal"
  EffortNone    -> "none"

-- | Supported `effort_level` strings, in descending-effort order.
supportedEffortLevels :: [Text]
supportedEffortLevels = fmap effortLevelName [minBound ..]

-- | Parse a configured effort string. Comparison is case-insensitive after
-- stripping whitespace. Unknown values are rejected rather than dropped.
parseEffortLevel :: Text -> Either String EffortLevel
parseEffortLevel raw =
  case T.toLower (T.strip raw) of
    "max"     -> Right EffortMax
    "xhigh"   -> Right EffortXHigh
    "high"    -> Right EffortHigh
    "medium"  -> Right EffortMedium
    "low"     -> Right EffortLow
    "minimal" -> Right EffortMinimal
    "none"    -> Right EffortNone
    _         ->
      Left
        ( "Unsupported effort_level: "
            <> T.unpack (T.strip raw)
            <> ". Supported values: "
            <> T.unpack (T.intercalate ", " supportedEffortLevels)
        )

instance ToJSON EffortLevel where
  toJSON = Aeson.String . effortLevelName

instance FromJSON EffortLevel where
  parseJSON = Aeson.withText "EffortLevel" $ \t ->
    case parseEffortLevel t of
      Right e  -> pure e
      Left err -> fail err

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

