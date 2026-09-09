{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hach.TUI.Types
  ( -- * Status and Focus
    TuiStatus(..)
  , FocusArea(..)
  , pattern FocusHistory
  , nextFocus
  , prevFocus

    -- * Screen Elements
  , ToolLifecycle(..)
  , ToolCard(..)
  , PermissionPrompt(..)
  , TranscriptItem(..)
  , pattern DiUser
  , pattern DiAssistant
  , pattern DiSystem
  , pattern DiNotice
  , pattern DiToolCard
  , DialogueItem
  , ToolItem

    -- * State
  , TuiState(..)
  , initialTuiState
  , tsHistory
  , tsTools
  , tsHistoryScroll
  , UsageStatus(..)

    -- * Events and Actions
  , UserKey(..)
  , TuiEvent(..)
  , TuiAction(..)
  , pattern ActionScrollHistory

    -- * Built-in slash commands
  , builtinCommands
  ) where

import Hach.Skills (SkillCatalog)
import Hach.Types (AgentEvent, GoalState, PermissionMode (..), SessionTokenUsage, TokenUsage, ToolResult, initialSessionTokenUsage)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Status of token usage reporting for the latest turn.
data UsageStatus
  = UsageVerified
  | UsageMissing
  deriving (Show, Eq)

-- | Operational status of the agent in the TUI.
data TuiStatus
  = StatusIdle
  | StatusThinking
  | StatusRunningTool !Text
  | StatusAwaitingPermission !Text
  | StatusFinished
  | StatusError !Text
  deriving (Show, Eq)

-- | Focusable panels in the interface: prompt input and transcript.
data FocusArea
  = FocusInput
  | FocusTranscript
  deriving (Show, Eq, Enum, Bounded)

pattern FocusHistory :: FocusArea
pattern FocusHistory = FocusTranscript

{-# COMPLETE FocusInput, FocusTranscript #-}
{-# COMPLETE FocusInput, FocusHistory #-}

-- | Cycle to the next focusable panel.
nextFocus :: FocusArea -> FocusArea
nextFocus FocusInput      = FocusTranscript
nextFocus FocusTranscript = FocusInput

-- | Cycle to the previous focusable panel.
prevFocus :: FocusArea -> FocusArea
prevFocus FocusInput      = FocusTranscript
prevFocus FocusTranscript = FocusInput

-- | Tool execution lifecycle state in the transcript.
data ToolLifecycle
  = Pending
  | Running
  | Finished !ToolResult
  | Denied !Text
  | Cancelled
  deriving (Show, Eq)

-- | Pending interactive permission ask shown in the TUI.
data PermissionPrompt = PermissionPrompt
  { ppId     :: !Int
  , ppTool   :: !Text
  , ppArgs   :: !Text
  , ppReason :: !Text
  } deriving (Show, Eq)

-- | Tool card record carried in the transcript.
data ToolCard = ToolCard
  { tcId        :: !Text
  , tcName      :: !Text
  , tcArgs      :: !Text
  , tcLifecycle :: !ToolLifecycle
  , tcExpanded  :: !Bool
  } deriving (Show, Eq)

-- | Screen element representing a single chronological entry in the transcript.
data TranscriptItem
  = TiUser !Text
  | TiAssistant !Text
  | TiSystem !Text
  | TiNotice !Text
  | TiToolCard !ToolCard
  deriving (Show, Eq)

type DialogueItem = TranscriptItem
type ToolItem = ToolCard

pattern DiUser :: Text -> TranscriptItem
pattern DiUser u = TiUser u

pattern DiAssistant :: Text -> TranscriptItem
pattern DiAssistant a = TiAssistant a

pattern DiSystem :: Text -> TranscriptItem
pattern DiSystem s = TiSystem s

pattern DiNotice :: Text -> TranscriptItem
pattern DiNotice n = TiNotice n

pattern DiToolCard :: ToolCard -> TranscriptItem
pattern DiToolCard tc = TiToolCard tc

{-# COMPLETE DiUser, DiAssistant, DiSystem, DiNotice, DiToolCard #-}

-- | Full screen state for the TUI.
data TuiState = TuiState
  { tsModelName          :: !Text
  , tsCurrentTurn        :: !Int
  , tsMaxTurns           :: !(Maybe Int)
  , tsStatus             :: !TuiStatus
  , tsFocus              :: !FocusArea
  , tsInputBuffer        :: !Text
  , tsTranscript         :: ![TranscriptItem]
  , tsTranscriptScroll   :: !Int
  , tsTranscriptManualScroll :: !Bool
  , tsSelectedToolIndex  :: !Int
  , tsShowHelp           :: !Bool
  , tsShouldQuit         :: !Bool
  , tsCancelRequested    :: !Bool
  , tsPromptHistory      :: ![Text]
  , tsPromptHistoryIndex :: !(Maybe Int)
  , tsPromptDraft        :: !Text
  , tsContextTokens      :: !Int
  , tsTokenUsage         :: !(Maybe TokenUsage)
  , tsSessionTokens      :: !SessionTokenUsage
  , tsUsageStatus        :: !UsageStatus
  , tsSkills             :: !SkillCatalog
  , tsGoalState          :: !(Maybe GoalState)
  , tsPermissionMode     :: !PermissionMode
  , tsPendingAsk         :: !(Maybe PermissionPrompt)
  } deriving (Show, Eq)

-- | Project tool cards from the transcript for the Tool Activity pane.
tsTools :: TuiState -> [ToolCard]
tsTools state = [tc | TiToolCard tc <- tsTranscript state]

-- | Compatibility accessor for reading the transcript.
tsHistory :: TuiState -> [TranscriptItem]
tsHistory = tsTranscript

-- | Compatibility accessor for transcript scroll.
tsHistoryScroll :: TuiState -> Int
tsHistoryScroll = tsTranscriptScroll

-- | Initialize a clean TUI state.
initialTuiState :: Text -> Maybe Int -> TuiState
initialTuiState model maxTurns = TuiState
  { tsModelName          = model
  , tsCurrentTurn        = 0
  , tsMaxTurns           = maxTurns
  , tsStatus             = StatusIdle
  , tsFocus              = FocusInput
  , tsInputBuffer        = ""
  , tsTranscript         = []
  , tsTranscriptScroll   = 0
  , tsTranscriptManualScroll = False
  , tsSelectedToolIndex  = 0
  , tsShowHelp           = False
  , tsShouldQuit         = False
  , tsCancelRequested    = False
  , tsPromptHistory      = []
  , tsPromptHistoryIndex = Nothing
  , tsPromptDraft        = ""
  , tsContextTokens      = 0
  , tsTokenUsage         = Nothing
  , tsSessionTokens      = initialSessionTokenUsage
  , tsUsageStatus        = UsageVerified
  , tsSkills             = Map.empty
  , tsGoalState          = Nothing
  , tsPermissionMode     = ModeDefault
  , tsPendingAsk         = Nothing
  }

-- | Simplified user keystroke events abstracted from Vty.
data UserKey
  = KeyChar !Char
  | KeyEnter
  | KeyBackspace
  | KeyDelete
  | KeyTab
  | KeyBackTab
  | KeyEsc
  | KeyUp
  | KeyDown
  | KeyPageUp
  | KeyPageDown
  | KeyScrollUp
  | KeyScrollDown
  | KeyF1
  | KeyCtrl !Char
  deriving (Show, Eq)

-- | Events processed by the pure TUI reducer.
data TuiEvent
  = EvUserKey !UserKey
  | EvHarness !AgentEvent
  | EvSubmit !Text
  deriving (Show, Eq)

-- | Actions requested by the reducer for the external environment to perform.
data TuiAction
  = ActionRunAgent !Text
  | ActionRunGoal !Text
  | ActionCancelAgent
  | ActionQuit
  | ActionScrollTranscript !Int
  | ActionSetPermissionMode !PermissionMode
  | ActionRespondPermission !Int !Bool
  deriving (Show, Eq)

-- | Canonical list of built-in slash commands recognised by the TUI.
builtinCommands :: [Text]
builtinCommands =
  [ "/clear"
  , "/help"
  , "/cost"
  , "/compact"
  , "/goal"
  , "/exit"
  , "/quit"
  , "/model"
  , "/config"
  , "/context"
  , "/resume"
  , "/plan"
  , "/diff"
  , "/tasks"
  , "/theme"
  , "/status"
  , "/memory"
  , "/init"
  , "/permissions"
  , "/fewer-permission-prompts"
  , "/doctor"
  , "/copy"
  , "/reload-skills"
  , "/mcp"
  , "/plugin"
  ]

pattern ActionScrollHistory :: Int -> TuiAction
pattern ActionScrollHistory delta = ActionScrollTranscript delta

{-# COMPLETE ActionRunAgent, ActionRunGoal, ActionCancelAgent, ActionQuit, ActionScrollTranscript, ActionSetPermissionMode, ActionRespondPermission #-}
{-# COMPLETE ActionRunAgent, ActionRunGoal, ActionCancelAgent, ActionQuit, ActionScrollHistory, ActionSetPermissionMode, ActionRespondPermission #-}
