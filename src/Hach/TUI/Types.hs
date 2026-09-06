{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Hach.TUI.Types
  ( -- * Status and Focus
    TuiStatus(..)
  , FocusArea(..)
  , nextFocus
  , prevFocus

    -- * Screen Elements
  , ToolLifecycle(..)
  , ToolCard(..)
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
  , UsageStatus(..)

    -- * Events and Actions
  , UserKey(..)
  , TuiEvent(..)
  , TuiAction(..)
  ) where

import Hach.Skills (SkillCatalog)
import Hach.Types (AgentEvent, GoalState, SessionTokenUsage, TokenUsage, ToolResult, initialSessionTokenUsage)
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
  | StatusFinished
  | StatusError !Text
  deriving (Show, Eq)

-- | Focusable panels in the interface.
data FocusArea
  = FocusInput
  | FocusHistory
  | FocusTools
  deriving (Show, Eq, Enum, Bounded)

-- | Cycle to the next focusable panel.
nextFocus :: FocusArea -> FocusArea
nextFocus FocusInput   = FocusHistory
nextFocus FocusHistory = FocusTools
nextFocus FocusTools   = FocusInput

-- | Cycle to the previous focusable panel.
prevFocus :: FocusArea -> FocusArea
prevFocus FocusInput   = FocusTools
prevFocus FocusTools   = FocusHistory
prevFocus FocusHistory = FocusInput

-- | Tool execution lifecycle state in the transcript.
data ToolLifecycle
  = Pending
  | Running
  | Finished !ToolResult
  | Denied !Text
  | Cancelled
  deriving (Show, Eq)

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
  , tsHistoryScroll      :: !Int
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
  } deriving (Show, Eq)

-- | Project tool cards from the transcript for the Tool Activity pane.
tsTools :: TuiState -> [ToolCard]
tsTools state = [tc | TiToolCard tc <- tsTranscript state]

-- | Compatibility accessor for reading the transcript.
tsHistory :: TuiState -> [TranscriptItem]
tsHistory = tsTranscript

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
  , tsHistoryScroll      = 0
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
  | ActionScrollHistory !Int
  | ActionScrollHistoryToBottom
  | ActionScrollTools !Int
  deriving (Show, Eq)
