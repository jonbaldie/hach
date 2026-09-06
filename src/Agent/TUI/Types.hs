{-# LANGUAGE OverloadedStrings #-}

module Agent.TUI.Types
  ( -- * Status and Focus
    TuiStatus(..)
  , FocusArea(..)
  , nextFocus
  , prevFocus

    -- * Screen Elements
  , ToolItem(..)
  , DialogueItem(..)

    -- * State
  , TuiState(..)
  , initialTuiState

    -- * Events and Actions
  , UserKey(..)
  , TuiEvent(..)
  , TuiAction(..)
  ) where

import Agent.Skills (SkillCatalog)
import Agent.Types (AgentEvent, GoalState, TokenUsage, ToolResult)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

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

-- | Individual tool execution card shown in the activity panel.
data ToolItem = ToolItem
  { tiName     :: !Text
  , tiArgs     :: !Text
  , tiResult   :: !(Maybe ToolResult)
  , tiExpanded :: !Bool
  } deriving (Show, Eq)

-- | Dialogue history entry shown in the conversation panel.
data DialogueItem
  = DiUser !Text
  | DiAssistant !Text
  | DiSystem !Text
  | DiNotice !Text
  deriving (Show, Eq)

-- | Full screen state for the TUI.
data TuiState = TuiState
  { tsModelName          :: !Text
  , tsCurrentTurn        :: !Int
  , tsMaxTurns           :: !(Maybe Int)
  , tsStatus             :: !TuiStatus
  , tsFocus              :: !FocusArea
  , tsInputBuffer        :: !Text
  , tsHistory            :: ![DialogueItem]
  , tsTools              :: ![ToolItem]
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
  , tsSkills             :: !SkillCatalog
  , tsGoalState          :: !(Maybe GoalState)
  } deriving (Show, Eq)

-- | Initialize a clean TUI state.
initialTuiState :: Text -> Maybe Int -> TuiState
initialTuiState model maxTurns = TuiState
  { tsModelName          = model
  , tsCurrentTurn        = 0
  , tsMaxTurns           = maxTurns
  , tsStatus             = StatusIdle
  , tsFocus              = FocusInput
  , tsInputBuffer        = ""
  , tsHistory            = []
  , tsTools              = []
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
