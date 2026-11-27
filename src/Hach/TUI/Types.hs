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
  , Transcript(..)
  , emptyTranscript
  , transcriptNull
  , transcriptLength
  , transcriptToolCount
  , transcriptToolItemIndex
  , transcriptItems
  , transcriptFromList
  , transcriptLast
  , transcriptLastAssistant
  , appendTranscript
  , transcriptSlice
  , transcriptRetainLast
  , transcriptToggleTool
  , transcriptUpdateFirstPending
  , transcriptUpdateFirstRunning
  , transcriptDenyFirstMatching
  , transcriptCancelUnresolved
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
  , tuiModelContextLimit
  , UsageStatus(..)

    -- * Events and Actions
  , UserKey(..)
  , TuiEvent(..)
  , SlashCommandResult(..)
  , TuiAction(..)
  , pattern ActionScrollHistory

    -- * Built-in slash commands
  , builtinCommands
  ) where

import Hach.Inference (CostPolicy(..))
import Hach.Skills (SkillCatalog)
import Hach.Types (AgentEvent, GoalState, Message, PermissionMode (..), SessionTokenUsage, TokenUsage, ToolResult, initialSessionTokenUsage, modelContextLimit)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, ViewL(..), ViewR(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

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

-- | Append-efficient, indexed transcript storing chronological items along
-- with tool-card positions and active unresolved tool queues so appends are
-- O(1) amortized and lifecycle updates are O(log N) without scanning history.
data Transcript = Transcript
  { trItems         :: !(Seq TranscriptItem)
  , trToolPositions :: !(Seq Int)
  , trPendingTools  :: !(Seq (Int, Int))
  , trRunningTools  :: !(Seq (Int, Int))
  , trVersion       :: !Int
  } deriving (Show)

instance Eq Transcript where
  a == b = trItems a == trItems b

-- | An empty transcript.
emptyTranscript :: Transcript
emptyTranscript = Transcript Seq.empty Seq.empty Seq.empty Seq.empty 0

-- | O(1) check whether the transcript has no items.
transcriptNull :: Transcript -> Bool
transcriptNull = Seq.null . trItems

-- | O(1) total number of items in the transcript.
transcriptLength :: Transcript -> Int
transcriptLength = Seq.length . trItems

-- | O(1) total number of tool cards in the transcript.
transcriptToolCount :: Transcript -> Int
transcriptToolCount = Seq.length . trToolPositions

-- | O(log N) lookup of the transcript-item index for the k-th tool card.
transcriptToolItemIndex :: Int -> Transcript -> Maybe Int
transcriptToolItemIndex cardIdx Transcript{..}
  | cardIdx >= 0 && cardIdx < Seq.length trToolPositions =
      Just (Seq.index trToolPositions cardIdx)
  | otherwise = Nothing

-- | Materialize the transcript items as a chronological list.
transcriptItems :: Transcript -> [TranscriptItem]
transcriptItems = toList . trItems

-- | Build an indexed transcript from a chronological list of items.
transcriptFromList :: [TranscriptItem] -> Transcript
transcriptFromList items = appendTranscript items emptyTranscript

-- | O(1) access to the last item in the transcript, if non-empty.
transcriptLast :: Transcript -> Maybe TranscriptItem
transcriptLast Transcript{..} = case Seq.viewr trItems of
  _ :> x -> Just x
  EmptyR -> Nothing

-- | Walk backward from the end of the transcript to find the latest non-blank
-- assistant response without reversing the entire transcript.
transcriptLastAssistant :: Transcript -> Maybe Text
transcriptLastAssistant Transcript{..} = go (Seq.viewr trItems)
  where
    go EmptyR = Nothing
    go (rest :> item) = case item of
      TiAssistant t | not (T.null (T.strip t)) -> Just t
      _                                        -> go (Seq.viewr rest)

-- | Append chronological items in O(k) time, indexing any tool cards.
appendTranscript :: [TranscriptItem] -> Transcript -> Transcript
appendTranscript [] tr = tr
appendTranscript items tr0 =
  let step tr item =
        let itemIdx = Seq.length (trItems tr)
            items'  = trItems tr |> item
        in case item of
          TiToolCard tc ->
            let cardIdx = Seq.length (trToolPositions tr)
                pos'    = trToolPositions tr |> itemIdx
                (pend', run') = case tcLifecycle tc of
                  Pending -> (trPendingTools tr |> (itemIdx, cardIdx), trRunningTools tr)
                  Running -> (trPendingTools tr, trRunningTools tr |> (itemIdx, cardIdx))
                  _       -> (trPendingTools tr, trRunningTools tr)
            in tr
                 { trItems         = items'
                 , trToolPositions = pos'
                 , trPendingTools  = pend'
                 , trRunningTools  = run'
                 }
          _ -> tr { trItems = items' }
      updated = foldl' step tr0 items
  in updated { trVersion = trVersion tr0 + 1 }

-- | O(log N + (end - start)) slice of @(itemIndex, item)@ for @[start .. end)@.
transcriptSlice :: Int -> Int -> Transcript -> [(Int, TranscriptItem)]
transcriptSlice start end Transcript{..}
  | start >= end || start >= Seq.length trItems = []
  | otherwise =
      let s = max 0 start
          e = min (Seq.length trItems) end
          slice = Seq.take (e - s) (Seq.drop s trItems)
      in zip [s ..] (toList slice)

-- | Retain only the last @k@ items, rebuilding the tool-card indices over them.
transcriptRetainLast :: Int -> Transcript -> Transcript
transcriptRetainLast k tr@Transcript{..}
  | k <= 0 = emptyTranscript { trVersion = trVersion + 1 }
  | Seq.length trItems <= k = tr
  | otherwise =
      let kept = Seq.drop (Seq.length trItems - k) trItems
      in (transcriptFromList (toList kept)) { trVersion = trVersion + 1 }

-- | Update a tool card at a known transcript-item index in O(log N).
adjustToolCardAt :: Int -> (ToolCard -> ToolCard) -> Seq TranscriptItem -> Seq TranscriptItem
adjustToolCardAt itemIdx f = Seq.adjust' update itemIdx
  where
    update (TiToolCard tc) = TiToolCard (f tc)
    update other           = other

-- | O(log N) toggle of the @tcExpanded@ flag for the k-th tool card.
transcriptToggleTool :: Int -> Transcript -> Transcript
transcriptToggleTool cardIdx tr@Transcript{..} =
  case transcriptToolItemIndex cardIdx tr of
    Nothing -> tr
    Just itemIdx ->
      tr { trItems   = adjustToolCardAt itemIdx (\tc -> tc { tcExpanded = not (tcExpanded tc) }) trItems
         , trVersion = trVersion + 1
         }

-- | O(log N) transition of the first 'Pending' tool card to 'Running'.
transcriptUpdateFirstPending :: Text -> Text -> Transcript -> (Transcript, Maybe Int)
transcriptUpdateFirstPending name args tr = go (trPendingTools tr)
  where
    go q = case Seq.viewl q of
      EmptyL -> (tr { trPendingTools = Seq.empty }, Nothing)
      (itemIdx, cardIdx) :< rest ->
        case Seq.lookup itemIdx (trItems tr) of
          Just (TiToolCard tc) | tcLifecycle tc == Pending ->
            let items' = adjustToolCardAt itemIdx
                  (\c -> c { tcName = name, tcArgs = args, tcLifecycle = Running })
                  (trItems tr)
                tr' = tr
                  { trItems        = items'
                  , trPendingTools = rest
                  , trRunningTools = trRunningTools tr |> (itemIdx, cardIdx)
                  , trVersion      = trVersion tr + 1
                  }
            in (tr', Just cardIdx)
          _ -> go rest

-- | O(log N) transition of the first 'Running' tool card to 'Finished'.
transcriptUpdateFirstRunning :: ToolResult -> Transcript -> Transcript
transcriptUpdateFirstRunning res tr = go (trRunningTools tr)
  where
    go q = case Seq.viewl q of
      EmptyL -> tr { trRunningTools = Seq.empty }
      (itemIdx, _) :< rest ->
        case Seq.lookup itemIdx (trItems tr) of
          Just (TiToolCard tc) | tcLifecycle tc == Running ->
            let items' = adjustToolCardAt itemIdx
                  (\c -> c { tcLifecycle = Finished res })
                  (trItems tr)
            in tr
                 { trItems        = items'
                 , trRunningTools = rest
                 , trVersion      = trVersion tr + 1
                 }
          _ -> go rest

-- | Transition the earliest unresolved ('Pending' or 'Running') tool card
-- matching @tool@ to 'Denied', scanning only unresolved queues.
transcriptDenyFirstMatching :: Text -> Text -> Transcript -> Transcript
transcriptDenyFirstMatching tool reason tr =
  let matchesUnresolved (itemIdx, _) = case Seq.lookup itemIdx (trItems tr) of
        Just (TiToolCard tc) ->
          (tcLifecycle tc == Pending || tcLifecycle tc == Running) && tcName tc == tool
        _ -> False
      findFirst q = case Seq.viewl q of
        EmptyL -> Nothing
        x :< rest
          | matchesUnresolved x -> Just (fst x, rest)
          | otherwise -> fmap (fmap (x Seq.<|)) (findFirst rest)
      firstPend = findFirst (trPendingTools tr)
      firstRun  = findFirst (trRunningTools tr)
      chosen = case (firstRun, firstPend) of
        (Just (ri, rRest), Just (pi', pRest))
          | ri <= pi' -> Just (ri, True, rRest)
          | otherwise -> Just (pi', False, pRest)
        (Just (ri, rRest), Nothing)  -> Just (ri, True, rRest)
        (Nothing, Just (pi', pRest)) -> Just (pi', False, pRest)
        (Nothing, Nothing)           -> Nothing
  in case chosen of
       Nothing -> tr
       Just (itemIdx, fromRun, restQueue) ->
         let items' = adjustToolCardAt itemIdx
               (\c -> c { tcLifecycle = Denied reason })
               (trItems tr)
         in tr
              { trItems        = items'
              , trPendingTools = if fromRun then trPendingTools tr else restQueue
              , trRunningTools = if fromRun then restQueue else trRunningTools tr
              , trVersion      = trVersion tr + 1
              }

-- | Mark every unresolved ('Pending' or 'Running') tool card as 'Cancelled'
-- in O(U log N) time where U is the number of unresolved cards.
transcriptCancelUnresolved :: Transcript -> Transcript
transcriptCancelUnresolved tr@Transcript{..}
  | Seq.null trPendingTools && Seq.null trRunningTools = tr
  | otherwise =
      let cancelAt items (itemIdx, _) = case Seq.lookup itemIdx items of
            Just (TiToolCard tc) | tcLifecycle tc `elem` [Pending, Running] ->
              adjustToolCardAt itemIdx (\c -> c { tcLifecycle = Cancelled }) items
            _ -> items
          items1 = foldl' cancelAt trItems trPendingTools
          items2 = foldl' cancelAt items1 trRunningTools
      in tr
           { trItems        = items2
           , trPendingTools = Seq.empty
           , trRunningTools = Seq.empty
           , trVersion      = trVersion + 1
           }

-- | Full screen state for the TUI.
data TuiState = TuiState
  { tsModelName          :: !Text
  , tsCurrentTurn        :: !Int
  , tsMaxTurns           :: !(Maybe Int)
  , tsStatus             :: !TuiStatus
  , tsFocus              :: !FocusArea
  , tsInputBuffer        :: !Text
  , tsTranscript         :: !Transcript
  , tsConversation       :: ![Message]
    -- ^ Canonical Core history for model calls and session persistence.
  , tsIncurredSpendUsd   :: !Double
    -- ^ Reported cost of every billed request observed in this TUI, including
    -- requests of interrupted runs. Saved as the session's spend; @/cost reset@
    -- does not clear it.
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
  , tsDisplayedSessionTokens :: !SessionTokenUsage
    -- ^ Session usage shown by @/cost@. @/cost reset@ clears it.
  , tsUsageStatus        :: !UsageStatus
  , tsSkills             :: !SkillCatalog
  , tsGoalState          :: !(Maybe GoalState)
  , tsPermissionMode     :: !PermissionMode
  , tsPendingAsk         :: !(Maybe PermissionPrompt)
  , tsTheme              :: !(Maybe Text)
    -- ^ Theme named in settings; reported by @/theme@, not yet applied.
  , tsProjectInstructions :: !(Maybe FilePath)
    -- ^ Instructions file loaded into the system prompt at launch.
  , tsCostPolicy         :: !CostPolicy
    -- ^ How @/cost@ treats cost the endpoint did not report. Non-secret; set
    -- from the connection at launch, not guessed from the model name.
  , tsModelLimits        :: !(Map Text Int)
    -- ^ Model metadata context limits.
  } deriving (Show, Eq)

-- | Resolve the active model's context limit in the TUI, if known.
tuiModelContextLimit :: TuiState -> Maybe Int
tuiModelContextLimit state =
  modelContextLimit (tsModelLimits state) (tsModelName state)

-- | Project tool cards from the transcript for the Tool Activity pane.
tsTools :: TuiState -> [ToolCard]
tsTools state = [tc | TiToolCard tc <- transcriptItems (tsTranscript state)]

-- | Compatibility accessor for reading the transcript.
tsHistory :: TuiState -> [TranscriptItem]
tsHistory = transcriptItems . tsTranscript

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
  , tsTranscript         = emptyTranscript
  , tsConversation       = []
  , tsIncurredSpendUsd   = 0
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
  , tsDisplayedSessionTokens = initialSessionTokenUsage
  , tsUsageStatus        = UsageVerified
  , tsSkills             = Map.empty
  , tsGoalState          = Nothing
  , tsPermissionMode     = ModeDefault
  , tsPendingAsk         = Nothing
  , tsTheme              = Nothing
  , tsProjectInstructions = Nothing
  , tsCostPolicy         = CostReportedOnly
  , tsModelLimits        = Map.empty
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
  | EvConversation ![Message]
  | EvSubmit !Text
  deriving (Show, Eq)

-- | Outcome of an IO-backed slash command, reported back into the transcript.
data SlashCommandResult
  = DiffResult !(Either Text Text)
    -- ^ @git diff HEAD@ output, or the reason it could not be read.
  | TaskListResult !Text
    -- ^ The task list exactly as the @task_list@ tool reports it.
  | ClipboardResult !(Either Text Int)
    -- ^ Characters copied to the clipboard, or why the copy failed.
  | SkillsReloaded !SkillCatalog
    -- ^ The skill catalog just read from the current workspace.
  deriving (Show, Eq)

-- | Actions requested by the reducer for the external environment to perform.
data TuiAction
  = ActionInitializeProject
  | ActionShowDiff
  | ActionListTasks
  | ActionCopyToClipboard !Text
  | ActionReloadSkills
  | ActionRunAgent !Text
  | ActionRunGoal !Text
  | ActionCancelAgent
  | ActionQuit
  | ActionScrollTranscript !Int
  | ActionScrollPermission !Int !Int
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

{-# COMPLETE ActionInitializeProject, ActionShowDiff, ActionListTasks, ActionCopyToClipboard, ActionReloadSkills, ActionRunAgent, ActionRunGoal, ActionCancelAgent, ActionQuit, ActionScrollTranscript, ActionScrollPermission, ActionSetPermissionMode, ActionRespondPermission #-}
{-# COMPLETE ActionInitializeProject, ActionShowDiff, ActionListTasks, ActionCopyToClipboard, ActionReloadSkills, ActionRunAgent, ActionRunGoal, ActionCancelAgent, ActionQuit, ActionScrollHistory, ActionScrollPermission, ActionSetPermissionMode, ActionRespondPermission #-}
