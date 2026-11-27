{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.TUI.State
  ( updateTui
  , handleUserKey
  , handleAgentEvent
  , applyProjectInitializationResult
  , applySlashCommandResult
  , toggleToolExpanded
  , shouldAutoScroll
  , isTranscriptAppendingEvent
  ) where

import Hach.Inference (CostPolicy(..))
import Hach.Memory (ProjectInitializationResult(..))
import Hach.Skills (inputSlashCompletion, parseSkillInvocations, skillName)
import Hach.TUI.Conversation (compactTranscriptHistory)
import Hach.TUI.Types
import Hach.TUI.UI (formatTokens)
import Hach.Types
  ( AgentEvent(..)
  , GoalState(..)
  , GoalStatus(..)
  , GoalVerdict(..)
  , SessionTokenUsage(..)
  , TokenUsage(..)
  , ToolCall(..)
  , addUsageToSession
  , contextSaturationPercent
  , goalArgIsClear
  , initialGoalState
  , initialSessionTokenUsage
  , maxGoalConditionLength
  , permissionModeName
  , PermissionMode(..)
  )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Printf (printf)

-- | Apply the result of the IO-side project initialization action.
applyProjectInitializationResult :: ProjectInitializationResult -> TuiState -> TuiState
applyProjectInitializationResult result state =
  let appendNotice notice = state { tsTranscript = appendTranscript [DiNotice notice] (tsTranscript state) }
  in case result of
       ProjectInitialized ->
         appendNotice "Initialized CLAUDE.md guidelines template."
       ProjectAlreadyPresent ->
         appendNotice "CLAUDE.md already exists; left it unchanged."
       ProjectInitializationFailed err ->
         let notice = "Error: " <> err
         in (appendNotice notice)
              { tsStatus = StatusError notice
              , tsFocus = FocusInput
              }

-- | Report the outcome of an IO-backed slash command in the transcript.
applySlashCommandResult :: SlashCommandResult -> TuiState -> TuiState
applySlashCommandResult result state = case result of
  DiffResult (Right diff)
    | T.null (T.strip diff) -> appendNotice "Diff: no changes to tracked files against HEAD."
    | otherwise             -> appendNotice ("Diff against HEAD:\n" <> T.stripEnd diff)
  DiffResult (Left err) ->
    appendNotice ("Diff unavailable: " <> err)
  TaskListResult tasks ->
    appendNotice ("Tasks:\n" <> T.stripEnd tasks)
  ClipboardResult (Right chars) ->
    appendNotice ("Copied last response to clipboard (" <> T.pack (show chars) <> " characters).")
  ClipboardResult (Left err) ->
    appendNotice ("Copy failed: " <> err)
  SkillsReloaded catalog ->
    (appendNotice ("Skills reloaded: " <> T.pack (show (Map.size catalog)) <> " available"))
      { tsSkills = catalog }
  where
    appendNotice notice = state { tsTranscript = appendTranscript [DiNotice notice] (tsTranscript state) }

-- | Text of the most recent non-empty assistant reply, if any.
lastAssistantText :: TuiState -> Maybe T.Text
lastAssistantText = transcriptLastAssistant . tsTranscript

-- | Report the theme named in settings; the TUI has one built-in colour scheme.
themeNotice :: Maybe T.Text -> T.Text
themeNotice = \case
  Just theme -> "Theme: " <> theme <> " (from settings; not applied yet, the TUI always uses its built-in colours)."
  Nothing    -> "Theme: none configured; the TUI uses its built-in colours."

-- | Report which project instructions file reached the system prompt at launch.
memoryNotice :: Maybe FilePath -> T.Text
memoryNotice = \case
  Just file -> "Project memory: " <> T.pack file <> " was loaded into the system prompt at launch."
  Nothing   -> "Project memory: none loaded; no AGENTS.md, AGENT.md or CLAUDE.md with content was found at launch."

-- | Pure state reducer for the TUI.
-- Evaluates an incoming 'TuiEvent' against the current 'TuiState',
-- yielding the updated state and any side-effecting 'TuiAction's.
updateTui :: TuiEvent -> TuiState -> (TuiState, [TuiAction])
updateTui event state = case event of
  EvSubmit prompt ->
    handleSubmitPrompt prompt state

  EvUserKey key ->
    handleUserKey key state

  EvHarness agentEv ->
    (handleAgentEvent agentEv (recordBilledUsage agentEv state), [])

  EvConversation messages
    | tsCancelRequested state -> (state, [])
    | otherwise -> (state { tsConversation = messages }, [])

-- | Add a billed request's usage to the session the moment the TUI observes
-- it. This is the only adder of session usage and spend. It runs before the
-- cancel guard because a request is billed even when its run is interrupted
-- or its output is discarded.
recordBilledUsage :: AgentEvent -> TuiState -> TuiState
recordBilledUsage event state = case event of
  EvLLMResponse _ _ (Just u) -> addUsage u False
  EvGoalEvaluationUsage u    -> addUsage u True
  _                          -> state
  where
    addUsage u isEval = state
      { tsDisplayedSessionTokens = addUsageToSession u isEval (tsDisplayedSessionTokens state)
      , tsIncurredSpendUsd       = tsIncurredSpendUsd state + fromMaybe 0 (tuCost u)
      }

-- | Whether the agent harness is currently busy running an inference turn or tool.
isBusy :: TuiStatus -> Bool
isBusy = \case
  StatusThinking               -> True
  StatusRunningTool _          -> True
  StatusAwaitingPermission _   -> True
  _                            -> False

-- | Format a comprehensive cost and token breakdown report.
formatCostReport :: TuiState -> T.Text
formatCostReport state@TuiState{..} =
  let mLimit = tuiModelContextLimit state
      capacityPart = case mLimit of
        Just limit | limit > 0 ->
          let pct = contextSaturationPercent tsContextTokens limit
          in " — " <> formatTokens tsContextTokens <> "/" <> formatTokens limit <> " capacity (" <> T.pack (show pct) <> "%)"
        _ ->
          " — " <> formatTokens tsContextTokens <> "/? capacity"
      contextLine = case tsTokenUsage of
        Just TokenUsage{..} ->
          let cachePart = if tuCachedTokens > 0 then ", " <> formatTokens tuCachedTokens <> " cached" else ""
              costPart = case tuCost of
                Just c  -> ", $" <> T.pack (printf "%.4f" c)
                Nothing -> ""
          in "Tokens: " <> formatTokens tsContextTokens <> " in context window (" <>
             formatTokens tuPromptTokens <> " prompt" <> cachePart <> ", " <>
             formatTokens tuCompletionTokens <> " completion" <> costPart <> ")" <> capacityPart
        Nothing ->
          "Tokens: " <> formatTokens tsContextTokens <> " in context window" <> capacityPart

      stu = tsDisplayedSessionTokens
      cacheSessionPart = if stuCachedTokens stu > 0 then ", " <> formatTokens (stuCachedTokens stu) <> " cached" else ""
      evalSessionPart = if stuEvaluationTokens stu > 0 then ", " <> formatTokens (stuEvaluationTokens stu) <> " goal-eval" else ""
      sessionLine =
        "Session Cumulative: " <> formatTokens (stuTotalTokens stu) <> " tokens (" <>
        formatTokens (stuPromptTokens stu) <> " prompt" <> cacheSessionPart <> ", " <>
        formatTokens (stuCompletionTokens stu) <> " completion" <> evalSessionPart <> ")"

      costLine = case (stuTotalCost stu, tsCostPolicy) of
        (Just c, _) -> "\nReported API Cost: $" <> T.pack (printf "%.4f" c)
        (Nothing, CostReportedOnly) ->
          "\nAPI Cost: not reported by the endpoint (no estimate is made)"
  in contextLine <> "\n" <> sessionLine <> costLine

-- | Handle submitting a user task prompt.
handleSubmitPrompt :: T.Text -> TuiState -> (TuiState, [TuiAction])
handleSubmitPrompt rawPrompt state
  | T.null trimmed = (state, [])
  | trimmed == "/clear" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          busy = isBusy (tsStatus state)
          actions = if busy then [ActionCancelAgent] else []
          newStatus = if busy then StatusIdle else tsStatus state
      in ( state { tsTranscript         = emptyTranscript
                 , tsConversation       = []
                 , tsTranscriptScroll   = 0
                 , tsTranscriptManualScroll = False
                 , tsSelectedToolIndex  = 0
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 , tsGoalState          = Nothing
                 , tsStatus             = newStatus
                 , tsCancelRequested    = if busy then True else False
                 , tsContextTokens      = 0
                 , tsTokenUsage         = Nothing
                  , tsUsageStatus        = UsageVerified
                  , tsPendingAsk         = Nothing
                  }
         , actions
         )
  | trimmed == "/help" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
      in ( state { tsShowHelp           = True
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/cost reset" || trimmed == "/cost clear" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice "Session token usage reset to 0."] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 , tsDisplayedSessionTokens = initialSessionTokenUsage
                 }
         , []
         )
  | trimmed == "/cost" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          costNotice = formatCostReport state
          newTranscript = appendTranscript [DiNotice costNotice] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/compact" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          hasEarlierTranscript = transcriptLength (tsTranscript state) > 4
          retained = transcriptRetainLast 4 (tsTranscript state)
          retainedItems = transcriptItems retained
          newTranscript = if hasEarlierTranscript
            then (transcriptFromList (DiNotice "Prior conversation turns compacted for context efficiency." : retainedItems))
                   { trVersion = trVersion (tsTranscript state) + 1 }
            else appendTranscript [DiNotice "Conversation history compacted."] (tsTranscript state)
          newConversation = if hasEarlierTranscript
            then compactTranscriptHistory (tsConversation state) retainedItems
            else tsConversation state
      in ( state { tsTranscript         = newTranscript
                 , tsConversation       = newConversation
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed `elem` ["/exit", "/quit"] =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
      in ( state { tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 , tsShouldQuit         = True
                 }
         , [ActionQuit]
         )
  | trimmed == "/model" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice ("Current model: " <> tsModelName state)] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | T.isPrefixOf "/model " trimmed =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          mName = T.strip (T.drop 7 trimmed)
          (newModel, notice) = if T.null mName
            then (tsModelName state, "Usage: /model <model_name>")
            else (mName, "Model switched to: " <> mName)
          newTranscript = appendTranscript [DiNotice notice] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsModelName          = newModel
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/config" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          notice = "Configuration: model=" <> tsModelName state <> ", maxTurns=" <> T.pack (show (tsMaxTurns state))
          newTranscript = appendTranscript [DiNotice notice] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/context" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          notice = "Context tokens: " <> formatTokens (tsContextTokens state) <> " tracked"
          newTranscript = appendTranscript [DiNotice notice] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/resume" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice "/resume is not available inside the TUI. Restart with hach --continue to resume the latest session in this workspace, or hach --session-id <ID> for a specific one."] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/plan" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice "Plan mode activated. Read-only actions allowed."] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 , tsPermissionMode    = ModePlan
                 }
         , [ActionSetPermissionMode ModePlan]
         )
  | trimmed == "/diff" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
      in ( state { tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , [ActionShowDiff]
         )
  | trimmed == "/tasks" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
      in ( state { tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , [ActionListTasks]
         )
  | trimmed == "/theme" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice (themeNotice (tsTheme state))] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/status" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          notice = "Status: " <> (if isBusy (tsStatus state) then "busy" else "idle") <> " | Model: " <> tsModelName state <> " | Turn: " <> T.pack (show (tsCurrentTurn state)) <> "/" <> T.pack (show (tsMaxTurns state))
          newTranscript = appendTranscript [DiNotice notice] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/memory" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice (memoryNotice (tsProjectInstructions state))] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/init" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = tsTranscript state
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , [ActionInitializeProject]
         )
  | trimmed == "/permissions" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          notice = "Permissions policy: " <> permissionModeName (tsPermissionMode state)
          newTranscript = appendTranscript [DiNotice notice] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/fewer-permission-prompts" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice "Permissions set to acceptEdits: Auto-approving file edits."] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 , tsPermissionMode    = ModeAcceptEdits
                 }
         , [ActionSetPermissionMode ModeAcceptEdits]
         )
  | trimmed == "/doctor" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice "/doctor is not implemented: no health checks were run."] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/copy" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          cleared = state { tsInputBuffer        = ""
                          , tsPromptHistory      = newPromptHistory
                          , tsPromptHistoryIndex = Nothing
                          , tsPromptDraft        = ""
                          }
      in case lastAssistantText state of
           Just reply -> (cleared, [ActionCopyToClipboard reply])
           Nothing ->
             ( cleared { tsTranscript = appendTranscript [DiNotice "Nothing to copy: there is no assistant response yet."] (tsTranscript state) }
             , []
             )
  | trimmed == "/reload-skills" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
      in ( state { tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , [ActionReloadSkills]
         )
  | trimmed == "/mcp" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice "/mcp is not implemented: Hach does not connect to MCP servers yet."] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/plugin" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = appendTranscript [DiNotice "/plugin is not implemented: Hach has no plugin system yet."] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/goal" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          (goalNotice, _) = goalStatusText (tsGoalState state)
          newTranscript = appendTranscript [DiNotice goalNotice] (tsTranscript state)
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | T.isPrefixOf "/goal " trimmed =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          argText = T.drop (T.length ("/goal " :: T.Text)) trimmed
      in if goalArgIsClear argText
           then
             let (clearNotice, newGoalState) = case tsGoalState state of
                   Just gs | gsStatus gs `notElem` [GoalCleared, GoalFailed, GoalAchieved] ->
                     ( "Goal cleared: " <> gsCondition gs
                     , Just gs { gsStatus = GoalCleared } )
                   _ ->
                     ( "No goal set", Nothing )
                 newTranscript = appendTranscript [DiNotice clearNotice] (tsTranscript state)
             in ( state { tsTranscript         = newTranscript
                        , tsInputBuffer        = ""
                        , tsPromptHistory      = newPromptHistory
                        , tsPromptHistoryIndex = Nothing
                        , tsPromptDraft        = ""
                        , tsGoalState          = newGoalState
                        }
                , [] )
           else if T.null (T.strip argText)
             then
               let newTranscript = appendTranscript
                     [DiNotice "Usage: /goal <condition> or /goal clear"]
                     (tsTranscript state)
               in ( state { tsTranscript         = newTranscript
                          , tsInputBuffer        = ""
                          , tsPromptHistory      = newPromptHistory
                          , tsPromptHistoryIndex = Nothing
                          , tsPromptDraft        = ""
                          }
                  , [] )
           else if T.length argText > maxGoalConditionLength
             then
               let newTranscript = appendTranscript
                     [DiNotice ("Goal condition too long (max " <>
                       T.pack (show maxGoalConditionLength) <> " characters).")]
                     (tsTranscript state)
               in ( state { tsTranscript         = newTranscript
                          , tsInputBuffer        = ""
                          , tsPromptHistory      = newPromptHistory
                          , tsPromptHistoryIndex = Nothing
                          , tsPromptDraft        = ""
                          }
                  , [] )
           else
             let condition = T.strip argText
                 gs = initialGoalState condition
                 newTranscript = appendTranscript
                   [ DiUser trimmed
                   , DiNotice ("Goal set: " <> condition)
                   ]
                   (tsTranscript state)
             in ( state { tsTranscript         = newTranscript
                        , tsInputBuffer        = ""
                        , tsStatus             = StatusThinking
                        , tsFocus              = FocusTranscript
                        , tsPromptHistory      = newPromptHistory
                        , tsPromptHistoryIndex = Nothing
                        , tsPromptDraft        = ""
                        , tsGoalState          = Just gs
                        , tsTranscriptManualScroll = False
                        , tsCancelRequested    = False
                        }
                , [ActionRunGoal condition] )
  | otherwise =
      let (_, invokedSkills) = parseSkillInvocations (tsSkills state) trimmed
          skillNotices = [ DiNotice ("Activated skill: " <> skillName s) | s <- invokedSkills ]
          newTranscript = appendTranscript (DiUser trimmed : skillNotices) (tsTranscript state)
          newPromptHistory = tsPromptHistory state ++ [trimmed]
          newState = state
            { tsTranscript         = newTranscript
            , tsInputBuffer        = ""
            , tsStatus             = StatusThinking
            , tsFocus              = FocusTranscript
            , tsPromptHistory      = newPromptHistory
            , tsPromptHistoryIndex = Nothing
            , tsPromptDraft        = ""
            , tsCancelRequested    = False
            , tsTranscriptManualScroll = False
            }
      in (newState, [ActionRunAgent trimmed])
  where
    trimmed = T.strip rawPrompt

-- | Process user keypress events.
handleUserKey :: UserKey -> TuiState -> (TuiState, [TuiAction])
handleUserKey key state@TuiState{..} =
  -- 1. Global shortcuts (work in any focus mode)
  case key of
    KeyCtrl 'q' ->
      (state { tsShouldQuit = True }, [ActionQuit])

    KeyCtrl 'c'
      | isBusy tsStatus ->
          ( state { tsCancelRequested = True
                  , tsStatus = StatusError "Turn cancelled by user."
                  , tsFocus = FocusInput
                  , tsTranscript = transcriptCancelUnresolved tsTranscript
                  , tsPendingAsk = Nothing
                  }
          , [ActionCancelAgent]
          )
      | otherwise ->
          (state { tsShouldQuit = True }, [ActionQuit])

    KeyEsc
      | isBusy tsStatus ->
          ( state { tsCancelRequested = True
                  , tsStatus = StatusError "Turn cancelled by user."
                  , tsFocus = FocusInput
                  , tsTranscript = transcriptCancelUnresolved tsTranscript
                  , tsPendingAsk = Nothing
                  }
          , [ActionCancelAgent]
          )
      | tsShowHelp ->
          (state { tsShowHelp = False }, [])
      | otherwise ->
          (state, [])

    other ->
      case tsPendingAsk of
        Just prompt -> handlePermissionKey other prompt state
        Nothing
          -- The help guide is modal, like the permission prompt:
          -- only its dismiss keys act while it is open, so typing
          -- and submitting behind it cannot happen unseen. Ctrl+Q,
          -- Ctrl+C and Esc are handled above and still work.
          | tsShowHelp -> case other of
              KeyF1 -> (state { tsShowHelp = False }, [])
              KeyChar '?' -> (state { tsShowHelp = False }, [])
              _ -> (state, [])
          | otherwise -> handleUnaskedKey other state

handlePermissionKey :: UserKey -> PermissionPrompt -> TuiState -> (TuiState, [TuiAction])
handlePermissionKey key prompt state = case key of
  KeyUp       -> scroll (-1)
  KeyDown     -> scroll 1
  KeyPageUp   -> scroll (-5)
  KeyPageDown -> scroll 5
  KeyScrollUp -> scroll (-3)
  KeyScrollDown -> scroll 3
  KeyChar 'y' -> approve
  KeyChar 'Y' -> approve
  KeyEnter    -> approve
  KeyChar 'n' -> deny
  KeyChar 'N' -> deny
  _           -> (state, [])
  where
    scroll delta = (state, [ActionScrollPermission (ppId prompt) delta])
    approve =
      ( state { tsPendingAsk = Nothing, tsStatus = StatusRunningTool (ppTool prompt) }
      , [ActionRespondPermission (ppId prompt) True]
      )
    deny =
      ( state { tsPendingAsk = Nothing, tsStatus = StatusThinking }
      , [ActionRespondPermission (ppId prompt) False]
      )

handleUnaskedKey :: UserKey -> TuiState -> (TuiState, [TuiAction])
handleUnaskedKey key state@TuiState{..} = case key of
  KeyF1 ->
    (state { tsShowHelp = not tsShowHelp }, [])

  KeyChar 'q'
    | tsFocus /= FocusInput && not (isBusy tsStatus) ->
        (state { tsShouldQuit = True }, [ActionQuit])

  KeyChar '?'
    | tsFocus /= FocusInput ->
        (state { tsShowHelp = not tsShowHelp }, [])

  KeyTab ->
    case inputSlashCompletion tsSkills builtinCommands tsInputBuffer of
      Just suffix | tsFocus == FocusInput ->
        (editInputBuffer (<> suffix) state, [])
      _ ->
        (state { tsFocus = nextFocus tsFocus }, [])

  KeyBackTab ->
    (state { tsFocus = prevFocus tsFocus }, [])

  KeyScrollUp ->
    (state { tsTranscriptScroll = max 0 (tsTranscriptScroll - 2), tsTranscriptManualScroll = True }, [ActionScrollTranscript (-2)])

  KeyScrollDown ->
    (state { tsTranscriptScroll = tsTranscriptScroll + 2, tsTranscriptManualScroll = True }, [ActionScrollTranscript 2])

  _ -> case tsFocus of
    FocusInput ->
      handleInputKey key state
    FocusTranscript ->
      handleTranscriptKey key state

-- | Leave prompt-history browse mode so subsequent Up/Down does not
-- overwrite an in-progress edit of a recalled prompt.
abandonHistoryBrowse :: TuiState -> TuiState
abandonHistoryBrowse state = state { tsPromptHistoryIndex = Nothing }

-- | Apply a buffer edit and leave history browse mode.
editInputBuffer :: (T.Text -> T.Text) -> TuiState -> TuiState
editInputBuffer f state@TuiState{..} =
  abandonHistoryBrowse state { tsInputBuffer = f tsInputBuffer }

-- | True when a Harness event changes the transcript view — either by
-- appending a Transcript Item or by updating a Tool Card in place.
isTranscriptAppendingEvent :: AgentEvent -> Bool
isTranscriptAppendingEvent = \case
  EvLLMResponse{}          -> True
  EvDone{}                 -> True
  EvError{}                -> True
  EvToolCall{}             -> True
  EvToolResult{}           -> True
  EvPermissionDenied{}     -> True
  EvHookTriggered{}        -> True
  EvSessionSaved{}         -> True
  EvNotificationSent{}     -> True
  EvGoalEvaluated{}        -> True
  EvGoalAchieved{}         -> True
  EvGoalFailed{}           -> True
  EvGoalBlocked{}          -> True
  _                        -> False

-- | Key handling inside the input text area.
handleInputKey :: UserKey -> TuiState -> (TuiState, [TuiAction])
handleInputKey key state@TuiState{..} = case key of
  KeyEnter ->
    handleSubmitPrompt tsInputBuffer state

  KeyUp
    | null tsPromptHistory -> (state, [])
    | otherwise ->
        let total = length tsPromptHistory
            (newIdx, draft) = case tsPromptHistoryIndex of
              Nothing  -> (total - 1, tsInputBuffer)
              Just idx -> (max 0 (idx - 1), tsPromptDraft)
            newBuffer = tsPromptHistory !! newIdx
        in ( state { tsInputBuffer        = newBuffer
                   , tsPromptHistoryIndex = Just newIdx
                   , tsPromptDraft        = draft
                   }
           , []
           )

  KeyDown -> case tsPromptHistoryIndex of
    Nothing -> (state, [])
    Just idx
      | idx + 1 < length tsPromptHistory ->
          let newIdx = idx + 1
              newBuffer = tsPromptHistory !! newIdx
          in ( state { tsInputBuffer        = newBuffer
                     , tsPromptHistoryIndex = Just newIdx
                     }
             , []
             )
      | otherwise ->
          -- Reached past the newest prompt: restore draft buffer
          ( state { tsInputBuffer        = tsPromptDraft
                  , tsPromptHistoryIndex = Nothing
                  , tsPromptDraft        = ""
                  }
          , []
          )

  KeyChar c ->
    (editInputBuffer (`T.snoc` c) state, [])

  KeyBackspace ->
    (editInputBuffer (T.dropEnd 1) state, [])

  KeyDelete ->
    (editInputBuffer (T.dropEnd 1) state, [])

  KeyCtrl 'u' ->
    (editInputBuffer (const "") state, [])

  KeyPageUp ->
    (state { tsTranscriptScroll = max 0 (tsTranscriptScroll - 5) }, [ActionScrollTranscript (-5)])

  KeyPageDown ->
    (state { tsTranscriptScroll = tsTranscriptScroll + 5 }, [ActionScrollTranscript 5])

  _ ->
    (state, [])

-- | Key handling when the unified transcript panel is focused.
handleTranscriptKey :: UserKey -> TuiState -> (TuiState, [TuiAction])
handleTranscriptKey key state@TuiState{..} =
  let totalCards = transcriptToolCount tsTranscript
  in case key of
    KeyUp ->
      let scrolled = state { tsTranscriptScroll = max 0 (tsTranscriptScroll - 1), tsTranscriptManualScroll = False }
          selected
            | totalCards > 0 = scrolled { tsSelectedToolIndex = max 0 (tsSelectedToolIndex - 1) }
            | otherwise      = scrolled
      in (selected, [ActionScrollTranscript (-1)])

    KeyDown ->
      let scrolled = state { tsTranscriptScroll = tsTranscriptScroll + 1, tsTranscriptManualScroll = False }
          selected
            | totalCards > 0 = scrolled { tsSelectedToolIndex = min (totalCards - 1) (tsSelectedToolIndex + 1) }
            | otherwise      = scrolled
      in (selected, [ActionScrollTranscript 1])

    KeyPageUp ->
      (state { tsTranscriptScroll = max 0 (tsTranscriptScroll - 5) }, [ActionScrollTranscript (-5)])

    KeyPageDown ->
      (state { tsTranscriptScroll = tsTranscriptScroll + 5 }, [ActionScrollTranscript 5])

    KeyEnter ->
      (toggleToolExpanded tsSelectedToolIndex (state { tsTranscriptManualScroll = False }), [])

    KeyChar ' ' ->
      (toggleToolExpanded tsSelectedToolIndex (state { tsTranscriptManualScroll = False }), [])

    KeyChar 'c'
      | isBusy tsStatus ->
          -- While a turn is active, printable keys are not transcript commands.
          -- In particular, typing 'c' must not clear the in-flight transcript.
          (state, [])
      | otherwise ->
          -- Clear transcript and reset context window tokens when explicitly idle.
          (state { tsTranscript = emptyTranscript, tsConversation = [], tsTranscriptScroll = 0, tsTranscriptManualScroll = False, tsSelectedToolIndex = 0, tsContextTokens = 0, tsTokenUsage = Nothing, tsUsageStatus = UsageVerified }, [])

    _ ->
      (state, [])

-- | Toggle the expanded state of a tool card.
toggleToolExpanded :: Int -> TuiState -> TuiState
toggleToolExpanded targetIdx state@TuiState{..} =
  state { tsTranscript = transcriptToggleTool targetIdx tsTranscript }

-- | Pure update of state when an 'AgentEvent' arrives from the harness.
-- When a cancel has been requested (via /clear, Esc, or Ctrl+C while busy),
-- stale in-flight events are dropped to prevent ghost output from the
-- cancelled turn polluting the cleared history.
handleAgentEvent :: AgentEvent -> TuiState -> TuiState
handleAgentEvent event state@TuiState{..}
  | tsCancelRequested = state
  | otherwise = case event of
  EvTurnStart n ->
    state { tsCurrentTurn = n, tsStatus = StatusThinking }

  EvPromptingLLM _ ->
    state { tsStatus = StatusThinking }

  EvLLMResponse mContent calls mUsage ->
    let textItems = case mContent of
          Just c | not (T.null (T.strip c)) -> [TiAssistant c]
          _                                  -> []
        cardItems = [ TiToolCard ToolCard
                        { tcId        = callId tc
                        , tcName      = functionName tc
                        , tcArgs      = callArgsRaw tc
                        , tcLifecycle = Pending
                        , tcExpanded  = False
                        }
                    | tc <- calls
                    ]
        newTranscript = appendTranscript (textItems ++ cardItems) tsTranscript
        newStatus = if null calls then StatusFinished else tsStatus
        (newContextTokens, newUsage, newUsageStatus) = case mUsage of
          Just u  -> (tuTotalTokens u, Just u, UsageVerified)
          Nothing -> (tsContextTokens, tsTokenUsage, UsageMissing)
    in state
         { tsTranscript    = newTranscript
         , tsStatus        = newStatus
         , tsContextTokens = newContextTokens
         , tsTokenUsage    = newUsage
         , tsUsageStatus   = newUsageStatus
         }

  EvToolCall name args ->
    let (newTranscript, mIdx) = transcriptUpdateFirstPending name args tsTranscript
        newSelected = case mIdx of
          Just idx -> idx
          Nothing  -> tsSelectedToolIndex
    in state
      { tsTranscript        = newTranscript
      , tsStatus            = StatusRunningTool name
      , tsSelectedToolIndex = newSelected
      }

  EvToolResult _name res ->
    let newTranscript = transcriptUpdateFirstRunning res tsTranscript
    in state { tsTranscript = newTranscript, tsStatus = StatusThinking }

  EvDone ans ->
    let finalTranscript = case transcriptLast tsTranscript of
          Just lastItem | lastItem == TiAssistant ans -> tsTranscript
          _ -> appendTranscript [TiAssistant ans] tsTranscript
    in state { tsTranscript = finalTranscript, tsStatus = StatusFinished, tsFocus = FocusInput, tsPendingAsk = Nothing }

  EvError err ->
    state
      { tsTranscript = appendTranscript [TiNotice ("Error: " <> err)] tsTranscript
      , tsStatus     = StatusError err
      , tsFocus      = FocusInput
      , tsPendingAsk = Nothing
      }

  EvTurnComplete _ ->
    state

  EvGoalSet cond ->
    state { tsGoalState = Just (initialGoalState cond) }

  EvGoalEvaluated verdict reason ->
    case tsGoalState of
      Just gs -> state
        { tsGoalState = Just gs
          { gsLastVerdict = Just verdict
          , gsLastReason  = Just reason
          , gsTurnCount   = gsTurnCount gs + 1
          }
        , tsTranscript = appendTranscript [TiNotice ("Goal evaluated: " <> verdictText verdict <> " — " <> reason)] tsTranscript
        }
      Nothing -> state

  -- Recorded by 'recordBilledUsage'.
  EvGoalEvaluationUsage _ ->
    state

  EvGoalAchieved cond ->
    case tsGoalState of
      Just gs -> state
        { tsGoalState = Just gs { gsStatus = GoalAchieved }
        , tsTranscript = appendTranscript [TiNotice ("Goal achieved: " <> cond)] tsTranscript
        , tsFocus = FocusInput
        }
      Nothing -> state

  EvGoalFailed cond reason ->
    case tsGoalState of
      Just gs -> state
        { tsGoalState = Just gs { gsStatus = GoalFailed }
        , tsTranscript = appendTranscript [TiNotice ("Goal failed: " <> cond <> " — " <> reason)] tsTranscript
        , tsFocus = FocusInput
        }
      Nothing -> state

  EvGoalCleared _ ->
    state { tsGoalState = Nothing }

  EvGoalBlocked cond ->
    case tsGoalState of
      Just _ -> state
        { tsTranscript = appendTranscript
          [ TiNotice ("No progress detected. Goal still active: " <> cond)
          , TiNotice "Run /goal again to continue after your next prompt."
          ] tsTranscript
        , tsFocus = FocusInput
        }
      Nothing -> state

  EvPartialResponse _ -> state
  EvToolCallDelta _ -> state
  EvPermissionDenied tool reason ->
    let updatedTranscript = transcriptDenyFirstMatching tool reason tsTranscript
        finalTranscript   = appendTranscript [TiNotice ("Permission denied for " <> tool <> ": " <> reason)] updatedTranscript
    in state { tsTranscript = finalTranscript, tsStatus = StatusThinking, tsPendingAsk = Nothing }

  EvPermissionAsk askId tool args reason ->
    state
      { tsPendingAsk = Just (PermissionPrompt askId tool args reason)
      , tsStatus     = StatusAwaitingPermission tool
      }
  EvPermissionModeChanged mode ->
    state { tsPermissionMode = mode }
  EvHookTriggered hook res ->
    state { tsTranscript = appendTranscript [TiNotice ("Hook triggered: " <> hook <> " -> " <> res)] tsTranscript }
  EvSessionSaved path ->
    state { tsTranscript = appendTranscript [DiNotice ("Session saved to " <> path)] tsTranscript }
  EvNotificationSent msg ->
    state { tsTranscript = appendTranscript [DiNotice ("Notification: " <> msg)] tsTranscript }

-- | Render a goal verdict as display text.
verdictText :: GoalVerdict -> T.Text
verdictText = \case
  GoalMet         -> "met"
  GoalNotYetMet   -> "not yet met"
  GoalImpossible  -> "impossible"

-- | Produce a status notice and updated goal state for the `/goal` command.
goalStatusText :: Maybe GoalState -> (T.Text, Maybe GoalState)
goalStatusText Nothing = ("No goal set", Nothing)
goalStatusText (Just gs) =
  case gsStatus gs of
    GoalActive ->
      let reasonText = case gsLastReason gs of
            Just r  -> "  Reason: " <> r
            Nothing -> ""
      in ( "Goal active: " <> gsCondition gs
         <> "  Turns: " <> T.pack (show (gsTurnCount gs))
         <> reasonText
         , Just gs )
    GoalAchieved ->
      ( "Goal achieved: " <> gsCondition gs
      <> "  Turns: " <> T.pack (show (gsTurnCount gs))
      , Just gs )
    GoalFailed ->
      let reasonText = case gsLastReason gs of
            Just r  -> "  Reason: " <> r
            Nothing -> ""
      in ( "Goal failed: " <> gsCondition gs <> reasonText
         , Just gs )
    GoalCleared ->
      ( "Goal cleared: " <> gsCondition gs
      , Just gs )

-- | Determine whether the transcript viewport should automatically scroll to the bottom.
-- Manual scrolling takes precedence over following incoming output.
-- Otherwise, follow appending events when focus is on the prompt input.
shouldAutoScroll :: TuiState -> AgentEvent -> Bool
shouldAutoScroll state ev =
  not (tsTranscriptManualScroll state)
    && tsFocus state == FocusInput && isTranscriptAppendingEvent ev
