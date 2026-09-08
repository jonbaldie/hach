{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.TUI.State
  ( updateTui
  , handleUserKey
  , handleAgentEvent
  , toggleToolExpanded
  , shouldAutoScroll
  , isTranscriptAppendingEvent
  ) where

import Hach.Sessions (estimateCostUsd)
import Hach.Skills (inputSlashCompletion, parseSkillInvocations, skillName)
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
  , ToolResult
  , addUsageToSession
  , contextSaturationPercent
  , goalArgIsClear
  , initialGoalState
  , initialSessionTokenUsage
  , maxGoalConditionLength
  , permissionModeName
  , PermissionMode(..)
  , modelContextLimit
  )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Text.Printf (printf)

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
    (handleAgentEvent agentEv state, [])

-- | Whether the agent harness is currently busy running an inference turn or tool.
isBusy :: TuiStatus -> Bool
isBusy = \case
  StatusThinking      -> True
  StatusRunningTool _ -> True
  _                   -> False

-- | Format a comprehensive cost and token breakdown report.
formatCostReport :: TuiState -> T.Text
formatCostReport TuiState{..} =
  let limit = modelContextLimit tsModelName
      pct = contextSaturationPercent tsContextTokens tsModelName
      contextLine = case tsTokenUsage of
        Just TokenUsage{..} ->
          let cachePart = if tuCachedTokens > 0 then ", " <> formatTokens tuCachedTokens <> " cached" else ""
              costPart = case tuCost of
                Just c  -> ", $" <> T.pack (printf "%.4f" c)
                Nothing -> ""
          in "Tokens: " <> formatTokens tsContextTokens <> " in context window (" <>
             formatTokens tuPromptTokens <> " prompt" <> cachePart <> ", " <>
             formatTokens tuCompletionTokens <> " completion" <> costPart <> ") — " <>
             formatTokens tsContextTokens <> "/" <> formatTokens limit <> " capacity (" <> T.pack (show pct) <> "%)"
        Nothing ->
          "Tokens: " <> formatTokens tsContextTokens <> " in context window — " <>
          formatTokens tsContextTokens <> "/" <> formatTokens limit <> " capacity (" <> T.pack (show pct) <> "%)"

      stu = tsSessionTokens
      cacheSessionPart = if stuCachedTokens stu > 0 then ", " <> formatTokens (stuCachedTokens stu) <> " cached" else ""
      evalSessionPart = if stuEvaluationTokens stu > 0 then ", " <> formatTokens (stuEvaluationTokens stu) <> " goal-eval" else ""
      sessionLine =
        "Session Cumulative: " <> formatTokens (stuTotalTokens stu) <> " tokens (" <>
        formatTokens (stuPromptTokens stu) <> " prompt" <> cacheSessionPart <> ", " <>
        formatTokens (stuCompletionTokens stu) <> " completion" <> evalSessionPart <> ")"

      costLine = case stuTotalCost stu of
        Just c  -> "\nReported API Cost: $" <> T.pack (printf "%.4f" c)
        Nothing ->
          if stuTotalTokens stu > 0
            then let est = estimateCostUsd tsModelName (stuPromptTokens stu) (stuCompletionTokens stu)
                 in "\nEstimated Cost: ~$" <> T.pack (printf "%.4f" est)
            else ""
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
      in ( state { tsTranscript         = []
                 , tsTranscriptScroll   = 0
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
          newTranscript = tsTranscript state ++ [DiNotice "Session token usage reset to 0."]
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 , tsSessionTokens      = initialSessionTokenUsage
                 }
         , []
         )
  | trimmed == "/cost" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          costNotice = formatCostReport state
          newTranscript = tsTranscript state ++ [DiNotice costNotice]
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
          newTranscript = if length (tsTranscript state) > 4
            then DiNotice "Prior conversation turns compacted for context efficiency." : drop (length (tsTranscript state) - 4) (tsTranscript state)
            else tsTranscript state ++ [DiNotice "Conversation history compacted."]
      in ( state { tsTranscript         = newTranscript
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
          newTranscript = tsTranscript state ++ [DiNotice ("Current model: " <> tsModelName state)]
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
          newTranscript = tsTranscript state ++ [DiNotice notice]
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
          newTranscript = tsTranscript state ++ [DiNotice notice]
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
          newTranscript = tsTranscript state ++ [DiNotice notice]
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
          newTranscript = tsTranscript state ++ [DiNotice "Session resume initialized."]
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
          newTranscript = tsTranscript state ++ [DiNotice "Plan mode activated. Read-only actions allowed."]
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
          newTranscript = tsTranscript state ++ [DiNotice "Git working tree diff inspected."]
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/tasks" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = tsTranscript state ++ [DiNotice "Task list: No active background tasks."]
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/theme" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = tsTranscript state ++ [DiNotice "Theme: dark"]
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
          newTranscript = tsTranscript state ++ [DiNotice notice]
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
          newTranscript = tsTranscript state ++ [DiNotice "Project memory instructions active."]
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
          newTranscript = tsTranscript state ++ [DiNotice "Initialized CLAUDE.md guidelines template."]
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/permissions" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          notice = "Permissions policy: " <> permissionModeName (tsPermissionMode state)
          newTranscript = tsTranscript state ++ [DiNotice notice]
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
          newTranscript = tsTranscript state ++ [DiNotice "Permissions set to acceptEdits: Auto-approving file edits."]
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
          newTranscript = tsTranscript state ++ [DiNotice "Doctor: All systems operational."]
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
          newTranscript = tsTranscript state ++ [DiNotice "Last response copied to clipboard."]
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/reload-skills" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = tsTranscript state ++ [DiNotice ("Skills reloaded: " <> T.pack (show (Map.size (tsSkills state))) <> " available")]
      in ( state { tsTranscript         = newTranscript
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/mcp" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newTranscript = tsTranscript state ++ [DiNotice "MCP: Model Context Protocol servers loaded."]
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
          newTranscript = tsTranscript state ++ [DiNotice "Plugins: 0 loaded"]
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
          newTranscript = tsTranscript state ++ [DiNotice goalNotice]
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
                 newTranscript = tsTranscript state ++ [DiNotice clearNotice]
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
               let newTranscript = tsTranscript state ++
                     [DiNotice "Usage: /goal <condition> or /goal clear"]
               in ( state { tsTranscript         = newTranscript
                          , tsInputBuffer        = ""
                          , tsPromptHistory      = newPromptHistory
                          , tsPromptHistoryIndex = Nothing
                          , tsPromptDraft        = ""
                          }
                  , [] )
           else if T.length argText > maxGoalConditionLength
             then
               let newTranscript = tsTranscript state ++
                     [DiNotice ("Goal condition too long (max " <>
                       T.pack (show maxGoalConditionLength) <> " characters).")]
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
                 newTranscript = tsTranscript state ++
                   [ DiUser trimmed
                   , DiNotice ("Goal set: " <> condition)
                   ]
             in ( state { tsTranscript         = newTranscript
                        , tsInputBuffer        = ""
                        , tsStatus             = StatusThinking
                        , tsFocus              = FocusTranscript
                        , tsPromptHistory      = newPromptHistory
                        , tsPromptHistoryIndex = Nothing
                        , tsPromptDraft        = ""
                        , tsGoalState          = Just gs
                        , tsCancelRequested    = False
                        }
                , [ActionRunGoal condition] )
  | otherwise =
      let (_, invokedSkills) = parseSkillInvocations (tsSkills state) trimmed
          skillNotices = [ DiNotice ("Activated skill: " <> skillName s) | s <- invokedSkills ]
          newTranscript = tsTranscript state ++ [DiUser trimmed] ++ skillNotices
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
                  , tsTranscript = cancelUnresolvedToolCards tsTranscript
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
                  , tsTranscript = cancelUnresolvedToolCards tsTranscript
                  }
          , [ActionCancelAgent]
          )
      | tsShowHelp ->
          (state { tsShowHelp = False }, [])
      | otherwise ->
          (state, [])

    KeyF1 ->
      (state { tsShowHelp = not tsShowHelp }, [])

    KeyChar 'q'
      | tsFocus /= FocusInput && not (isBusy tsStatus) ->
          (state { tsShouldQuit = True }, [ActionQuit])

    KeyChar '?'
      | tsFocus /= FocusInput ->
          (state { tsShowHelp = not tsShowHelp }, [])

    KeyTab ->
      -- Accept the inline slash-completion ghost text (built-in commands
      -- and user-invocable skills) when the user is typing a slash-command
      -- prefix in the input box; otherwise cycle panel focus as usual.
      case inputSlashCompletion tsSkills builtinCommands tsInputBuffer of
        Just suffix | tsFocus == FocusInput ->
          (editInputBuffer (<> suffix) state, [])
        _ ->
          (state { tsFocus = nextFocus tsFocus }, [])

    KeyBackTab ->
      (state { tsFocus = prevFocus tsFocus }, [])

    KeyScrollUp ->
      (state { tsTranscriptScroll = max 0 (tsTranscriptScroll - 2) }, [ActionScrollTranscript (-2)])

    KeyScrollDown ->
      (state { tsTranscriptScroll = tsTranscriptScroll + 2 }, [ActionScrollTranscript 2])

    -- 2. Focus-specific actions
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
  let toolCards = [tc | TiToolCard tc <- tsTranscript]
      totalCards = length toolCards
  in case key of
    KeyUp
      | totalCards > 0 ->
          let newIdx = max 0 (tsSelectedToolIndex - 1)
          in (state { tsSelectedToolIndex = newIdx }, [])
      | otherwise ->
          (state { tsTranscriptScroll = max 0 (tsTranscriptScroll - 1) }, [ActionScrollTranscript (-1)])

    KeyDown
      | totalCards > 0 ->
          let newIdx = min (totalCards - 1) (tsSelectedToolIndex + 1)
          in (state { tsSelectedToolIndex = newIdx }, [])
      | otherwise ->
          (state { tsTranscriptScroll = tsTranscriptScroll + 1 }, [ActionScrollTranscript 1])

    KeyPageUp ->
      (state { tsTranscriptScroll = max 0 (tsTranscriptScroll - 5) }, [ActionScrollTranscript (-5)])

    KeyPageDown ->
      (state { tsTranscriptScroll = tsTranscriptScroll + 5 }, [ActionScrollTranscript 5])

    KeyEnter ->
      (toggleToolExpanded tsSelectedToolIndex state, [])

    KeyChar ' ' ->
      (toggleToolExpanded tsSelectedToolIndex state, [])

    KeyChar 'c' ->
      -- Clear transcript and reset context window tokens
      (state { tsTranscript = [], tsTranscriptScroll = 0, tsSelectedToolIndex = 0, tsContextTokens = 0, tsTokenUsage = Nothing, tsUsageStatus = UsageVerified }, [])

    _ ->
      (state, [])

-- | Toggle the expanded state of a tool card.
toggleToolExpanded :: Int -> TuiState -> TuiState
toggleToolExpanded targetIdx state@TuiState{..} =
  let updateCard (i, acc) item = case item of
        TiToolCard tc
          | i == targetIdx -> (i + 1, TiToolCard tc { tcExpanded = not (tcExpanded tc) } : acc)
          | otherwise      -> (i + 1, item : acc)
        other -> (i, other : acc)
      (_, newTranscriptRev) = foldl updateCard (0, []) tsTranscript
  in state { tsTranscript = reverse newTranscriptRev }

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
        newTranscript = tsTranscript ++ textItems ++ cardItems
        newStatus = if null calls then StatusFinished else tsStatus
        (newContextTokens, newUsage, newSessionTokens, newUsageStatus) = case mUsage of
          Just u  -> (tuTotalTokens u, Just u, addUsageToSession u False tsSessionTokens, UsageVerified)
          Nothing -> (tsContextTokens, tsTokenUsage, tsSessionTokens, UsageMissing)
    in state
         { tsTranscript    = newTranscript
         , tsStatus        = newStatus
         , tsContextTokens = newContextTokens
         , tsTokenUsage    = newUsage
         , tsSessionTokens = newSessionTokens
         , tsUsageStatus   = newUsageStatus
         }

  EvToolCall name args ->
    let (newTranscript, mIdx) = updateFirstPendingTool name args tsTranscript
        newSelected = case mIdx of
          Just idx -> idx
          Nothing  -> tsSelectedToolIndex
    in state
      { tsTranscript        = newTranscript
      , tsStatus            = StatusRunningTool name
      , tsSelectedToolIndex = newSelected
      }

  EvToolResult _name res ->
    let newTranscript = updateFirstRunningToFinished res tsTranscript
    in state { tsTranscript = newTranscript, tsStatus = StatusThinking }

  EvDone ans ->
    let finalTranscript =
          if not (null tsTranscript) && last tsTranscript == TiAssistant ans
            then tsTranscript
            else tsTranscript ++ [TiAssistant ans]
    in state { tsTranscript = finalTranscript, tsStatus = StatusFinished, tsFocus = FocusInput }

  EvError err ->
    state
      { tsTranscript = tsTranscript ++ [TiNotice ("Error: " <> err)]
      , tsStatus     = StatusError err
      , tsFocus      = FocusInput
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
        , tsTranscript = tsTranscript ++ [TiNotice ("Goal evaluated: " <> verdictText verdict <> " — " <> reason)]
        }
      Nothing -> state

  EvGoalEvaluationUsage u ->
    state { tsSessionTokens = addUsageToSession u True tsSessionTokens }

  EvGoalAchieved cond ->
    case tsGoalState of
      Just gs -> state
        { tsGoalState = Just gs { gsStatus = GoalAchieved }
        , tsTranscript = tsTranscript ++ [TiNotice ("Goal achieved: " <> cond)]
        , tsFocus = FocusInput
        }
      Nothing -> state

  EvGoalFailed cond reason ->
    case tsGoalState of
      Just gs -> state
        { tsGoalState = Just gs { gsStatus = GoalFailed }
        , tsTranscript = tsTranscript ++ [TiNotice ("Goal failed: " <> cond <> " — " <> reason)]
        , tsFocus = FocusInput
        }
      Nothing -> state

  EvGoalCleared _ ->
    state { tsGoalState = Nothing }

  EvGoalBlocked cond ->
    case tsGoalState of
      Just _ -> state
        { tsTranscript = tsTranscript ++
          [ TiNotice ("No progress detected. Goal still active: " <> cond)
          , TiNotice "Run /goal again to continue after your next prompt."
          ]
        , tsFocus = FocusInput
        }
      Nothing -> state

  EvPartialResponse _ -> state
  EvToolCallDelta _ -> state
  EvPermissionDenied tool reason ->
    let updatedTranscript = updateFirstMatchingToDenied tool reason tsTranscript
        finalTranscript   = updatedTranscript ++ [TiNotice ("Permission denied for " <> tool <> ": " <> reason)]
    in state { tsTranscript = finalTranscript }
  EvHookTriggered hook res ->
    state { tsTranscript = tsTranscript ++ [TiNotice ("Hook triggered: " <> hook <> " -> " <> res)] }
  EvSessionSaved path ->
    state { tsTranscript = tsTranscript ++ [DiNotice ("Session saved to " <> path)] }
  EvNotificationSent msg ->
    state { tsTranscript = tsTranscript ++ [DiNotice ("Notification: " <> msg)] }

-- | Transition the first 'Pending' card to 'Running', replacing its arguments,
-- and return the card's 0-based index among tool cards in the transcript.
updateFirstPendingTool :: T.Text -> T.Text -> [TranscriptItem] -> ([TranscriptItem], Maybe Int)
updateFirstPendingTool name args = go 0
  where
    go _ [] = ([], Nothing)
    go cardIdx (TiToolCard tc : rest)
      | tcLifecycle tc == Pending =
          let updated = TiToolCard tc { tcName = name, tcArgs = args, tcLifecycle = Running }
          in (updated : rest, Just cardIdx)
      | otherwise =
          let (rest', mIdx) = go (cardIdx + 1) rest
          in (TiToolCard tc : rest', mIdx)
    go cardIdx (x : rest) =
      let (rest', mIdx) = go cardIdx rest
      in (x : rest', mIdx)

-- | Transition the first 'Running' card to 'Finished' with the tool result.
updateFirstRunningToFinished :: ToolResult -> [TranscriptItem] -> [TranscriptItem]
updateFirstRunningToFinished res = go
  where
    go [] = []
    go (TiToolCard tc : rest)
      | tcLifecycle tc == Running =
          TiToolCard tc { tcLifecycle = Finished res } : rest
    go (x : rest) = x : go rest

-- | Transition the first 'Pending' or 'Running' card matching the tool name to 'Denied'.
updateFirstMatchingToDenied :: T.Text -> T.Text -> [TranscriptItem] -> [TranscriptItem]
updateFirstMatchingToDenied tool reason = go
  where
    go [] = []
    go (TiToolCard tc : rest)
      | (tcLifecycle tc == Pending || tcLifecycle tc == Running) && tcName tc == tool =
          TiToolCard tc { tcLifecycle = Denied reason } : rest
    go (x : rest) = x : go rest

-- | Mark every unresolved (Pending or Running) tool card as Cancelled.
cancelUnresolvedToolCards :: [TranscriptItem] -> [TranscriptItem]
cancelUnresolvedToolCards = map $ \case
  TiToolCard tc
    | tcLifecycle tc `elem` [Pending, Running] ->
        TiToolCard tc { tcLifecycle = Cancelled }
  item -> item

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
-- Auto-scroll policy: when focus is on the prompt input, every transcript-appending
-- Harness event scrolls the transcript viewport to the end.
-- When focus is on the transcript, incoming events leave the viewport alone.
shouldAutoScroll :: TuiState -> AgentEvent -> Bool
shouldAutoScroll state ev =
  tsFocus state == FocusInput && isTranscriptAppendingEvent ev
