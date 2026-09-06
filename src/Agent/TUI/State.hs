{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.TUI.State
  ( updateTui
  , handleUserKey
  , handleAgentEvent
  , toggleToolExpanded
  ) where

import Agent.Skills (Skill(..), injectSkillsIntoPrompt, parseSkillInvocations, skillInvocationCompletion)
import Agent.TUI.Types
import Agent.TUI.UI (formatTokens)
import Agent.Types (AgentEvent(..), TokenUsage(..), ToolResult)
import qualified Data.Text as T

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

-- | Handle submitting a user task prompt.
handleSubmitPrompt :: T.Text -> TuiState -> (TuiState, [TuiAction])
handleSubmitPrompt rawPrompt state
  | T.null trimmed = (state, [])
  | trimmed == "/clear" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          busy = isBusy (tsStatus state)
          actions = if busy then [ActionCancelAgent] else []
          newStatus = if busy then StatusIdle else tsStatus state
      in ( state { tsHistory            = []
                 , tsHistoryScroll      = 0
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 , tsStatus             = newStatus
                 , tsCancelRequested    = if busy then True else False
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
  | trimmed == "/cost" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          costNotice = case tsTokenUsage state of
            Just TokenUsage{..} ->
              "Tokens: " <> formatTokens (tsContextTokens state) <> " in context window (" <>
              formatTokens tuPromptTokens <> " prompt, " <>
              formatTokens tuCompletionTokens <> " completion)"
            Nothing ->
              "Tokens: " <> formatTokens (tsContextTokens state) <> " in context window"
          newHistory = tsHistory state ++ [DiNotice costNotice]
      in ( state { tsHistory            = newHistory
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | trimmed == "/compact" =
      let newPromptHistory = tsPromptHistory state ++ [trimmed]
          newHistory = if length (tsHistory state) > 4
            then DiNotice "Prior conversation turns compacted for context efficiency." : drop (length (tsHistory state) - 4) (tsHistory state)
            else tsHistory state ++ [DiNotice "Conversation history compacted."]
      in ( state { tsHistory            = newHistory
                 , tsInputBuffer        = ""
                 , tsPromptHistory      = newPromptHistory
                 , tsPromptHistoryIndex = Nothing
                 , tsPromptDraft        = ""
                 }
         , []
         )
  | otherwise =
      let (cleanedPrompt, invokedSkills) = parseSkillInvocations (tsSkills state) trimmed
          finalPrompt = injectSkillsIntoPrompt invokedSkills cleanedPrompt
          skillNotices = [ DiNotice ("Activated skill: " <> skillName s) | s <- invokedSkills ]
          newHistory = tsHistory state ++ [DiUser trimmed] ++ skillNotices
          newPromptHistory = tsPromptHistory state ++ [trimmed]
          newState = state
            { tsHistory            = newHistory
            , tsInputBuffer        = ""
            , tsStatus             = StatusThinking
            , tsFocus              = FocusHistory
            , tsPromptHistory      = newPromptHistory
            , tsPromptHistoryIndex = Nothing
            , tsPromptDraft        = ""
            , tsCancelRequested    = False
            }
      in (newState, [ActionRunAgent finalPrompt])
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
                  }
          , [ActionCancelAgent]
          )
      | otherwise ->
          (state { tsShouldQuit = True }, [ActionQuit])

    KeyEsc
      | isBusy tsStatus ->
          ( state { tsCancelRequested = True
                  , tsStatus = StatusError "Turn cancelled by user."
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
      -- Accept the inline skill-completion ghost text when the user is
      -- typing a slash-command prefix in the input box; otherwise cycle
      -- panel focus as usual.
      case skillInvocationCompletion tsSkills tsInputBuffer of
        Just suffix | tsFocus == FocusInput ->
          (state { tsInputBuffer = tsInputBuffer `T.append` suffix }, [])
        _ ->
          (state { tsFocus = nextFocus tsFocus }, [])

    KeyBackTab ->
      (state { tsFocus = prevFocus tsFocus }, [])

    KeyScrollUp -> case tsFocus of
      FocusTools -> (state, [ActionScrollTools (-2)])
      _          -> (state { tsHistoryScroll = max 0 (tsHistoryScroll - 2) }, [ActionScrollHistory (-2)])

    KeyScrollDown -> case tsFocus of
      FocusTools -> (state, [ActionScrollTools 2])
      _          -> (state { tsHistoryScroll = tsHistoryScroll + 2 }, [ActionScrollHistory 2])

    -- 2. Focus-specific actions
    _ -> case tsFocus of
      FocusInput ->
        handleInputKey key state

      FocusHistory ->
        handleHistoryKey key state

      FocusTools ->
        handleToolsKey key state

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
    (state { tsInputBuffer = tsInputBuffer `T.snoc` c }, [])

  KeyBackspace ->
    (state { tsInputBuffer = if T.null tsInputBuffer then "" else T.init tsInputBuffer }, [])

  KeyDelete ->
    (state { tsInputBuffer = if T.null tsInputBuffer then "" else T.init tsInputBuffer }, [])

  KeyCtrl 'u' ->
    (state { tsInputBuffer = "" }, [])

  KeyPageUp ->
    (state { tsHistoryScroll = max 0 (tsHistoryScroll - 5) }, [ActionScrollHistory (-5)])

  KeyPageDown ->
    (state { tsHistoryScroll = tsHistoryScroll + 5 }, [ActionScrollHistory 5])

  _ ->
    (state, [])

-- | Key handling when the conversation history panel is focused.
handleHistoryKey :: UserKey -> TuiState -> (TuiState, [TuiAction])
handleHistoryKey key state@TuiState{..} = case key of
  KeyUp ->
    (state { tsHistoryScroll = max 0 (tsHistoryScroll - 1) }, [ActionScrollHistory (-1)])

  KeyDown ->
    (state { tsHistoryScroll = tsHistoryScroll + 1 }, [ActionScrollHistory 1])

  KeyPageUp ->
    (state { tsHistoryScroll = max 0 (tsHistoryScroll - 5) }, [ActionScrollHistory (-5)])

  KeyPageDown ->
    (state { tsHistoryScroll = tsHistoryScroll + 5 }, [ActionScrollHistory 5])

  KeyChar 'c' ->
    -- Clear dialogue history and reset context window tokens
    (state { tsHistory = [], tsHistoryScroll = 0, tsContextTokens = 0, tsTokenUsage = Nothing }, [])

  _ ->
    (state, [])

-- | Key handling when the tool activity panel is focused.
handleToolsKey :: UserKey -> TuiState -> (TuiState, [TuiAction])
handleToolsKey key state@TuiState{..} =
  let totalTools = length tsTools
  in case key of
    KeyUp ->
      let newIdx = max 0 (tsSelectedToolIndex - 1)
      in (state { tsSelectedToolIndex = newIdx }, [ActionScrollTools (-1)])

    KeyDown ->
      let newIdx = min (max 0 (totalTools - 1)) (tsSelectedToolIndex + 1)
      in (state { tsSelectedToolIndex = newIdx }, [ActionScrollTools 1])

    KeyPageUp ->
      let newIdx = max 0 (tsSelectedToolIndex - 5)
      in (state { tsSelectedToolIndex = newIdx }, [ActionScrollTools (-5)])

    KeyPageDown ->
      let newIdx = min (max 0 (totalTools - 1)) (tsSelectedToolIndex + 5)
      in (state { tsSelectedToolIndex = newIdx }, [ActionScrollTools 5])

    KeyEnter ->
      (toggleToolExpanded tsSelectedToolIndex state, [])

    KeyChar ' ' ->
      (toggleToolExpanded tsSelectedToolIndex state, [])

    _ ->
      (state, [])

-- | Toggle the expanded state of a tool card.
toggleToolExpanded :: Int -> TuiState -> TuiState
toggleToolExpanded idx state@TuiState{..} =
  let updatedTools = zipWith (\i item ->
        if i == idx then item { tiExpanded = not (tiExpanded item) } else item) [0..] tsTools
  in state { tsTools = updatedTools }

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
    let withText = case mContent of
          Just c | not (T.null (T.strip c)) ->
            tsHistory ++ [DiAssistant c]
          _ -> tsHistory
        newStatus = if null calls then StatusFinished else tsStatus
        (newContextTokens, newUsage) = case mUsage of
          Just u  -> (tuTotalTokens u, Just u)
          Nothing -> (tsContextTokens, tsTokenUsage)
    in state
         { tsHistory       = withText
         , tsStatus        = newStatus
         , tsContextTokens = newContextTokens
         , tsTokenUsage    = newUsage
         }

  EvToolCall name args ->
    let newItem = ToolItem
          { tiName     = name
          , tiArgs     = args
          , tiResult   = Nothing
          , tiExpanded = False
          }
        newTools = tsTools ++ [newItem]
    in state
      { tsTools             = newTools
      , tsStatus            = StatusRunningTool name
      , tsSelectedToolIndex = max 0 (length newTools - 1)
      }

  EvToolResult _name res ->
    let updatedTools = updateLatestToolResult res tsTools
    in state { tsTools = updatedTools, tsStatus = StatusThinking }

  EvDone ans ->
    let finalHistory =
          if not (null tsHistory) && last tsHistory == DiAssistant ans
            then tsHistory
            else tsHistory ++ [DiAssistant ans]
    in state { tsHistory = finalHistory, tsStatus = StatusFinished, tsFocus = FocusInput }

  EvError err ->
    state
      { tsHistory = tsHistory ++ [DiNotice ("Error: " <> err)]
      , tsStatus  = StatusError err
      , tsFocus   = FocusInput
      }

  EvTurnComplete _ ->
    state

-- | Attach tool execution output to the most recent unfinished tool item.
updateLatestToolResult :: ToolResult -> [ToolItem] -> [ToolItem]
updateLatestToolResult res items =
  case reverse items of
    (latest : rest) | tiResult latest == Nothing ->
      reverse (latest { tiResult = Just res } : rest)
    _ -> items
