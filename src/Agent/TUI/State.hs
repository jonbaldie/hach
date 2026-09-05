{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.TUI.State
  ( updateTui
  , handleUserKey
  , handleAgentEvent
  , toggleToolExpanded
  ) where

import Agent.TUI.Types
import Agent.Types (AgentEvent(..), ToolResult)
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

-- | Handle submitting a user task prompt.
handleSubmitPrompt :: T.Text -> TuiState -> (TuiState, [TuiAction])
handleSubmitPrompt rawPrompt state
  | T.null (T.strip rawPrompt) = (state, [])
  | otherwise =
      let trimmed = T.strip rawPrompt
          newHistory = tsHistory state ++ [DiUser trimmed]
          newState = state
            { tsHistory     = newHistory
            , tsInputBuffer = ""
            , tsStatus      = StatusThinking
            , tsFocus       = FocusHistory
            }
      in (newState, [ActionRunAgent trimmed])

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

    KeyChar '?'
      | tsFocus /= FocusInput ->
          (state { tsShowHelp = not tsShowHelp }, [])

    KeyTab ->
      (state { tsFocus = nextFocus tsFocus }, [])

    KeyBackTab ->
      (state { tsFocus = prevFocus tsFocus }, [])

    -- 2. Focus-specific actions
    _ -> case tsFocus of
      FocusInput ->
        handleInputKey key state

      FocusHistory ->
        handleHistoryKey key state

      FocusTools ->
        handleToolsKey key state
  where
    isBusy = \case
      StatusThinking      -> True
      StatusRunningTool _ -> True
      _                   -> False

-- | Key handling inside the input text area.
handleInputKey :: UserKey -> TuiState -> (TuiState, [TuiAction])
handleInputKey key state@TuiState{..} = case key of
  KeyEnter ->
    handleSubmitPrompt tsInputBuffer state

  KeyChar c ->
    (state { tsInputBuffer = tsInputBuffer `T.snoc` c }, [])

  KeyBackspace ->
    (state { tsInputBuffer = if T.null tsInputBuffer then "" else T.init tsInputBuffer }, [])

  KeyCtrl 'u' ->
    (state { tsInputBuffer = "" }, [])

  _ ->
    (state, [])

-- | Key handling when the conversation history panel is focused.
handleHistoryKey :: UserKey -> TuiState -> (TuiState, [TuiAction])
handleHistoryKey key state@TuiState{..} = case key of
  KeyUp ->
    (state { tsHistoryScroll = max 0 (tsHistoryScroll - 1) }, [])

  KeyDown ->
    (state { tsHistoryScroll = tsHistoryScroll + 1 }, [])

  KeyPageUp ->
    (state { tsHistoryScroll = max 0 (tsHistoryScroll - 5) }, [])

  KeyPageDown ->
    (state { tsHistoryScroll = tsHistoryScroll + 5 }, [])

  KeyChar 'c' ->
    -- Clear dialogue history
    (state { tsHistory = [], tsHistoryScroll = 0 }, [])

  _ ->
    (state, [])

-- | Key handling when the tool activity panel is focused.
handleToolsKey :: UserKey -> TuiState -> (TuiState, [TuiAction])
handleToolsKey key state@TuiState{..} =
  let totalTools = length tsTools
  in case key of
    KeyUp ->
      (state { tsSelectedToolIndex = max 0 (tsSelectedToolIndex - 1) }, [])

    KeyDown ->
      (state { tsSelectedToolIndex = min (max 0 (totalTools - 1)) (tsSelectedToolIndex + 1) }, [])

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
handleAgentEvent :: AgentEvent -> TuiState -> TuiState
handleAgentEvent event state@TuiState{..} = case event of
  EvTurnStart n ->
    state { tsCurrentTurn = n, tsStatus = StatusThinking }

  EvPromptingLLM _ ->
    state { tsStatus = StatusThinking }

  EvLLMResponse mContent calls ->
    let withText = case mContent of
          Just c | not (T.null (T.strip c)) ->
            tsHistory ++ [DiAssistant c]
          _ -> tsHistory
        newStatus = if null calls then StatusFinished else tsStatus
    in state { tsHistory = withText, tsStatus = newStatus }

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
    in state { tsHistory = finalHistory, tsStatus = StatusFinished }

  EvError err ->
    state
      { tsHistory = tsHistory ++ [DiNotice ("Error: " <> err)]
      , tsStatus  = StatusError err
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
