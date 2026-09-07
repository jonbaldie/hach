{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.TUI.App
  ( runTui
  , vtyToUserKey
  , dialogueToMessages
  , transcriptToMessages
  , transcriptItemsToMessages
  , cancelledToolCallPlaceholder
  , shouldAutoScroll
  , isTranscriptAppendingEvent
  , runGoalWorker
  , goalAgentConfig
  , initialTuiLaunch
  ) where

import Hach.Core
import Hach.Env (buildSystemPrompt, loadProjectInstructions)
import Hach.Interpreter.IO
import Hach.Skills (discoverSkills)
import Hach.Tools
import Hach.TUI.State
import Hach.TUI.Types
import Hach.TUI.UI
import Hach.Types
import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.CrossPlatform as VtyCross

-- | Convert Vty events to abstracted 'UserKey's.
vtyToUserKey :: Vty.Event -> Maybe UserKey
vtyToUserKey = \case
  Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl] -> Just (KeyCtrl 'q')
  Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Just (KeyCtrl 'c')
  Vty.EvKey (Vty.KChar 'u') [Vty.MCtrl] -> Just (KeyCtrl 'u')
  Vty.EvKey (Vty.KChar '\t') []        -> Just KeyTab
  Vty.EvKey (Vty.KChar '\t') [Vty.MShift] -> Just KeyBackTab
  Vty.EvKey Vty.KBackTab _             -> Just KeyBackTab
  Vty.EvKey Vty.KEnter []              -> Just KeyEnter
  Vty.EvKey Vty.KBS []                 -> Just KeyBackspace
  Vty.EvKey Vty.KDel []                -> Just KeyDelete
  Vty.EvKey Vty.KEsc []                -> Just KeyEsc
  Vty.EvKey Vty.KUp []                 -> Just KeyUp
  Vty.EvKey Vty.KDown []               -> Just KeyDown
  Vty.EvKey Vty.KPageUp _              -> Just KeyPageUp
  Vty.EvKey Vty.KPageDown _            -> Just KeyPageDown
  Vty.EvMouseDown _ _ Vty.BScrollUp _  -> Just KeyScrollUp
  Vty.EvMouseDown _ _ Vty.BScrollDown _-> Just KeyScrollDown
  Vty.EvKey (Vty.KFun 1) []            -> Just KeyF1
  Vty.EvKey (Vty.KChar c) []           -> Just (KeyChar c)
  _                                    -> Nothing

-- | Algebra that pipes every agent execution event into the Brick BChan.
tuiAlgebra :: BChan AgentEvent -> IOEnv -> AgentAlgebra IO
tuiAlgebra chan env = ioAlgebraWithLog (writeBChan chan) env

-- | Compute starting state and initial actions from an optional CLI prompt.
initialTuiLaunch :: Maybe Text -> TuiState -> (TuiState, [TuiAction])
initialTuiLaunch Nothing st = (st, [])
initialTuiLaunch (Just p) st
  | T.null (T.strip p) = (st, [])
  | otherwise          = updateTui (EvSubmit p) st

-- | Run the full modern TUI application.
runTui :: IOEnv -> Maybe Text -> Maybe Int -> IO ()
runTui ioEnv initialPrompt mMaxTurns = do
  eventChan <- newBChan 100
  workerVar <- newTVarIO (Nothing :: Maybe (Async ()))

  skills <- discoverSkills (ioWorkspace ioEnv)
  mGuidelines <- loadProjectInstructions (ioWorkspace ioEnv)
  let sysPrompt = buildSystemPrompt mGuidelines

  let baseState = (initialTuiState (ioModel ioEnv) mMaxTurns) { tsSkills = skills }
      (startingState, initialActions) = initialTuiLaunch initialPrompt baseState

  let app :: App TuiState AgentEvent Name
      app = App
        { appDraw         = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent  = handleBrickEvent eventChan workerVar ioEnv sysPrompt
        , appStartEvent   = do
            -- Dispatch actions produced by any initial prompt provided on CLI
            currentState <- get
            forM_ initialActions $ \case
              ActionQuit -> halt
              ActionCancelAgent -> pure ()
              ActionRunAgent prompt -> do
                triggerAgentRun eventChan workerVar ioEnv sysPrompt (tsMaxTurns currentState) prompt (tsHistory currentState)
                vScrollToEnd (viewportScroll VpTranscript)
              ActionRunGoal condition -> do
                triggerGoalRun eventChan workerVar ioEnv sysPrompt (tsMaxTurns currentState) condition (tsHistory currentState)
                vScrollToEnd (viewportScroll VpTranscript)
              ActionScrollTranscript delta ->
                vScrollBy (viewportScroll VpTranscript) delta
        , appAttrMap      = const tuiAttrMap
        }

  -- Vty's built-in width table under-measures the UI's double-width glyphs,
  -- which shifts everything drawn after them and breaks border alignment.
  installWideGlyphWidths

  let buildVty = do
        vty <- VtyCross.mkVty Vty.defaultConfig
        let output = Vty.outputIface vty
        Vty.setMode output Vty.Mouse True
        pure vty
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just eventChan) app startingState

  -- Cleanup any background worker on exit
  mWorker <- atomically $ readTVar workerVar
  mapM_ cancel mWorker

-- | Collapse a run of same-role text items with one intercalate so the
-- copy is linear in the total text rather than quadratic in the run length.
collapseAdjacent
  :: (a -> Maybe Text)
  -> (Text -> a)
  -> [a]
  -> [a]
collapseAdjacent view wrap = go
  where
    go [] = []
    go (x : xs) = case view x of
      Just t ->
        let (more, rest) = spanView [] xs
        in wrap (T.intercalate "\n\n" (t : more)) : go rest
      Nothing ->
        x : go xs

    spanView acc [] = (reverse acc, [])
    spanView acc (y : ys) = case view y of
      Just t  -> spanView (t : acc) ys
      Nothing -> (reverse acc, y : ys)

-- | Convert dialogue history into LLM messages for multi-turn context.
-- Uses the expanded prompt for the latest turn so that skill instructions
-- reach the model while preserving clean display history in the UI.
dialogueToMessages :: Text -> Text -> [DialogueItem] -> [Message]
dialogueToMessages sysPrompt currentPrompt items =
  let priorItems = dropLastUser items
      priorMsgs  = transcriptItemsToMessages priorItems
      -- A prior user turn with no assistant reply plus the new prompt would
      -- otherwise emit two adjoining UserMsg values, which OpenRouter rejects.
      msgs = SystemMsg sysPrompt : priorMsgs ++ [UserMsg currentPrompt]
  in collapseAdjacent (\case UserMsg t -> Just t; _ -> Nothing) UserMsg msgs
  where
    dropLastUser [] = []
    dropLastUser xs =
      let rev = reverse xs
          (notices, rest) = span (\case DiNotice _ -> True; _ -> False) rev
      in case rest of
           (DiUser _ : prior) -> reverse (notices ++ prior)
           _                  -> xs

-- | Synonym for 'dialogueToMessages' using unified transcript terminology.
transcriptToMessages :: Text -> Text -> [TranscriptItem] -> [Message]
transcriptToMessages = dialogueToMessages

-- | Convert a chronological list of transcript items into LLM messages.
-- A run of consecutive tool cards following an assistant text item (or standing alone)
-- becomes one assistant message carrying the text plus those tool calls,
-- followed by one tool message per card keyed by call id.
-- Notices remain omitted. Consecutive assistant or user text items
-- (including those that become adjacent after notices are dropped) are
-- collapsed so the resulting message list never contains two adjoining
-- 'AssistantMsg' or 'UserMsg' values.
transcriptItemsToMessages :: [TranscriptItem] -> [Message]
transcriptItemsToMessages = go . collapseAdjacentTextItems . filter (not . isNotice)
  where
    isNotice (TiNotice _) = True
    isNotice _            = False

    collapseAdjacentTextItems =
      collapseAdjacent (\case TiAssistant t -> Just t; _ -> Nothing) TiAssistant
      . collapseAdjacent (\case TiUser t -> Just t; _ -> Nothing) TiUser

    extractCards (TiToolCard c : rest) =
      let (cs, remItems) = extractCards rest
      in (c : cs, remItems)
    extractCards remItems = ([], remItems)

    go [] = []
    go (TiAssistant text : rest) =
      let (cards, remaining) = extractCards rest
          mText = if T.null text then Nothing else Just text
      in if null cards
           then AssistantMsg mText [] : go remaining
           else
             let toolCalls = map cardToToolCall cards
                 toolMsgs  = map cardToToolMsg cards
             in AssistantMsg mText toolCalls : toolMsgs ++ go remaining
    go (TiToolCard card : rest) =
      let (cards, remaining) = extractCards rest
          allCards = card : cards
          toolCalls = map cardToToolCall allCards
          toolMsgs  = map cardToToolMsg allCards
      in AssistantMsg Nothing toolCalls : toolMsgs ++ go remaining
    go (TiUser u : rest) =
      UserMsg u : go rest
    go (TiSystem s : rest) =
      SystemMsg s : go rest
    go (TiNotice _ : rest) =
      go rest

-- | Convert a 'ToolCard' into a provider-facing 'ToolCall'.
cardToToolCall :: ToolCard -> ToolCall
cardToToolCall tc = ToolCall
  { callId       = tcId tc
  , functionName = tcName tc
  , callArgsRaw  = tcArgs tc
  }

-- | Convert a 'ToolCard' into a provider-facing 'ToolMsg'.
cardToToolMsg :: ToolCard -> Message
cardToToolMsg tc =
  ToolMsg (tcId tc) (tcName tc) (toolCardContent (tcLifecycle tc))

-- | Placeholder text for unresolved tool calls (Pending, Running, Cancelled).
cancelledToolCallPlaceholder :: Text
cancelledToolCallPlaceholder = "Tool call was cancelled before completion."

-- | Extract tool message content based on card lifecycle.
toolCardContent :: ToolLifecycle -> Text
toolCardContent = \case
  Finished res  -> toolResultToText res
  Denied reason -> reason
  Pending       -> cancelledToolCallPlaceholder
  Running       -> cancelledToolCallPlaceholder
  Cancelled     -> cancelledToolCallPlaceholder

-- | Trigger background agent task execution.
triggerAgentRun
  :: BChan AgentEvent
  -> TVar (Maybe (Async ()))
  -> IOEnv
  -> Text
  -> Maybe Int
  -> Text
  -> [DialogueItem]
  -> EventM Name TuiState ()
triggerAgentRun eventChan workerVar ioEnv sysPrompt mMaxTurns currentPrompt historyItems = liftIO $ do
  -- Cancel existing worker if any
  mOldWorker <- atomically $ do
    w <- readTVar workerVar
    writeTVar workerVar Nothing
    pure w
  mapM_ cancel mOldWorker

  newWorker <- async $ do
    let agentConfig = AgentConfig
          { cfgModel        = ioModel ioEnv
          , cfgSystemPrompt = Just sysPrompt
          , cfgMaxTurns     = mMaxTurns
          }
        initHistory = dialogueToMessages sysPrompt currentPrompt historyItems

    res <- try (foldAgentProgram (tuiAlgebra eventChan ioEnv) (agentLoop agentConfig allToolDefs initHistory))
    case res of
      Left (ex :: SomeException) ->
        writeBChan eventChan (EvError (T.pack (show ex)))
      Right (AgentCompleted ans, _) ->
        writeBChan eventChan (EvDone ans)
      Right (AgentMaxTurnsReached n, _) ->
        writeBChan eventChan (EvError ("Maximum turns reached (" <> T.pack (show n) <> ")"))
      Right (AgentFailed err, _) ->
        writeBChan eventChan (EvError err)

  atomically $ writeTVar workerVar (Just newWorker)

-- | Construct the 'AgentConfig' for a goal-directed run in the TUI.
goalAgentConfig :: IOEnv -> Text -> Maybe Int -> AgentConfig
goalAgentConfig ioEnv sysPrompt mMaxTurns = AgentConfig
  { cfgModel        = ioModel ioEnv
  , cfgSystemPrompt = Just sysPrompt
  , cfgMaxTurns     = mMaxTurns
  }

-- | Execute a goal-directed agent run using the given algebra and emit events.
runGoalWorker
  :: AgentAlgebra IO
  -> AgentConfig
  -> Text
  -> [DialogueItem]
  -> (AgentEvent -> IO ())
  -> IO ()
runGoalWorker algebra agentConfig condition historyItems emitEvent = do
  let sysPrompt = fromMaybe "" (cfgSystemPrompt agentConfig)
      initHistory = dialogueToMessages sysPrompt condition historyItems
  res <- try (foldAgentProgram algebra
              (goalLoop agentConfig allToolDefs condition defaultBlockCap initHistory))
  case res of
    Left (ex :: SomeException) ->
      emitEvent (EvError (T.pack (show ex)))
    Right (AgentCompleted ans, _, gs) ->
      case gsStatus gs of
        GoalFailed -> emitEvent (EvError ans)
        GoalActive -> emitEvent (EvError ans)
        _          -> emitEvent (EvDone ans)
    Right (AgentMaxTurnsReached n, _, _) ->
      emitEvent (EvError ("Maximum turns reached (" <> T.pack (show n) <> ")"))
    Right (AgentFailed err, _, _) ->
      emitEvent (EvError err)

-- | Trigger background goal-directed agent execution.
triggerGoalRun
  :: BChan AgentEvent
  -> TVar (Maybe (Async ()))
  -> IOEnv
  -> Text
  -> Maybe Int
  -> Text          -- ^ goal condition (also used as the first-turn directive)
  -> [DialogueItem]
  -> EventM Name TuiState ()
triggerGoalRun eventChan workerVar ioEnv sysPrompt mMaxTurns condition historyItems = liftIO $ do
  mOldWorker <- atomically $ do
    w <- readTVar workerVar
    writeTVar workerVar Nothing
    pure w
  mapM_ cancel mOldWorker

  newWorker <- async $ do
    let agentConfig = goalAgentConfig ioEnv sysPrompt mMaxTurns
    runGoalWorker (tuiAlgebra eventChan ioEnv) agentConfig condition historyItems (writeBChan eventChan)

  atomically $ writeTVar workerVar (Just newWorker)

-- | Handle Brick UI events.
handleBrickEvent
  :: BChan AgentEvent
  -> TVar (Maybe (Async ()))
  -> IOEnv
  -> Text
  -> BrickEvent Name AgentEvent
  -> EventM Name TuiState ()
handleBrickEvent eventChan workerVar ioEnv sysPrompt = \case
  AppEvent agentEv -> do
    currentState <- get
    modify (handleAgentEvent agentEv)
    when (shouldAutoScroll currentState agentEv) $
      vScrollToEnd (viewportScroll VpTranscript)

  VtyEvent vtyEv -> do
    case vtyToUserKey vtyEv of
      Just key -> do
        currentState <- get
        let (nextState, actions) = updateTui (EvUserKey key) currentState
        put nextState
        forM_ actions $ \case
          ActionQuit ->
            halt
          ActionCancelAgent -> liftIO $ do
            mWorker <- atomically $ do
              w <- readTVar workerVar
              writeTVar workerVar Nothing
              pure w
            mapM_ cancel mWorker
          ActionRunAgent prompt -> do
            triggerAgentRun eventChan workerVar ioEnv sysPrompt (tsMaxTurns nextState) prompt (tsHistory nextState)
            vScrollToEnd (viewportScroll VpTranscript)
          ActionRunGoal condition -> do
            triggerGoalRun eventChan workerVar ioEnv sysPrompt (tsMaxTurns nextState) condition (tsHistory nextState)
            vScrollToEnd (viewportScroll VpTranscript)
          ActionScrollTranscript delta ->
            vScrollBy (viewportScroll VpTranscript) delta
      Nothing ->
        pure ()

  _ ->
    pure ()
