{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.TUI.App
  ( runTui
  , buildTuiSystemPrompt
  , vtyToUserKey
  , brickToUserKey
  , dialogueToMessages
  , transcriptToMessages
  , transcriptItemsToMessages
  , messagesToTranscriptItems
  , cancelledToolCallPlaceholder
  , runAgentWorker
  , shouldAutoScroll
  , isTranscriptAppendingEvent
  , runGoalWorker
  , runGoalWorkerWithHistory
  , goalAgentConfig
  , runEnvForModel
  , initialTuiLaunch
  , PermissionGate
  , newPermissionGate
  , resolveAskWithGate
  , respondPermission
  , cancelPermissionAsk
  , awaitPermissionAsk
  ) where

import Hach.Clipboard (copyToClipboard)
import Hach.Core
import Hach.CLI (renderRunOutcome)
import Hach.Env (buildSystemPromptWithAppend)
import Hach.Git (getGitDiff)
import Hach.Inference (connectionCostPolicy)
import Hach.Interpreter.IO
import Hach.Memory (loadProjectInstructions, loadProjectInstructionsFile)
import Hach.Skills (discoverSkills, expandSlashInvokedPrompt)
import Hach.Sessions (buildSessionHistory, saveRunSession)
import Hach.TUI.Conversation
  ( cancelledToolCallPlaceholder
  , messagesToTranscriptItems
  , transcriptItemsToMessages
  )
import Hach.Tools
import Hach.TUI.State
import Hach.TUI.Types
import Hach.TUI.UI
import Hach.Types
import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
  ( TMVar
  , TVar
  , atomically
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTVar
  , retry
  , takeTMVar
  , tryTakeTMVar
  , writeTVar
  )
import Control.Exception (SomeAsyncException(..), SomeException, fromException, tryJust)
import Control.Monad (forM_, void)
import System.Timeout (timeout)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, listToMaybe)
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

-- | Brick wraps wheel input over viewports in 'MouseDown', while input
-- outside those regions remains a raw 'VtyEvent'. Handle both forms.
brickToUserKey :: BrickEvent n e -> Maybe UserKey
brickToUserKey = \case
  VtyEvent event -> vtyToUserKey event
  MouseDown _ Vty.BScrollUp _ _ -> Just KeyScrollUp
  MouseDown _ Vty.BScrollDown _ _ -> Just KeyScrollDown
  _ -> Nothing

data PendingAsk = PendingAsk
  { paId     :: !Int
  , paTool   :: !Text
  , paArgs   :: !Text
  , paReason :: !Text
  }

data PermissionGate = PermissionGate
  { pgNextId  :: !(TVar Int)
  , pgPending :: !(TVar (Maybe PendingAsk))
  , pgReply   :: !(TMVar (Int, Bool))
  }

newPermissionGate :: IO PermissionGate
newPermissionGate = do
  nextId <- newTVarIO 1
  pending <- newTVarIO Nothing
  reply <- newEmptyTMVarIO
  pure (PermissionGate nextId pending reply)

resolveAskWithGate :: PermissionGate -> (AgentEvent -> IO ()) -> Text -> Text -> Text -> IO (Maybe Text)
resolveAskWithGate gate emit tool args reason = do
  askId <- atomically $ do
    i <- readTVar (pgNextId gate)
    writeTVar (pgNextId gate) (i + 1)
    writeTVar (pgPending gate) (Just (PendingAsk i tool args reason))
    void (tryTakeTMVar (pgReply gate))
    pure i
  emit (EvPermissionAsk askId tool args reason)
  (replyId, approved) <- atomically (takeTMVar (pgReply gate))
  atomically $ writeTVar (pgPending gate) Nothing
  pure $ if replyId == askId && approved
    then Nothing
    else Just interactiveAskDeniedReason

respondPermission :: PermissionGate -> Int -> Bool -> IO Bool
respondPermission gate expectedId approved = atomically $ do
  mAsk <- readTVar (pgPending gate)
  case mAsk of
    Just ask | paId ask == expectedId -> do
      writeTVar (pgPending gate) Nothing
      void (tryTakeTMVar (pgReply gate))
      putTMVar (pgReply gate) (paId ask, approved)
      pure True
    _ -> pure False

cancelPermissionAsk :: PermissionGate -> IO ()
cancelPermissionAsk gate = atomically $ do
  mAsk <- readTVar (pgPending gate)
  writeTVar (pgPending gate) Nothing
  case mAsk of
    Just ask -> do
      void (tryTakeTMVar (pgReply gate))
      putTMVar (pgReply gate) (paId ask, False)
    Nothing -> pure ()

trySync :: IO a -> IO (Either SomeException a)
trySync = tryJust $ \e ->
  case fromException e of
    Just (SomeAsyncException _) -> Nothing
    Nothing -> Just e

interruptWorker :: Maybe (Async ()) -> IO ()
interruptWorker = mapM_ $ \w -> void (async (cancel w))

awaitPermissionAsk :: PermissionGate -> Int -> IO (Maybe (Int, Text, Text, Text))
awaitPermissionAsk gate usec = do
  timeout usec $ atomically $ do
    mAsk <- readTVar (pgPending gate)
    case mAsk of
      Nothing  -> retry
      Just ask -> pure (paId ask, paTool ask, paArgs ask, paReason ask)

-- | Pipe live events to Brick. The worker sends terminal events after the
-- final canonical conversation has reached the reducer.
tuiAlgebra :: BChan TuiEvent -> IOEnv -> AgentAlgebra IO
tuiAlgebra chan env = ioAlgebraWithLog emit env
  where
    emit event = case event of
      EvDone{}  -> pure ()
      EvError{} -> pure ()
      _         -> writeBChan chan (EvHarness event)

-- | Compute starting state and initial actions from an optional CLI prompt.
initialTuiLaunch :: Maybe Text -> TuiState -> (TuiState, [TuiAction])
initialTuiLaunch Nothing st = (st, [])
initialTuiLaunch (Just p) st
  | T.null (T.strip p) = (st, [])
  | otherwise          = updateTui (EvSubmit p) st

-- | Execute one side-effecting action requested by the pure reducer.
runTuiAction
  :: BChan TuiEvent
  -> TVar (Maybe (Async ()))
  -> PermissionGate
  -> IOEnv
  -> Text
  -> Maybe Double
  -> TuiAction
  -> EventM Name TuiState ()
runTuiAction eventChan workerVar gate ioEnv sysPrompt mMaxBudgetUsd = \case
  ActionInitializeProject -> do
    result <- liftIO (initializeProjectWorkspace ioEnv)
    modify (applyProjectInitializationResult result)
  ActionShowDiff -> do
    diff <- liftIO (currentIOWorkspace ioEnv >>= getGitDiff)
    modify (applySlashCommandResult (DiffResult diff))
  ActionListTasks -> do
    tasks <- liftIO executeTaskList
    modify (applySlashCommandResult (TaskListResult (toolResultToText tasks)))
  ActionCopyToClipboard text -> do
    copied <- liftIO (copyToClipboard text)
    modify (applySlashCommandResult (ClipboardResult (T.length text <$ copied)))
  ActionQuit -> do
    liftIO $ do
      cancelPermissionAsk gate
      mWorker <- atomically $ do
        w <- readTVar workerVar
        writeTVar workerVar Nothing
        pure w
      mapM_ cancel mWorker
    halt
  ActionSetPermissionMode mode ->
    liftIO (setIOPermissionMode ioEnv mode)
  ActionRespondPermission askId approved ->
    liftIO (void (respondPermission gate askId approved))
  ActionCancelAgent -> liftIO $ do
    cancelPermissionAsk gate
    mWorker <- atomically $ do
      w <- readTVar workerVar
      writeTVar workerVar Nothing
      pure w
    interruptWorker mWorker
  ActionRunAgent prompt -> do
    currentState <- get
    triggerAgentRun eventChan workerVar gate ioEnv (tsModelName currentState) sysPrompt (tsMaxTurns currentState) mMaxBudgetUsd prompt (tsConversation currentState)
    vScrollToEnd (viewportScroll VpTranscript)
  ActionRunGoal condition -> do
    currentState <- get
    triggerGoalRun eventChan workerVar gate ioEnv (tsModelName currentState) sysPrompt (tsMaxTurns currentState) mMaxBudgetUsd condition (tsConversation currentState)
    vScrollToEnd (viewportScroll VpTranscript)
  ActionScrollTranscript delta ->
    vScrollBy (viewportScroll VpTranscript) delta
  ActionScrollPermission askId delta ->
    vScrollBy (viewportScroll (VpApproval askId)) delta

-- | Build the active system prompt for the TUI given workspace and optional custom appended prompt.
buildTuiSystemPrompt :: FilePath -> Maybe Text -> IO Text
buildTuiSystemPrompt workspace mAppendPrompt = do
  mGuidelines <- loadProjectInstructions workspace
  pure (buildSystemPromptWithAppend mGuidelines mAppendPrompt)

-- | Run the full modern TUI application.
runTui :: IOEnv -> Maybe Text -> Maybe Int -> Maybe Double -> Maybe Text -> Maybe Text -> Text -> Maybe (SessionInfo, [Message]) -> IO ()
runTui ioEnv0 initialPrompt mMaxTurns mMaxBudgetUsd mAppendPrompt mTheme activeSid mLoadedSession = do
  eventChan <- newBChan 100
  workerVar <- newTVarIO (Nothing :: Maybe (Async ()))
  gate <- newPermissionGate
  let ioEnv = ioEnv0 { ioResolveAsk = resolveAskWithGate gate (writeBChan eventChan . EvHarness) }

  activeWorkspace <- currentIOWorkspace ioEnv
  skills <- discoverSkills activeWorkspace
  mInstructions <- loadProjectInstructionsFile activeWorkspace
  let sysPrompt = buildSystemPromptWithAppend (fmap snd mInstructions) mAppendPrompt
      -- Blank instructions files are left out of the system prompt.
      loadedInstructions =
        [ file | Just (file, content) <- [mInstructions], not (T.null (T.strip content)) ]
  initialMode <- currentIOPermissionMode ioEnv

  let loadedConversation = maybe [] snd mLoadedSession
      loadedTranscript = messagesToTranscriptItems loadedConversation
      loadedTurns = case mLoadedSession of
        Just (info, _) -> siTurns info
        Nothing        -> 0
      baseState = (initialTuiState (ioModel ioEnv) mMaxTurns)
        { tsSkills = skills
        , tsPermissionMode = initialMode
        , tsTranscript = loadedTranscript
        , tsConversation = loadedConversation
        , tsCurrentTurn = loadedTurns
        , tsTheme = mTheme
        , tsProjectInstructions = listToMaybe loadedInstructions
        , tsCostPolicy = connectionCostPolicy (ioConnection ioEnv)
        }
      (startingState, initialActions) = initialTuiLaunch initialPrompt baseState

  let app :: App TuiState TuiEvent Name
      app = App
        { appDraw         = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent  = handleBrickEvent eventChan workerVar gate ioEnv sysPrompt mMaxBudgetUsd
        , appStartEvent   = do
            forM_ initialActions (runTuiAction eventChan workerVar gate ioEnv sysPrompt mMaxBudgetUsd)
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
  finalState <- customMain initialVty buildVty (Just eventChan) app startingState

  cancelPermissionAsk gate
  mWorker <- atomically $ readTVar workerVar
  mapM_ cancel mWorker

  saveRunSession activeWorkspace activeSid (ioModel ioEnv) (fmap fst mLoadedSession)
    (tsRunSpendUsd finalState) (tsConversation finalState)

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

-- | Convert display transcript items into messages for compatibility and
-- focused tests. Live TUI workers build requests from 'tsConversation', since
-- the transcript cannot represent all Core history.
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

-- | Per-run interpreter environment: the TUI's live model selection (changed
-- via @/model@) overrides the model captured in the startup environment. The
-- update is a snapshot, so a run already in flight keeps the model it started
-- with and the next run picks up the selection.
runEnvForModel :: Text -> IOEnv -> IOEnv
runEnvForModel model ioEnv = ioEnv { ioModel = model }

-- | Trigger background agent task execution.
triggerAgentRun
  :: BChan TuiEvent
  -> TVar (Maybe (Async ()))
  -> PermissionGate
  -> IOEnv
  -> Text         -- ^ model currently selected in the TUI
  -> Text
  -> Maybe Int
  -> Maybe Double
  -> Text
  -> [Message]
  -> EventM Name TuiState ()
triggerAgentRun eventChan workerVar gate ioEnv selectedModel sysPrompt mMaxTurns mMaxBudgetUsd currentPrompt priorHistory = do
  st <- get
  liftIO $ do
    cancelPermissionAsk gate
    mOldWorker <- atomically $ do
      w <- readTVar workerVar
      writeTVar workerVar Nothing
      pure w
    interruptWorker mOldWorker

    activeWorkspace <- currentIOWorkspace ioEnv
    finalPrompt <- expandSlashInvokedPrompt activeWorkspace (tsSkills st) currentPrompt

    newWorker <- async $ do
      let runEnv = runEnvForModel selectedModel ioEnv
          agentConfig = goalAgentConfig runEnv sysPrompt mMaxTurns mMaxBudgetUsd
      runAgentWorker (tuiAlgebra eventChan runEnv) agentConfig finalPrompt priorHistory
        (emitRunEnd eventChan)

    atomically $ writeTVar workerVar (Just newWorker)

-- | Construct the 'AgentConfig' for a goal-directed run in the TUI. The model
-- comes from the run environment, so callers route it through 'runEnvForModel'
-- to honour the TUI's live @/model@ selection.
goalAgentConfig :: IOEnv -> Text -> Maybe Int -> Maybe Double -> AgentConfig
goalAgentConfig ioEnv sysPrompt mMaxTurns mMaxBudgetUsd = AgentConfig
  { cfgModel        = ioModel ioEnv
  , cfgSystemPrompt = Just sysPrompt
  , cfgMaxTurns     = mMaxTurns
  , cfgMaxBudgetUsd = mMaxBudgetUsd
  }

-- | Post a finished run to the TUI: its spend, its final history, then its
-- terminal event.
emitRunEnd :: BChan TuiEvent -> Double -> [Message] -> AgentEvent -> IO ()
emitRunEnd eventChan spent history event = do
  writeBChan eventChan (EvRunSpend spent)
  writeBChan eventChan (EvConversation history)
  writeBChan eventChan (EvHarness event)

-- | Run a normal agent turn from the canonical conversation and return the
-- run's spend and final history before emitting its terminal event.
runAgentWorker
  :: AgentAlgebra IO
  -> AgentConfig
  -> Text
  -> [Message]
  -> (Double -> [Message] -> AgentEvent -> IO ())
  -> IO ()
runAgentWorker algebra agentConfig prompt priorHistory emit = do
  let sysPrompt = fromMaybe "" (cfgSystemPrompt agentConfig)
      initHistory = buildSessionHistory sysPrompt (Just priorHistory) prompt
  res <- trySync (foldAgentProgram algebra (agentLoop agentConfig allToolDefs initHistory))
  case res of
    Left (ex :: SomeException) ->
      emit 0 initHistory (EvError (T.pack (show ex)))
    Right (result, finalHistory, spent) ->
      emit spent finalHistory (runOutcomeEvent (runOutcome result Nothing))

-- | Execute a goal-directed agent run using the given algebra and emit events.
runGoalWorker
  :: AgentAlgebra IO
  -> AgentConfig
  -> Text
  -> [Message]
  -> (AgentEvent -> IO ())
  -> IO ()
runGoalWorker algebra agentConfig condition priorHistory emitEvent =
  runGoalWorkerWithHistory algebra agentConfig condition priorHistory (\_ _ event -> emitEvent event)

-- | Goal worker variant that returns the run's spend and canonical history
-- before its terminal event so the TUI can save and resume the exact model
-- conversation.
runGoalWorkerWithHistory
  :: AgentAlgebra IO
  -> AgentConfig
  -> Text
  -> [Message]
  -> (Double -> [Message] -> AgentEvent -> IO ())
  -> IO ()
runGoalWorkerWithHistory algebra agentConfig condition priorHistory emit = do
  let sysPrompt = fromMaybe "" (cfgSystemPrompt agentConfig)
      initHistory = buildSessionHistory sysPrompt (Just priorHistory) condition
  res <- trySync (foldAgentProgram algebra
              (goalLoop agentConfig allToolDefs condition defaultBlockCap initHistory))
  case res of
    Left (ex :: SomeException) ->
      emit 0 initHistory (EvError (T.pack (show ex)))
    Right (result, finalHistory, goalState, spent) ->
      emit spent finalHistory (runOutcomeEvent (runOutcome result (Just goalState)))

-- | The terminal TUI event for a finished run: only success ends the run as
-- done; a stopped or failed run, including an unmet goal, ends in error.
runOutcomeEvent :: RunOutcome -> AgentEvent
runOutcomeEvent outcome = case outcome of
  RunSucceeded ans -> EvDone ans
  _                -> EvError (renderRunOutcome outcome)

-- | Trigger background goal-directed agent execution.
triggerGoalRun
  :: BChan TuiEvent
  -> TVar (Maybe (Async ()))
  -> PermissionGate
  -> IOEnv
  -> Text         -- ^ model currently selected in the TUI
  -> Text
  -> Maybe Int
  -> Maybe Double
  -> Text          -- ^ goal condition (also used as the first-turn directive)
  -> [Message]
  -> EventM Name TuiState ()
triggerGoalRun eventChan workerVar gate ioEnv selectedModel sysPrompt mMaxTurns mMaxBudgetUsd condition priorHistory = liftIO $ do
  cancelPermissionAsk gate
  mOldWorker <- atomically $ do
    w <- readTVar workerVar
    writeTVar workerVar Nothing
    pure w
  interruptWorker mOldWorker

  newWorker <- async $ do
    let runEnv = runEnvForModel selectedModel ioEnv
        agentConfig = goalAgentConfig runEnv sysPrompt mMaxTurns mMaxBudgetUsd
    runGoalWorkerWithHistory (tuiAlgebra eventChan runEnv) agentConfig condition priorHistory
      (emitRunEnd eventChan)

  atomically $ writeTVar workerVar (Just newWorker)

-- | Handle Brick UI events.
handleBrickEvent
  :: BChan TuiEvent
  -> TVar (Maybe (Async ()))
  -> PermissionGate
  -> IOEnv
  -> Text
  -> Maybe Double
  -> BrickEvent Name TuiEvent
  -> EventM Name TuiState ()
handleBrickEvent eventChan workerVar gate ioEnv sysPrompt mMaxBudgetUsd = \case
  AppEvent tuiEvent -> do
    currentState <- get
    modify (fst . updateTui tuiEvent)
    case tuiEvent of
      EvHarness agentEv | shouldAutoScroll currentState agentEv ->
        vScrollToEnd (viewportScroll VpTranscript)
      _ -> pure ()

  event -> do
    case brickToUserKey event of
      Just key -> do
        currentState <- get
        let (nextState, actions) = updateTui (EvUserKey key) currentState
        put nextState
        forM_ actions (runTuiAction eventChan workerVar gate ioEnv sysPrompt mMaxBudgetUsd)
      Nothing ->
        pure ()
