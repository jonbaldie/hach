{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.TUI.App
  ( runTui
  , buildTuiSystemPrompt
  , vtyToUserKey
  , brickToUserKey
  , messagesToTranscriptItems
  , shouldAutoScroll
  , isTranscriptAppendingEvent
  , runAgentWorker
  , runGoalWorker
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
import Hach.Env (buildSystemPromptWithAppend, formatUsd, loadProjectInstructions, loadProjectInstructionsFile)
import Hach.Git (getGitDiff)
import Hach.Interpreter.IO
import Hach.Skills (discoverSkills, expandSlashInvokedPrompt)
import Hach.Sessions (buildSessionHistory, saveRunSession)
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
import Control.Monad (forM_, void, when)
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

-- | Algebra that pipes every agent execution event into the Brick BChan.
tuiAlgebra :: BChan AgentEvent -> IOEnv -> AgentAlgebra IO
tuiAlgebra chan env = ioAlgebraWithLog (writeBChan chan) env

-- | Compute starting state and initial actions from an optional CLI prompt.
initialTuiLaunch :: Maybe Text -> TuiState -> (TuiState, [TuiAction])
initialTuiLaunch Nothing st = (st, [])
initialTuiLaunch (Just p) st
  | T.null (T.strip p) = (st, [])
  | otherwise          = updateTui (EvSubmit p) st

-- | Execute one side-effecting action requested by the pure reducer.
runTuiAction
  :: BChan AgentEvent
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
  let ioEnv = ioEnv0 { ioResolveAsk = resolveAskWithGate gate (writeBChan eventChan) }

  activeWorkspace <- currentIOWorkspace ioEnv
  skills <- discoverSkills activeWorkspace
  mInstructions <- loadProjectInstructionsFile activeWorkspace
  let sysPrompt = buildSystemPromptWithAppend (fmap snd mInstructions) mAppendPrompt
      -- Blank instructions files are left out of the system prompt.
      loadedInstructions =
        [ file | Just (file, content) <- [mInstructions], not (T.null (T.strip content)) ]
  initialMode <- currentIOPermissionMode ioEnv

  let loadedConversation = maybe [] snd mLoadedSession
      loadedTurns = case mLoadedSession of
        Just (info, _) -> siTurns info
        Nothing        -> 0
      baseState = (initialTuiState (ioModel ioEnv) mMaxTurns)
        { tsSkills = skills
        , tsPermissionMode = initialMode
        , tsTranscript = messagesToTranscriptItems loadedConversation
        , tsConversation = loadedConversation
        , tsCurrentTurn = loadedTurns
        , tsTheme = mTheme
        , tsProjectInstructions = listToMaybe loadedInstructions
        }
      (startingState, initialActions) = initialTuiLaunch initialPrompt baseState

  let app :: App TuiState AgentEvent Name
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

  saveRunSession activeWorkspace activeSid (ioModel ioEnv) (fmap fst mLoadedSession) (tsConversation finalState)

-- | Per-run interpreter environment: the TUI's live model selection (changed
-- via @/model@) overrides the model captured in the startup environment. The
-- update is a snapshot, so a run already in flight keeps the model it started
-- with and the next run picks up the selection.
runEnvForModel :: Text -> IOEnv -> IOEnv
runEnvForModel model ioEnv = ioEnv { ioModel = model }

-- | Trigger background agent task execution.
triggerAgentRun
  :: BChan AgentEvent
  -> TVar (Maybe (Async ()))
  -> PermissionGate
  -> IOEnv
  -> Text         -- ^ model currently selected in the TUI
  -> Text
  -> Maybe Int
  -> Maybe Double
  -> Text
  -> [Message]    -- ^ conversation so far
  -> EventM Name TuiState ()
triggerAgentRun eventChan workerVar gate ioEnv selectedModel sysPrompt mMaxTurns mMaxBudgetUsd currentPrompt conversation = do
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
      runAgentWorker (tuiAlgebra eventChan runEnv) agentConfig finalPrompt conversation (writeBChan eventChan)

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

-- | Run one agent turn on a prompt after the conversation so far, the same
-- way a headless run does, and emit its events and resulting conversation.
runAgentWorker
  :: AgentAlgebra IO
  -> AgentConfig
  -> Text              -- ^ prompt, with any invoked skills expanded
  -> [Message]         -- ^ conversation so far
  -> (AgentEvent -> IO ())
  -> IO ()
runAgentWorker algebra agentConfig prompt conversation emitEvent = do
  let initHistory = buildSessionHistory (workerSystemPrompt agentConfig) (Just conversation) prompt
  res <- trySync (foldAgentProgram (reportingConversation emitEvent algebra)
              (agentLoop agentConfig allToolDefs initHistory))
  emitRunOutcome emitEvent res

-- | Execute a goal-directed agent run using the given algebra and emit events.
runGoalWorker
  :: AgentAlgebra IO
  -> AgentConfig
  -> Text
  -> [Message]         -- ^ conversation so far
  -> (AgentEvent -> IO ())
  -> IO ()
runGoalWorker algebra agentConfig condition conversation emitEvent = do
  let initHistory = buildSessionHistory (workerSystemPrompt agentConfig) (Just conversation) condition
  res <- trySync (foldAgentProgram (reportingConversation emitEvent algebra)
              (goalLoop agentConfig allToolDefs condition defaultBlockCap initHistory))
  emitRunOutcome emitEvent (fmap (\(result, history, _) -> (result, history)) res)

workerSystemPrompt :: AgentConfig -> Text
workerSystemPrompt = fromMaybe "" . cfgSystemPrompt

-- | Report the conversation before every model call, so a run cancelled
-- part-way still leaves the TUI with the history the model last saw.
reportingConversation :: (AgentEvent -> IO ()) -> AgentAlgebra IO -> AgentAlgebra IO
reportingConversation emitEvent algebra = algebra
  { interpPrompt = \msgs tools -> do
      emitEvent (EvConversationUpdated msgs)
      interpPrompt algebra msgs tools
  }

-- | Hand a finished run's conversation back to the TUI, then report how it ended.
emitRunOutcome :: (AgentEvent -> IO ()) -> Either SomeException (AgentResult, [Message]) -> IO ()
emitRunOutcome emitEvent = \case
  Left ex -> emitEvent (EvError (T.pack (show ex)))
  Right (result, history) -> do
    emitEvent (EvConversationUpdated history)
    emitEvent $ case result of
      AgentCompleted ans -> EvDone ans
      AgentMaxTurnsReached n -> EvError ("Maximum turns reached (" <> T.pack (show n) <> ")")
      AgentBudgetExceeded spent budget ->
        EvError ("Budget exceeded (" <> formatUsd spent <> " spent of " <> formatUsd budget <> ")")
      AgentFailed err -> EvError err

-- | Trigger background goal-directed agent execution.
triggerGoalRun
  :: BChan AgentEvent
  -> TVar (Maybe (Async ()))
  -> PermissionGate
  -> IOEnv
  -> Text         -- ^ model currently selected in the TUI
  -> Text
  -> Maybe Int
  -> Maybe Double
  -> Text          -- ^ goal condition (also used as the first-turn directive)
  -> [Message]     -- ^ conversation so far
  -> EventM Name TuiState ()
triggerGoalRun eventChan workerVar gate ioEnv selectedModel sysPrompt mMaxTurns mMaxBudgetUsd condition conversation = liftIO $ do
  cancelPermissionAsk gate
  mOldWorker <- atomically $ do
    w <- readTVar workerVar
    writeTVar workerVar Nothing
    pure w
  interruptWorker mOldWorker

  newWorker <- async $ do
    let runEnv = runEnvForModel selectedModel ioEnv
        agentConfig = goalAgentConfig runEnv sysPrompt mMaxTurns mMaxBudgetUsd
    runGoalWorker (tuiAlgebra eventChan runEnv) agentConfig condition conversation (writeBChan eventChan)

  atomically $ writeTVar workerVar (Just newWorker)

-- | Handle Brick UI events.
handleBrickEvent
  :: BChan AgentEvent
  -> TVar (Maybe (Async ()))
  -> PermissionGate
  -> IOEnv
  -> Text
  -> Maybe Double
  -> BrickEvent Name AgentEvent
  -> EventM Name TuiState ()
handleBrickEvent eventChan workerVar gate ioEnv sysPrompt mMaxBudgetUsd = \case
  AppEvent agentEv -> do
    currentState <- get
    modify (handleAgentEvent agentEv)
    when (shouldAutoScroll currentState agentEv) $
      vScrollToEnd (viewportScroll VpTranscript)

  event -> do
    case brickToUserKey event of
      Just key -> do
        currentState <- get
        let (nextState, actions) = updateTui (EvUserKey key) currentState
        put nextState
        forM_ actions (runTuiAction eventChan workerVar gate ioEnv sysPrompt mMaxBudgetUsd)
      Nothing ->
        pure ()
