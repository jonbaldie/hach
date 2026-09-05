{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.TUI.App
  ( runTui
  , vtyToUserKey
  ) where

import Agent.Core
import Agent.Env (buildSystemPrompt, loadProjectInstructions)
import Agent.Interpreter.IO
import Agent.OpenRouter
import Agent.Skills (discoverSkills, injectSkillsIntoPrompt, parseSkillInvocations)
import Agent.Tools
import Agent.TUI.State
import Agent.TUI.Types
import Agent.TUI.UI
import Agent.Types
import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
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
tuiAlgebra chan IOEnv{..} = AgentAlgebra
  { interpPrompt = \msgs tools -> do
      let req = ChatRequest
            { reqModel      = ioModel
            , reqMessages   = msgs
            , reqTools      = tools
            , reqToolChoice = Just "auto"
            }
      res <- sendChatCompletion ioManager ioApiKey req
      case res of
        Right asstResp -> pure asstResp
        Left err       -> do
          writeBChan chan (EvError err)
          pure $ AssistantResponse (Just ("[API Error]: " <> err)) [] Nothing

  , interpTool = \call ->
      executeCodingTool ioWorkspace call

  , interpLog = \ev ->
      writeBChan chan ev
  }

-- | Run the full modern TUI application.
runTui :: IOEnv -> Maybe Text -> IO ()
runTui ioEnv initialPrompt = do
  eventChan <- newBChan 100
  workerVar <- newTVarIO (Nothing :: Maybe (Async ()))

  skills <- discoverSkills (ioWorkspace ioEnv)
  mGuidelines <- loadProjectInstructions (ioWorkspace ioEnv)
  let sysPrompt = buildSystemPrompt mGuidelines

  let baseState = (initialTuiState (ioModel ioEnv) 10) { tsSkills = skills }
      startingState = case initialPrompt of
        Just p  -> fst $ updateTui (EvSubmit p) baseState
        Nothing -> baseState

  let app :: App TuiState AgentEvent Name
      app = App
        { appDraw         = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent  = handleBrickEvent eventChan workerVar ioEnv sysPrompt
        , appStartEvent   = do
            -- If an initial prompt was provided on CLI, trigger its execution
            case initialPrompt of
              Just p  -> do
                currentState <- get
                let (cleaned, invoked) = parseSkillInvocations (tsSkills currentState) (T.strip p)
                    finalP = injectSkillsIntoPrompt invoked (if T.null cleaned then p else cleaned)
                triggerAgentRun eventChan workerVar ioEnv sysPrompt finalP [DiUser p]
              Nothing -> pure ()
        , appAttrMap      = const tuiAttrMap
        }

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

-- | Convert dialogue history into LLM messages for multi-turn context.
-- Uses the expanded prompt for the latest turn so that skill instructions
-- reach the model while preserving clean display history in the UI.
dialogueToMessages :: Text -> Text -> [DialogueItem] -> [Message]
dialogueToMessages sysPrompt currentPrompt items =
  let priorItems = dropLastUser items
      priorMsgs  = concatMap itemToMessages priorItems
  in SystemMsg sysPrompt : priorMsgs ++ [UserMsg currentPrompt]
  where
    dropLastUser [] = []
    dropLastUser (x:xs) =
      case reverse (x:xs) of
        (DiUser _ : rest) -> reverse rest
        _                 -> x : xs

    itemToMessages = \case
      DiUser u      -> [UserMsg u]
      DiAssistant a -> [AssistantMsg (Just a) []]
      DiSystem s    -> [SystemMsg s]
      DiNotice _    -> []

-- | Trigger background agent task execution.
triggerAgentRun
  :: BChan AgentEvent
  -> TVar (Maybe (Async ()))
  -> IOEnv
  -> Text
  -> Text
  -> [DialogueItem]
  -> EventM Name TuiState ()
triggerAgentRun eventChan workerVar ioEnv sysPrompt currentPrompt historyItems = liftIO $ do
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
          , cfgMaxTurns     = 10
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
    modify (handleAgentEvent agentEv)
    case agentEv of
      EvLLMResponse _ _ _ -> vScrollToEnd (viewportScroll VpHistory)
      EvDone _            -> vScrollToEnd (viewportScroll VpHistory)
      EvError _         -> vScrollToEnd (viewportScroll VpHistory)
      EvToolCall _ _    -> vScrollToEnd (viewportScroll VpTools)
      _                 -> pure ()

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
            triggerAgentRun eventChan workerVar ioEnv sysPrompt prompt (tsHistory nextState)
            vScrollToEnd (viewportScroll VpHistory)
          ActionScrollHistory delta ->
            vScrollBy (viewportScroll VpHistory) delta
          ActionScrollHistoryToBottom ->
            vScrollToEnd (viewportScroll VpHistory)
          ActionScrollTools delta ->
            vScrollBy (viewportScroll VpTools) (delta * 2)
      Nothing ->
        pure ()

  _ ->
    pure ()
