{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hach.Core
  ( -- * The Agent Instruction Functor & Free Monad
    AgentF(..)
  , AgentProgram(..)
  , AgentAlgebra(..)
  , foldAgentProgram

    -- * Smart Constructors
  , promptLLM
  , executeTool
  , logEvent
  , evaluateGoal
  , checkPermission
  , runHook
  , saveSession
  , loadSession
  , spawnAgent
  , sendMessageToAgent
  , listRunningAgents
  , callMcpTool
  , listMcpTools
  , runBackground
  , getTaskOutput
  , stopTask
  , sendNotification
  , gitStatus
  , createWorktree
  , enterWorktree
  , exitWorktree
  , loadMemory
  , resolveImport

    -- * Pure Agent Harness Loop
  , agentLoop
  , agentStep

    -- * Goal-Directed Loop
  , goalLoop
  , defaultBlockCap
  ) where

import Hach.Types
import Hach.Tools (resolveTool, resolvedToolCanonicalName)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- | The core signature of interaction steps for an autonomous agent.
--
-- The interactions of an agent with its
-- environment (the LLM oracle, external tool runtime, and telemetry channels)
-- are modeled as a signature functor, separating the pure orchestration
-- strategy from operational interpreters.
data AgentF next
  = PromptLLM ![Message] ![ToolDef] (Either Text AssistantResponse -> next)
  | ExecuteTool !ToolCall (ToolResult -> next)
  | LogEvent !AgentEvent next
  | EvaluateGoal !Text ![Message] (GoalEvaluation -> next)
  | CheckPermission !Text !Text (Bool -> next)
  | RunHook !HookEvent !Text (HookResult -> next)
  | SaveSession !SessionInfo (Text -> next)
  | LoadSession !SessionId (Maybe SessionInfo -> next)
  | SpawnAgent !Text !Text (AgentId -> next)
  | SendMessageToAgent !AgentId !Text (Text -> next)
  | ListRunningAgents ([AgentInfo] -> next)
  | CallMcpTool !Text !Text !Text (ToolResult -> next)
  | ListMcpTools ([ToolDef] -> next)
  | RunBackground !Text (TaskId -> next)
  | GetTaskOutput !TaskId (TaskInfo -> next)
  | StopTask !TaskId (Bool -> next)
  | SendNotification !Text !Text (() -> next)
  | GitStatus (GitStatusInfo -> next)
  | CreateWorktree !Text (FilePath -> next)
  | EnterWorktree !FilePath (() -> next)
  | ExitWorktree (() -> next)
  | LoadMemory !FilePath (Text -> next)
  | ResolveImport !FilePath (Text -> next)
  deriving Functor

-- | The free monad over 'AgentF', describing an interactive agent script.
data AgentProgram a
  = Pure a
  | Free (AgentF (AgentProgram a))
  deriving Functor

instance Applicative AgentProgram where
  pure = Pure
  Pure f <*> Pure x = Pure (f x)
  Pure f <*> Free m = Free (fmap f <$> m)
  Free mf <*> mx    = Free (fmap (<*> mx) mf)

instance Monad AgentProgram where
  Pure x >>= f = f x
  Free m >>= f = Free (fmap (>>= f) m)

-- | Request an inference turn from the model given the dialogue context.
promptLLM :: [Message] -> [ToolDef] -> AgentProgram (Either Text AssistantResponse)
promptLLM msgs tools = Free (PromptLLM msgs tools Pure)

-- | Invoke a specific tool in the execution environment.
executeTool :: ToolCall -> AgentProgram ToolResult
executeTool call = Free (ExecuteTool call Pure)

-- | Record a harness event (telemetry, step tracking, debugging).
logEvent :: AgentEvent -> AgentProgram ()
logEvent ev = Free (LogEvent ev (Pure ()))

-- | Request a goal evaluation: send the condition and transcript to the
-- evaluator LLM and receive a verdict plus reason.
evaluateGoal :: Text -> [Message] -> AgentProgram GoalEvaluation
evaluateGoal condition transcript = Free (EvaluateGoal condition transcript Pure)

checkPermission :: Text -> Text -> AgentProgram Bool
checkPermission tool args = Free (CheckPermission tool args Pure)

runHook :: HookEvent -> Text -> AgentProgram HookResult
runHook ev payload = Free (RunHook ev payload Pure)

saveSession :: SessionInfo -> AgentProgram Text
saveSession sinfo = Free (SaveSession sinfo Pure)

loadSession :: SessionId -> AgentProgram (Maybe SessionInfo)
loadSession sid = Free (LoadSession sid Pure)

spawnAgent :: Text -> Text -> AgentProgram AgentId
spawnAgent role desc = Free (SpawnAgent role desc Pure)

sendMessageToAgent :: AgentId -> Text -> AgentProgram Text
sendMessageToAgent aid msg = Free (SendMessageToAgent aid msg Pure)

listRunningAgents :: AgentProgram [AgentInfo]
listRunningAgents = Free (ListRunningAgents Pure)

callMcpTool :: Text -> Text -> Text -> AgentProgram ToolResult
callMcpTool srv tool args = Free (CallMcpTool srv tool args Pure)

listMcpTools :: AgentProgram [ToolDef]
listMcpTools = Free (ListMcpTools Pure)

runBackground :: Text -> AgentProgram TaskId
runBackground cmd = Free (RunBackground cmd Pure)

getTaskOutput :: TaskId -> AgentProgram TaskInfo
getTaskOutput tid = Free (GetTaskOutput tid Pure)

stopTask :: TaskId -> AgentProgram Bool
stopTask tid = Free (StopTask tid Pure)

sendNotification :: Text -> Text -> AgentProgram ()
sendNotification title body = Free (SendNotification title body Pure)

gitStatus :: AgentProgram GitStatusInfo
gitStatus = Free (GitStatus Pure)

createWorktree :: Text -> AgentProgram FilePath
createWorktree name = Free (CreateWorktree name Pure)

enterWorktree :: FilePath -> AgentProgram ()
enterWorktree path = Free (EnterWorktree path Pure)

exitWorktree :: AgentProgram ()
exitWorktree = Free (ExitWorktree Pure)

loadMemory :: FilePath -> AgentProgram Text
loadMemory path = Free (LoadMemory path Pure)

resolveImport :: FilePath -> AgentProgram Text
resolveImport path = Free (ResolveImport path Pure)

-- | An algebra for interpreting an 'AgentProgram' in a target monad @m@.
data AgentAlgebra m = AgentAlgebra
  { interpPrompt           :: [Message] -> [ToolDef] -> m (Either Text AssistantResponse)
  , interpTool             :: ToolCall -> m ToolResult
  , interpLog              :: AgentEvent -> m ()
  , interpEvaluate         :: Text -> [Message] -> m GoalEvaluation
  , interpCheckPermission  :: Text -> Text -> m Bool
  , interpRunHook          :: HookEvent -> Text -> m HookResult
  , interpSaveSession      :: SessionInfo -> m Text
  , interpLoadSession      :: SessionId -> m (Maybe SessionInfo)
  , interpSpawnAgent       :: Text -> Text -> m AgentId
  , interpSendMessage      :: AgentId -> Text -> m Text
  , interpListAgents       :: m [AgentInfo]
  , interpCallMcpTool      :: Text -> Text -> Text -> m ToolResult
  , interpListMcpTools     :: m [ToolDef]
  , interpRunBackground    :: Text -> m TaskId
  , interpGetTaskOutput    :: TaskId -> m TaskInfo
  , interpStopTask         :: TaskId -> m Bool
  , interpSendNotification :: Text -> Text -> m ()
  , interpGitStatus        :: m GitStatusInfo
  , interpCreateWorktree   :: Text -> m FilePath
  , interpEnterWorktree    :: FilePath -> m ()
  , interpExitWorktree     :: m ()
  , interpLoadMemory       :: FilePath -> m Text
  , interpResolveImport    :: FilePath -> m Text
  }

-- | Catamorphism: fold an 'AgentProgram' with an 'AgentAlgebra'.
foldAgentProgram :: Monad m => AgentAlgebra m -> AgentProgram a -> m a
foldAgentProgram alg = \case
  Pure a -> pure a
  Free step -> case step of
    PromptLLM msgs tools k -> do
      resp <- interpPrompt alg msgs tools
      foldAgentProgram alg (k resp)
    ExecuteTool call k -> do
      res <- interpTool alg call
      foldAgentProgram alg (k res)
    LogEvent ev next -> do
      interpLog alg ev
      foldAgentProgram alg next
    EvaluateGoal cond msgs k -> do
      eval <- interpEvaluate alg cond msgs
      foldAgentProgram alg (k eval)
    CheckPermission tool args k -> do
      b <- interpCheckPermission alg tool args
      foldAgentProgram alg (k b)
    RunHook ev payload k -> do
      res <- interpRunHook alg ev payload
      foldAgentProgram alg (k res)
    SaveSession sinfo k -> do
      sid <- interpSaveSession alg sinfo
      foldAgentProgram alg (k sid)
    LoadSession sid k -> do
      res <- interpLoadSession alg sid
      foldAgentProgram alg (k res)
    SpawnAgent role desc k -> do
      aid <- interpSpawnAgent alg role desc
      foldAgentProgram alg (k aid)
    SendMessageToAgent aid msg k -> do
      reply <- interpSendMessage alg aid msg
      foldAgentProgram alg (k reply)
    ListRunningAgents k -> do
      agents <- interpListAgents alg
      foldAgentProgram alg (k agents)
    CallMcpTool srv tool args k -> do
      res <- interpCallMcpTool alg srv tool args
      foldAgentProgram alg (k res)
    ListMcpTools k -> do
      tools <- interpListMcpTools alg
      foldAgentProgram alg (k tools)
    RunBackground cmd k -> do
      tid <- interpRunBackground alg cmd
      foldAgentProgram alg (k tid)
    GetTaskOutput tid k -> do
      info <- interpGetTaskOutput alg tid
      foldAgentProgram alg (k info)
    StopTask tid k -> do
      ok <- interpStopTask alg tid
      foldAgentProgram alg (k ok)
    SendNotification title body k -> do
      interpSendNotification alg title body
      foldAgentProgram alg (k ())
    GitStatus k -> do
      st <- interpGitStatus alg
      foldAgentProgram alg (k st)
    CreateWorktree name k -> do
      path <- interpCreateWorktree alg name
      foldAgentProgram alg (k path)
    EnterWorktree path k -> do
      interpEnterWorktree alg path
      foldAgentProgram alg (k ())
    ExitWorktree k -> do
      interpExitWorktree alg
      foldAgentProgram alg (k ())
    LoadMemory path k -> do
      content <- interpLoadMemory alg path
      foldAgentProgram alg (k content)
    ResolveImport path k -> do
      content <- interpResolveImport alg path
      foldAgentProgram alg (k content)

-- | Execute a single turn of the agent harness.
-- Returns either 'Left (finalResult, updatedHistory)' if the interaction
-- has terminated (either with an answer, an error, or max turns reached),
-- or 'Right updatedHistory' if another turn should be taken.
agentStep
  :: AgentConfig
  -> [ToolDef]
  -> Int
  -> [Message]
  -> AgentProgram (Either (AgentResult, [Message]) [Message])
agentStep cfg tools turn currentHistory
  | maybe False (turn >) (cfgMaxTurns cfg) = do
      logEvent (EvError "Maximum turns exceeded")
      pure $ Left (AgentMaxTurnsReached (turn - 1), currentHistory)
  | otherwise = do
      logEvent (EvTurnStart turn)
      logEvent (EvPromptingLLM (length currentHistory))
      promptLLM currentHistory tools >>= \case
        Left err -> do
          logEvent (EvError err)
          pure $ Left (AgentFailed err, currentHistory)
        Right resp -> do
          logEvent (EvLLMResponse (respContent resp) (respToolCalls resp) (respUsage resp))

          case respToolCalls resp of
            [] -> do
              -- The assistant did not call any tools; return its final message.
              let content = fromMaybe "" (respContent resp)
                  finalHistory = currentHistory ++ [AssistantMsg (respContent resp) []]
              logEvent (EvDone content)
              pure $ Left (AgentCompleted content, finalHistory)

            calls -> do
              -- The assistant invoked one or more tools.
              -- Record the assistant's intention in history first.
              let asstMsg = AssistantMsg (respContent resp) calls
              -- Execute each tool call in sequence, checking hooks and permissions.
              toolMsgs <- forM calls $ \call -> do
                logEvent (EvToolCall (functionName call) (callArgsRaw call))
                -- 1. Hook PreToolUse
                preHook <- runHook HookPreToolUse (functionName call <> " " <> callArgsRaw call)
                let isBlocked = case hrDecision preHook of
                      Just (PermDeny _) -> True
                      _                 -> False
                if isBlocked
                  then do
                    logEvent (EvHookTriggered "PreToolUse" "Blocked tool execution")
                    logEvent (EvPermissionDenied (functionName call) "Blocked by PreToolUse hook")
                    pure $ ToolMsg (callId call) (functionName call) "Execution blocked by PreToolUse hook."
                  else do
                    let effectiveCall = case hrModifiedInput preHook of
                          Just newVal -> call { callArgsRaw = TE.decodeUtf8 (BSL.toStrict (Aeson.encode newVal)) }
                          Nothing     -> call
                    case resolveTool effectiveCall of
                      Just (Left err) -> do
                        let res = ToolError err
                        logEvent (EvToolResult (functionName effectiveCall) res)
                        pure $ ToolMsg (callId effectiveCall) (functionName effectiveCall) (toolResultToText res)
                      resolved -> do
                        -- Registry aliases are authorized under their canonical name.
                        let permissionTool = case resolved of
                              Just (Right tool) -> resolvedToolCanonicalName tool
                              Nothing           -> functionName effectiveCall
                        allowed <- checkPermission permissionTool (callArgsRaw effectiveCall)
                        if not allowed
                          then do
                            logEvent (EvPermissionDenied permissionTool "Permission denied by policy")
                            pure $ ToolMsg (callId effectiveCall) (functionName effectiveCall) "Execution denied by permission policy."
                          else do
                            -- 3. Execute tool
                            res <- executeTool effectiveCall
                            logEvent (EvToolResult (functionName effectiveCall) res)
                            -- 4. Hook PostToolUse
                            postHook <- runHook HookPostToolUse (functionName effectiveCall <> " " <> toolResultToText res)
                            let baseOutput = toolResultToText res
                                finalOutput = case hrAdditionalContext postHook of
                                  Just extra -> baseOutput <> "\n[Additional Context]: " <> extra
                                  Nothing    -> baseOutput
                            pure $ ToolMsg (callId effectiveCall) (functionName effectiveCall) finalOutput

              let updatedHistory = currentHistory ++ [asstMsg] ++ toolMsgs
              logEvent (EvTurnComplete turn)
              pure $ Right updatedHistory

-- | The pure, recursive agent harness loop.
-- Unfolds turns until completion or the maximum turn limit is reached.
agentLoop
  :: AgentConfig
  -> [ToolDef]
  -> [Message]
  -> AgentProgram (AgentResult, [Message])
agentLoop cfg tools initialHistory = loop 1 initialHistory
  where
    loop turn hist = do
      agentStep cfg tools turn hist >>= \case
        Left (result, finalHist) -> pure (result, finalHist)
        Right nextHist          -> loop (turn + 1) nextHist

-- | Default block cap: number of consecutive no-progress turns before the
-- goal loop stops and returns control to the user.
defaultBlockCap :: Int
defaultBlockCap = 3

-- | Goal-directed agent loop.
--
-- Wraps 'agentStep' with a post-turn evaluation step.  After each turn that
-- completes without tool calls (i.e. the agent would otherwise stop), the
-- harness sends the condition and transcript to the evaluator LLM.  The
-- verdict determines whether the loop continues, the goal is achieved, or the
-- goal is impossible.
--
-- Turn that make tool calls count as progress and reset the no-progress
-- counter.  Consecutive completed turns without tool use increment it; when it
-- reaches the block cap the loop stops with a warning and the goal stays
-- active so the user can resume.
goalLoop
  :: AgentConfig
  -> [ToolDef]
  -> Text          -- ^ goal condition
  -> Int           -- ^ block cap (max consecutive no-progress turns)
  -> [Message]
  -> AgentProgram (AgentResult, [Message], GoalState)
goalLoop cfg tools condition blockCap initialHistory = do
  logEvent (EvGoalSet condition)
  loop 1 (initialGoalState condition) initialHistory
  where
    -- Clamp to a minimum of 1: a block cap of 0 or less is degenerate because
    -- the block decision is only reached *after* a no-progress turn runs, so
    -- the counter would otherwise exceed the cap.  1 is the smallest value
    -- that lets the invariant 'gsNoProgressCount <= blockCap' hold.
    effectiveCap = max 1 blockCap
    loop turn gs hist = do
      agentStep cfg tools turn hist >>= \case
        Right nextHist ->
          -- Tool calls were made: progress.  Reset no-progress counter.
          loop (turn + 1) gs { gsNoProgressCount = 0 } nextHist

        Left (result, finalHist) -> case result of
          AgentCompleted content ->
            case classifyCompletion content of
              GoalErrUnrecoverable -> do
                let g' = gs { gsStatus = GoalFailed }
                logEvent (EvGoalFailed condition content)
                pure (result, finalHist, g')

              GoalErrTransient ->
                -- Stop the loop but keep the goal active for retry.
                pure (result, finalHist, gs)

              GoalNoError -> do
                eval <- evaluateGoal condition finalHist
                let verdict = geVerdict eval
                    reason  = geReason eval
                    g1 = gs
                      { gsTurnCount       = gsTurnCount gs + 1
                      , gsLastVerdict     = Just verdict
                      , gsLastReason      = Just reason
                      , gsNoProgressCount = gsNoProgressCount gs + 1
                      }
                logEvent (EvGoalEvaluated verdict reason)
                case verdict of
                  GoalMet -> do
                    let g2 = g1 { gsStatus = GoalAchieved }
                    logEvent (EvGoalAchieved condition)
                    pure (result, finalHist, g2)

                  GoalImpossible -> do
                    let g2 = g1 { gsStatus = GoalFailed }
                    logEvent (EvGoalFailed condition reason)
                    pure (result, finalHist, g2)

                  GoalNotYetMet ->
                    if gsNoProgressCount g1 >= effectiveCap
                      then do
                        logEvent (EvGoalBlocked condition)
                        pure (result, finalHist, g1)
                      else do
                        let guidance = UserMsg
                              ( "Goal not yet met. " <> reason
                              <> " Continue working toward: " <> condition )
                            newHist = finalHist ++ [guidance]
                        loop (turn + 1) g1 newHist

          AgentMaxTurnsReached _n ->
            pure (result, finalHist, gs)

          AgentFailed err ->
            case classifyError err of
              GoalErrUnrecoverable -> do
                let g' = gs { gsStatus = GoalFailed }
                logEvent (EvGoalFailed condition err)
                pure (result, finalHist, g')
              _ ->
                pure (result, finalHist, gs)
