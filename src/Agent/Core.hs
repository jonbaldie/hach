{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Core
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

    -- * Pure Agent Harness Loop
  , agentLoop
  , agentStep

    -- * Goal-Directed Loop
  , goalLoop
  , defaultBlockCap
  ) where

import Agent.Types
import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

-- | The core signature of interaction steps for an autonomous agent.
--
-- In the spirit of Functional Pearls, the interactions of an agent with its
-- environment (the LLM oracle, external tool runtime, and telemetry channels)
-- are modeled as a signature functor, separating the pure orchestration
-- strategy from operational interpreters.
data AgentF next
  = PromptLLM ![Message] ![ToolDef] (Either Text AssistantResponse -> next)
  | ExecuteTool !ToolCall (ToolResult -> next)
  | LogEvent !AgentEvent next
  | EvaluateGoal !Text ![Message] (GoalEvaluation -> next)
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

-- | An algebra for interpreting an 'AgentProgram' in a target monad @m@.
data AgentAlgebra m = AgentAlgebra
  { interpPrompt   :: [Message] -> [ToolDef] -> m (Either Text AssistantResponse)
  , interpTool     :: ToolCall -> m ToolResult
  , interpLog      :: AgentEvent -> m ()
  , interpEvaluate :: Text -> [Message] -> m GoalEvaluation
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
  | turn > cfgMaxTurns cfg = do
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
              -- Execute each tool call in sequence, collecting results.
              toolMsgs <- forM calls $ \call -> do
                logEvent (EvToolCall (functionName call) (callArgsRaw call))
                res <- executeTool call
                logEvent (EvToolResult (functionName call) res)
                pure $ ToolMsg (callId call) (functionName call) (toolResultToText res)

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
