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

    -- * Pure Agent Harness Loop
  , agentLoop
  , agentStep
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

-- | An algebra for interpreting an 'AgentProgram' in a target monad @m@.
data AgentAlgebra m = AgentAlgebra
  { interpPrompt  :: [Message] -> [ToolDef] -> m (Either Text AssistantResponse)
  , interpTool    :: ToolCall -> m ToolResult
  , interpLog     :: AgentEvent -> m ()
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
