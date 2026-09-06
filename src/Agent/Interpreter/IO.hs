{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.Interpreter.IO
  ( IOEnv(..)
  , newIOEnv
  , ioAlgebra
  , runIO
  ) where

import Agent.Core
import Agent.OpenRouter
import Agent.Tools
import Agent.Types
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Runtime environment for executing an agent harness in real IO.
data IOEnv = IOEnv
  { ioManager   :: !Manager
  , ioApiKey    :: !Text
  , ioModel     :: !Text
  , ioWorkspace :: !FilePath
  , ioVerbose   :: !Bool
  }

-- | Initialize a new 'IOEnv' with a TLS manager.
newIOEnv :: Text -> Text -> FilePath -> Bool -> IO IOEnv
newIOEnv apiKey model workspace verbose = do
  mgr <- newManager tlsManagerSettings
  pure IOEnv
    { ioManager   = mgr
    , ioApiKey    = apiKey
    , ioModel     = model
    , ioWorkspace = workspace
    , ioVerbose   = verbose
    }

-- | Format and print events to the console for CLI observability.
renderEventIO :: Bool -> AgentEvent -> IO ()
renderEventIO verbose = \case
  EvTurnStart n ->
    when verbose $ putStrLn ("\n=== Turn " <> show n <> " ===")

  EvPromptingLLM msgCount ->
    when verbose $ putStrLn ("-> Prompting LLM with " <> show msgCount <> " messages in context...")

  EvLLMResponse mContent calls mUsage ->
    when verbose $ do
      case mUsage of
        Just TokenUsage{..} ->
          putStrLn ("<- Context tokens: " <> show tuTotalTokens <> " (prompt: " <> show tuPromptTokens <> ", completion: " <> show tuCompletionTokens <> ")")
        Nothing -> pure ()
      case mContent of
        Just c | not (T.null c) -> do
          putStrLn "<- Assistant:"
          TIO.putStrLn c
        _ -> pure ()
      when (not (null calls)) $
        putStrLn ("<- Assistant requested " <> show (length calls) <> " tool call(s)")

  EvToolCall name args ->
    when verbose $ do
      putStrLn ("\n[Tool Executing] " <> T.unpack name)
      TIO.putStrLn ("  Arguments: " <> args)

  EvToolResult name res ->
    when verbose $ do
      case res of
        ToolSuccess out -> do
          putStrLn ("[Tool " <> T.unpack name <> " Success]")
          let preview = if T.length out > 200 then T.take 200 out <> "\n...(truncated)..." else out
          TIO.putStrLn preview
        ToolError err -> do
          putStrLn ("[Tool " <> T.unpack name <> " Error]")
          TIO.putStrLn ("  " <> err)

  EvTurnComplete n ->
    when verbose $ putStrLn ("--- Completed Turn " <> show n <> " ---")

  EvDone ans -> do
    putStrLn "\n==================== Final Answer ===================="
    TIO.putStrLn ans
    putStrLn "======================================================"

  EvError err -> do
    putStrLn ("\n[Agent Error]: " <> T.unpack err)

-- | Concrete IO algebra interpreting agent instructions against real OpenRouter and OS.
ioAlgebra :: IOEnv -> AgentAlgebra IO
ioAlgebra IOEnv{..} = AgentAlgebra
  { interpPrompt = \msgs tools -> do
      let req = ChatRequest
            { reqModel      = ioModel
            , reqMessages   = msgs
            , reqTools      = tools
            , reqToolChoice = Just "auto"
            }
      sendChatCompletion ioManager ioApiKey req

  , interpTool = \call ->
      executeCodingTool ioWorkspace call

  , interpLog = renderEventIO ioVerbose
  }

-- | Run an 'AgentProgram' using real OpenRouter API and local filesystem.
runIO :: IOEnv -> AgentProgram a -> IO a
runIO env prog = foldAgentProgram (ioAlgebra env) prog
