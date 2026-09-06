{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Interpreter.IO
  ( IOEnv(..)
  , newIOEnv
  , ioAlgebra
  , ioAlgebraWithLog
  , runIO
  , evaluatorSystemPrompt
  , parseGoalEvaluation
  ) where

import Hach.Core
import qualified Hach.Git as Git
import Hach.Memory (loadHierarchicalMemory, resolveMemoryImports)
import Hach.Notifications (sendDesktopNotification)
import Hach.OpenRouter
import qualified Hach.Sessions as Sessions
import Hach.Tools
import Hach.Types
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.FilePath ((</>))

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

  EvGoalSet cond ->
    putStrLn ("\n[Goal] Set: " <> T.unpack cond)

  EvGoalEvaluated verdict reason ->
    putStrLn ("\n[Goal] Evaluated: " <> show verdict <> " — " <> T.unpack reason)

  EvGoalEvaluationUsage TokenUsage{..} ->
    when verbose $
      putStrLn ("\n[Goal Evaluator Usage] " <> show tuPromptTokens <> " prompt, "
                <> show tuCompletionTokens <> " completion, "
                <> show tuTotalTokens <> " total tokens")

  EvGoalAchieved cond ->
    putStrLn ("\n[Goal] Achieved: " <> T.unpack cond)

  EvGoalFailed cond reason ->
    putStrLn ("\n[Goal] Failed: " <> T.unpack cond <> " — " <> T.unpack reason)

  EvGoalCleared cond ->
    putStrLn ("\n[Goal] Cleared: " <> T.unpack cond)

  EvGoalBlocked cond ->
    putStrLn ("\n[Goal] No progress detected. Goal still active: " <> T.unpack cond)

  EvPartialResponse delta ->
    when verbose $ TIO.putStr delta

  EvToolCallDelta delta ->
    when verbose $ TIO.putStr delta

  EvPermissionDenied tool reason ->
    putStrLn ("\n[Permission Denied] " <> T.unpack tool <> ": " <> T.unpack reason)

  EvHookTriggered hook msg ->
    when verbose $ putStrLn ("\n[Hook " <> T.unpack hook <> "] " <> T.unpack msg)

  EvSessionSaved sid ->
    when verbose $ putStrLn ("\n[Session Saved] " <> T.unpack sid)

  EvNotificationSent msg ->
    putStrLn ("\n[Notification] " <> T.unpack msg)

-- | System prompt instructing the evaluator LLM to judge goal completion.
evaluatorSystemPrompt :: Text
evaluatorSystemPrompt =
  "You are a goal evaluator. You must determine whether a completion condition \
  \is met, not yet met, or impossible, based only on the conversation transcript. \
  \You cannot run tools or read files. You see only what the agent has surfaced. \
  \Respond with JSON only: {\"verdict\": \"met\" | \"not_yet_met\" | \"impossible\", \"reason\": \"<short reason>\"}"

-- | Parse the evaluator LLM response content into a 'GoalEvaluation'.
-- Handles raw JSON, markdown-wrapped JSON, and JSON embedded in prose.
parseGoalEvaluation :: Text -> GoalEvaluation
parseGoalEvaluation content =
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 content) of
    Right ge -> ge
    Left _   -> extractJson content
  where
    extractJson txt =
      case T.breakOn "{" txt of
        (_, rest) | not (T.null rest) ->
          -- Extract from the first '{' to the last '}' to handle
          -- markdown code fences and trailing text.
          let jsonPart = fst (T.breakOnEnd "}" rest)
          in if T.null jsonPart
               then fallback
               else case Aeson.eitherDecodeStrict (TE.encodeUtf8 jsonPart) of
                      Right ge -> ge
                      Left _   -> fallback
        _ -> fallback
    fallback = GoalEvaluation GoalNotYetMet "Could not parse evaluator response."

-- | Concrete IO algebra interpreting agent instructions against real OpenRouter and OS.
ioAlgebra :: IOEnv -> AgentAlgebra IO
ioAlgebra env = ioAlgebraWithLog (renderEventIO (ioVerbose env)) env

-- | Concrete IO algebra parameterized by an event logger (useful for TUI piping).
ioAlgebraWithLog :: (AgentEvent -> IO ()) -> IOEnv -> AgentAlgebra IO
ioAlgebraWithLog logger IOEnv{..} = AgentAlgebra
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

  , interpLog = logger

  , interpEvaluate = \condition transcript -> do
      let evalMsgs = SystemMsg evaluatorSystemPrompt
                   : UserMsg ("Condition: " <> condition <> "\n\nTranscript:\n" <> transcriptToText transcript)
                   : []
          req = ChatRequest
            { reqModel      = ioModel
            , reqMessages   = evalMsgs
            , reqTools      = []
            , reqToolChoice = Nothing
            }
      res <- sendChatCompletion ioManager ioApiKey req
      case res of
        Right asstResp -> do
          mapM_ (logger . EvGoalEvaluationUsage) (respUsage asstResp)
          case respContent asstResp of
            Just content -> pure (parseGoalEvaluation content)
            Nothing      -> pure (GoalEvaluation GoalNotYetMet "Empty evaluator response.")
        Left err ->
          pure (GoalEvaluation GoalNotYetMet ("Evaluator error: " <> err))

  , interpCheckPermission = \_tool _args -> pure True
  , interpRunHook = \_ev _payload -> pure defaultHookResult
  , interpSaveSession = \sinfo -> do
      Sessions.saveSession (ioWorkspace </> ".agents" </> "sessions") sinfo []
      pure (siId sinfo)
  , interpLoadSession = \sid -> do
      mRes <- Sessions.loadSession (ioWorkspace </> ".agents" </> "sessions") sid
      case mRes of
        Just _  -> pure (fmap fst mRes)
        Nothing -> do
          mResLegacy <- Sessions.loadSession (ioWorkspace </> ".agent" </> "sessions") sid
          pure (fmap fst mResLegacy)
  , interpSpawnAgent = \role _desc -> pure (AgentId ("agent_" <> role))
  , interpSendMessage = \aid msg -> pure ("Sent to " <> unAgentId aid <> ": " <> msg)
  , interpListAgents = pure
      [ AgentInfo (AgentId "explore") "explore" "default" "idle"
      , AgentInfo (AgentId "plan") "plan" "default" "idle"
      ]
  , interpCallMcpTool = \srv tool args ->
      pure (ToolSuccess ("MCP " <> srv <> "/" <> tool <> " called with: " <> args))
  , interpListMcpTools = pure []
  , interpRunBackground = \_cmd -> pure (TaskId "bg-cmd")
  , interpGetTaskOutput = \tid -> pure (TaskInfo tid "" "completed" "task done")
  , interpStopTask = \_tid -> pure True
  , interpSendNotification = \title body -> do
      _ <- sendDesktopNotification title body
      pure ()
  , interpGitStatus = Git.getGitStatus ioWorkspace
  , interpCreateWorktree = \name -> do
      res <- Git.createWorktree ioWorkspace name
      case res of
        Right p -> pure p
        Left err -> pure (T.unpack err)
  , interpEnterWorktree = \_path -> pure ()
  , interpExitWorktree = pure ()
  , interpLoadMemory = \path -> T.unlines <$> loadHierarchicalMemory ioWorkspace path
  , interpResolveImport = \path -> resolveMemoryImports ioWorkspace 4 path
  }
  where
    transcriptToText = T.unlines . map messageToText

    messageToText = \case
      SystemMsg c    -> "[System] " <> c
      UserMsg c      -> "[User] " <> c
      AssistantMsg mc _ -> "[Assistant] " <> fromMaybe "" mc
      ToolMsg _ name c -> "[Tool " <> name <> "] " <> c

-- | Run an 'AgentProgram' using real OpenRouter API and local filesystem.
runIO :: IOEnv -> AgentProgram a -> IO a
runIO env prog = foldAgentProgram (ioAlgebra env) prog
