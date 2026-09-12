{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Interpreter.IO
  ( IOEnv(..)
  , IOEnvPermissions(..)
  , initializeProjectWorkspace
  , initializeWorkspaceInstructionsFile
  , defaultIOEnvPermissions
  , newIOEnv
  , newIOEnvWithPermissions
  , setIOPermissionMode
  , currentIOPermissionMode
  , currentIOWorkspace
  , currentIOWorktree
  , ioAlgebra
  , ioAlgebraWithLog
  , renderEventIO
  , runIO
  , evaluatorSystemPrompt
  , parseGoalEvaluation
  , chatRequestFor
  ) where

import Hach.Core
import qualified Hach.Git as Git
import Hach.Hooks (executeHooks)
import Hach.Memory (loadHierarchicalMemory, resolveMemoryImports)
import Hach.Notifications (sendDesktopNotification)
import Hach.OpenRouter
import Hach.Permissions (evalPermission, evalPermissionForAuthority)
import qualified Hach.Sessions as Sessions
import Hach.Tools
import Hach.TUI.Types (ProjectInitializationResult(..))
import Hach.Types
import Control.Monad (when)
import Control.Exception (bracket, displayException, try)
import qualified Data.Aeson as Aeson
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO (hClose, hPutStrLn, stderr)
import System.Posix.IO (OpenFileFlags(..), OpenMode(WriteOnly), defaultFileFlags, fdToHandle, openFd)
import System.IO.Error (isAlreadyExistsError)

-- | Markdown starter instructions written by the TUI's @/init@ command.
claudeInstructionsTemplate :: Text
claudeInstructionsTemplate = T.unlines
  [ "# Project Guidelines"
  , ""
  , "Add project-specific instructions for the coding agent here."
  , ""
  , "## Development"
  , ""
  , "- Describe how to build and test the project."
  , "- Note conventions or constraints the agent should follow."
  ]

-- | Create the active project's 'CLAUDE.md' without replacing an existing file.
-- The active workspace is read at action time so worktree switches are honoured.
initializeProjectWorkspace :: IOEnv -> IO ProjectInitializationResult
initializeProjectWorkspace env =
  currentIOWorkspace env >>= initializeWorkspaceInstructionsFile

-- | Create 'CLAUDE.md' in the given directory without replacing an existing
-- file. Callable without an 'IOEnv' so the CLI's @--init@ flag can initialise
-- a workspace before any LLM credentials are resolved (Issue #118).
initializeWorkspaceInstructionsFile :: FilePath -> IO ProjectInitializationResult
initializeWorkspaceInstructionsFile workspace = do
  let target = workspace </> "CLAUDE.md"
  result <- try (initializeTarget target)
  pure $ case (result :: Either IOError ProjectInitializationResult) of
    Right outcome -> outcome
    Left err -> ProjectInitializationFailed (formatFailure target err)
  where
    initializeTarget target = do
      alreadyFile <- doesFileExist target
      alreadyDirectory <- doesDirectoryExist target
      if alreadyFile
        then pure ProjectAlreadyPresent
        else if alreadyDirectory
          then pure (ProjectInitializationFailed
            ("Could not create " <> T.pack target <> ": path is a directory."))
          else do
            createResult <- try (createExclusiveFile target)
            pure $ case (createResult :: Either IOError ()) of
              Right () -> ProjectInitialized
              Left err
                | isAlreadyExistsError err -> ProjectAlreadyPresent
                | otherwise -> ProjectInitializationFailed (formatFailure target err)

    formatFailure target err =
      "Could not create " <> T.pack target <> ": " <> T.pack (displayException err)

    createExclusiveFile target = do
      bracket
        (openFd target WriteOnly (defaultFileFlags { creat = Just 0o644, exclusive = True }) >>= fdToHandle)
        hClose
        (\handle -> TIO.hPutStr handle claudeInstructionsTemplate)

-- | Static permission and hook configuration threaded from CLI flags and
-- layered settings into the IO interpreter.
data IOEnvPermissions = IOEnvPermissions
  { iopInitialMode :: !PermissionMode
  , iopRules       :: ![PermissionRule]
  , iopHooks       :: !(Map HookEvent [HookHandler])
  }

-- | Open defaults: ask-by-default policy with no rules or hooks configured.
defaultIOEnvPermissions :: IOEnvPermissions
defaultIOEnvPermissions = IOEnvPermissions ModeDefault [] Map.empty

-- | Live permission runtime. Rules and hooks are fixed for the session; the
-- mode is mutable so slash commands can switch enforcement mid-session.
data PermissionRuntime = PermissionRuntime
  { prtMode  :: !(IORef PermissionMode)
  , prtRules :: ![PermissionRule]
  , prtHooks :: !(Map HookEvent [HookHandler])
  }

-- | Runtime environment for executing an agent harness in real IO.
data IOEnv = IOEnv
  { ioManager          :: !Manager
  , ioApiKey           :: !Text
  , ioModel            :: !Text
  , ioWorkspace        :: !FilePath
  , ioCurrentWorkspace :: !(IORef FilePath)
  , ioCurrentWorktree  :: !(IORef (Maybe FilePath))
  , ioVerbose          :: !Bool
  , ioPerms            :: !PermissionRuntime
  , ioEffortLevel      :: !(Maybe EffortLevel)
  , ioResolveAsk       :: Text -> Text -> Text -> IO Bool
  }

-- | Initialize a new 'IOEnv' with a TLS manager and open permission defaults.
newIOEnv :: Text -> Text -> FilePath -> Bool -> IO IOEnv
newIOEnv = newIOEnvWithPermissions defaultIOEnvPermissions

-- | Initialize a new 'IOEnv' with explicit permission mode, rules, and hooks.
newIOEnvWithPermissions
  :: IOEnvPermissions -> Text -> Text -> FilePath -> Bool -> IO IOEnv
newIOEnvWithPermissions perms apiKey model workspace verbose = do
  mgr <- newManager tlsManagerSettings
  modeRef <- newIORef (iopInitialMode perms)
  wsRef <- newIORef workspace
  wtRef <- newIORef Nothing
  pure IOEnv
    { ioManager          = mgr
    , ioApiKey           = apiKey
    , ioModel            = model
    , ioWorkspace        = workspace
    , ioCurrentWorkspace = wsRef
    , ioCurrentWorktree  = wtRef
    , ioVerbose          = verbose
    , ioPerms            = PermissionRuntime modeRef (iopRules perms) (iopHooks perms)
    , ioEffortLevel      = Nothing
    , ioResolveAsk       = \_ _ _ -> pure False
    }

-- | Switch the live permission mode; subsequent tool calls are checked
-- against the new mode.
setIOPermissionMode :: IOEnv -> PermissionMode -> IO ()
setIOPermissionMode env mode = writeIORef (prtMode (ioPerms env)) mode

-- | Read the currently active permission mode.
currentIOPermissionMode :: IOEnv -> IO PermissionMode
currentIOPermissionMode = readIORef . prtMode . ioPerms

-- | Read the currently active workspace directory.
currentIOWorkspace :: IOEnv -> IO FilePath
currentIOWorkspace = readIORef . ioCurrentWorkspace

-- | Read the currently active worktree directory, if inside one.
currentIOWorktree :: IOEnv -> IO (Maybe FilePath)
currentIOWorktree = readIORef . ioCurrentWorktree

-- | Format and print events to the console for CLI observability.
-- With @verbose@ off (@--print@ / @-p@) nothing reaches stdout: the caller
-- prints the formatted result there.
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

  EvDone ans ->
    when verbose $ do
      putStrLn "\n==================== Final Answer ===================="
      TIO.putStrLn ans
      putStrLn "======================================================"

  EvError err ->
    notice ("\n[Agent Error]: " <> T.unpack err)

  EvGoalSet cond ->
    notice ("\n[Goal] Set: " <> T.unpack cond)

  EvGoalEvaluated verdict reason ->
    notice ("\n[Goal] Evaluated: " <> show verdict <> " — " <> T.unpack reason)

  EvGoalEvaluationUsage TokenUsage{..} ->
    when verbose $
      putStrLn ("\n[Goal Evaluator Usage] " <> show tuPromptTokens <> " prompt, "
                <> show tuCompletionTokens <> " completion, "
                <> show tuTotalTokens <> " total tokens")

  EvGoalAchieved cond ->
    notice ("\n[Goal] Achieved: " <> T.unpack cond)

  EvGoalFailed cond reason ->
    notice ("\n[Goal] Failed: " <> T.unpack cond <> " — " <> T.unpack reason)

  EvGoalCleared cond ->
    notice ("\n[Goal] Cleared: " <> T.unpack cond)

  EvGoalBlocked cond ->
    notice ("\n[Goal] No progress detected. Goal still active: " <> T.unpack cond)

  EvPartialResponse delta ->
    when verbose $ TIO.putStr delta

  EvToolCallDelta delta ->
    when verbose $ TIO.putStr delta

  EvPermissionDenied tool reason ->
    notice ("\n[Permission Denied] " <> T.unpack tool <> ": " <> T.unpack reason)

  EvPermissionAsk _ tool _ reason ->
    notice ("\n[Permission Ask] " <> T.unpack tool <> ": " <> T.unpack reason)

  EvHookTriggered hook msg ->
    when verbose $ putStrLn ("\n[Hook " <> T.unpack hook <> "] " <> T.unpack msg)

  EvSessionSaved sid ->
    when verbose $ putStrLn ("\n[Session Saved] " <> T.unpack sid)

  EvNotificationSent msg ->
    notice ("\n[Notification] " <> T.unpack msg)
  where
    -- Quiet runs ('--print' / '-p') reserve stdout for the final answer,
    -- so notices move to stderr instead of disappearing.
    notice = if verbose then putStrLn else hPutStrLn stderr

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

-- | Build the OpenRouter payload for this environment. Effort is taken from
-- the session env so a later model switch does not drop it.
chatRequestFor :: IOEnv -> [Message] -> [ToolDef] -> Maybe Text -> ChatRequest
chatRequestFor IOEnv{..} msgs tools choice = ChatRequest
  { reqModel      = ioModel
  , reqMessages   = msgs
  , reqTools      = tools
  , reqToolChoice = choice
  , reqEffort     = ioEffortLevel
  }

-- | Concrete IO algebra parameterized by an event logger (useful for TUI piping).
ioAlgebraWithLog :: (AgentEvent -> IO ()) -> IOEnv -> AgentAlgebra IO
ioAlgebraWithLog logger env@IOEnv{..} = AgentAlgebra
  { interpPrompt = \msgs tools -> do
      let req = chatRequestFor env msgs tools (Just "auto")
      sendChatCompletion ioManager ioApiKey req

  , interpTool = \call -> do
      case functionName call of
        name | name `elem` ["EnterWorktree", "enter_worktree"] ->
          case parseEnterWorktreeArgs call of
            Left err   -> pure $ ToolError ("Failed to parse EnterWorktree args: " <> T.pack err)
            Right (EnterWorktreeArgs wtName) -> do
              res <- Git.createWorktree ioWorkspace wtName
              case res of
                Left err -> pure $ ToolError err
                Right wtPath -> do
                  writeIORef ioCurrentWorkspace wtPath
                  writeIORef ioCurrentWorktree (Just wtPath)
                  pure $ ToolSuccess ("Created and entered worktree: " <> T.pack wtPath)
        name | name `elem` ["ExitWorktree", "exit_worktree"] -> do
          mWt <- readIORef ioCurrentWorktree
          case mWt of
            Nothing -> pure $ ToolError "Not currently inside a worktree."
            Just _  -> do
              writeIORef ioCurrentWorkspace ioWorkspace
              writeIORef ioCurrentWorktree Nothing
              pure $ ToolSuccess "Exited worktree and restored workspace root."
        name | name `elem` ["EnterPlanMode", "enter_plan_mode"] -> do
          setIOPermissionMode env ModePlan
          currentWs <- readIORef ioCurrentWorkspace
          executeCodingTool currentWs call
        name | name `elem` ["ExitPlanMode", "exit_plan_mode"] -> do
          setIOPermissionMode env ModeDefault
          currentWs <- readIORef ioCurrentWorkspace
          executeCodingTool currentWs call
        _ -> do
          currentWs <- readIORef ioCurrentWorkspace
          executeCodingTool currentWs call

  , interpLog = logger

  , interpEvaluate = \condition transcript -> do
      let evalMsgs = SystemMsg evaluatorSystemPrompt
                   : UserMsg ("Condition: " <> condition <> "\n\nTranscript:\n" <> transcriptToText transcript)
                   : []
          req = chatRequestFor env evalMsgs [] Nothing
      res <- sendChatCompletion ioManager ioApiKey req
      case res of
        Right asstResp -> do
          mapM_ (logger . EvGoalEvaluationUsage) (respUsage asstResp)
          case respContent asstResp of
            Just content -> pure (parseGoalEvaluation content)
            Nothing      -> pure (GoalEvaluation GoalNotYetMet "Empty evaluator response.")
        Left err ->
          pure (GoalEvaluation GoalNotYetMet ("Evaluator error: " <> err))

  , interpCheckPermission = \tool args -> do
      mode <- readIORef prtMode
      let argsVal = fromMaybe Aeson.Null (Aeson.decodeStrict (TE.encodeUtf8 args))
          capability = resolveReadWorkspaceTool (ToolCall "" tool args)
          decision = case capability of
            Just (Right resolved) -> evalPermissionForAuthority mode prtRules (resolvedReadCanonicalName resolved) argsVal (resolvedReadAuthority resolved)
            _ -> evalPermission mode prtRules tool argsVal
      case decision of
        PermAllow      -> pure True
        PermDeny _     -> pure False
        PermAsk reason -> ioResolveAsk tool args reason

  , interpRunHook = \ev payload -> do
      currentWs <- readIORef ioCurrentWorkspace
      let (mTool, payloadVal) = splitHookPayload payload
      executeHooks currentWs prtHooks ev mTool payloadVal
  , interpSaveSession = \sinfo -> do
      currentWs <- readIORef ioCurrentWorkspace
      Sessions.saveSession (currentWs </> ".agents" </> "sessions") sinfo []
      pure (siId sinfo)
  , interpLoadSession = \sid -> do
      currentWs <- readIORef ioCurrentWorkspace
      mRes <- Sessions.loadSession (currentWs </> ".agents" </> "sessions") sid
      case mRes of
        Just _  -> pure (fmap fst mRes)
        Nothing -> do
          mResLegacy <- Sessions.loadSession (currentWs </> ".agent" </> "sessions") sid
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
  , interpGitStatus = do
      currentWs <- readIORef ioCurrentWorkspace
      Git.getGitStatus currentWs
  , interpCreateWorktree = \name -> do
      res <- Git.createWorktree ioWorkspace name
      case res of
        Right p -> pure p
        Left err -> pure (T.unpack err)
  , interpEnterWorktree = \path -> do
      writeIORef ioCurrentWorkspace path
      writeIORef ioCurrentWorktree (Just path)
  , interpExitWorktree = do
      writeIORef ioCurrentWorkspace ioWorkspace
      writeIORef ioCurrentWorktree Nothing
  , interpLoadMemory = \path -> do
      currentWs <- readIORef ioCurrentWorkspace
      T.unlines <$> loadHierarchicalMemory currentWs path
  , interpResolveImport = \path -> do
      currentWs <- readIORef ioCurrentWorkspace
      resolveMemoryImports currentWs 4 path
  }
  where
    PermissionRuntime{..} = ioPerms

    transcriptToText = T.unlines . map messageToText

    -- Core delivers hook payloads as "<tool> <json-or-text>"; split the tool
    -- name off and pass the remainder as JSON when it parses as such.
    splitHookPayload :: Text -> (Maybe Text, Aeson.Value)
    splitHookPayload payload =
      case T.breakOn " " payload of
        (tool, rest) | not (T.null tool), not (T.null rest) ->
          let restTxt = T.strip rest
          in ( Just tool
             , fromMaybe (Aeson.String restTxt)
                 (Aeson.decodeStrict (TE.encodeUtf8 restTxt))
             )
        _ -> (Nothing, Aeson.String payload)

    messageToText = \case
      SystemMsg c    -> "[System] " <> c
      UserMsg c      -> "[User] " <> c
      AssistantMsg mc _ -> "[Assistant] " <> fromMaybe "" mc
      ToolMsg _ name c -> "[Tool " <> name <> "] " <> c

-- | Run an 'AgentProgram' using real OpenRouter API and local filesystem.
runIO :: IOEnv -> AgentProgram a -> IO a
runIO env prog = foldAgentProgram (ioAlgebra env) prog
