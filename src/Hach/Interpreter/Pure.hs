{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Interpreter.Pure
  ( MockEnv(..)
  , emptyMockEnv
  , runPure
  , pureAlgebra
  , pureStep
  ) where

import Hach.Core
import Hach.Tools
import Hach.Types
import Control.Monad.IO.Class ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory)

-- | State environment for pure simulation and testing of the agent.
data MockEnv = MockEnv
  { mockLLMSteps          :: ![[Message] -> [ToolDef] -> Either Text AssistantResponse]
  , mockFiles             :: !(Map FilePath Text)
  , mockCommandOutputs    :: !(Map Text (Int, Text, Text)) -- ^ (exitCode, stdout, stderr)
  , mockEvents            :: ![AgentEvent]
  , mockGoalEvaluations   :: ![Text -> [Message] -> GoalEvaluation]
  , mockGoalEvaluationUsages :: ![Maybe TokenUsage]
  , mockPermissions       :: !(Text -> Text -> Bool)
  , mockHooks             :: !(HookEvent -> Text -> HookResult)
  , mockSavedSessions     :: !(Map SessionId SessionInfo)
  , mockRunningAgents     :: ![AgentInfo]
  , mockMcpTools          :: ![ToolDef]
  , mockMcpResults        :: !(Map (Text, Text) ToolResult)
  , mockTasks             :: !(Map TaskId TaskInfo)
  , mockNotifications     :: ![(Text, Text)]
  , mockGitStatus         :: !GitStatusInfo
  , mockWorktrees         :: ![FilePath]
  , mockCurrentWorktree   :: !(Maybe FilePath)
  }

-- | An initial empty mock environment.
emptyMockEnv :: MockEnv
emptyMockEnv = MockEnv
  { mockLLMSteps        = []
  , mockFiles           = Map.empty
  , mockCommandOutputs  = Map.empty
  , mockEvents          = []
  , mockGoalEvaluations = []
  , mockGoalEvaluationUsages = []
  , mockPermissions     = \_ _ -> True
  , mockHooks           = \_ _ -> HookResult Nothing Nothing Nothing Nothing
  , mockSavedSessions   = Map.empty
  , mockRunningAgents   = []
  , mockMcpTools        = []
  , mockMcpResults      = Map.empty
  , mockTasks           = Map.empty
  , mockNotifications   = []
  , mockGitStatus       = GitStatusInfo "main" True [] []
  , mockWorktrees       = []
  , mockCurrentWorktree = Nothing
  }

-- | Helper to lift a successful pure assistant response function into 'Either Text AssistantResponse'.
pureStep :: ([Message] -> [ToolDef] -> AssistantResponse) -> ([Message] -> [ToolDef] -> Either Text AssistantResponse)
pureStep f msgs tools = Right (f msgs tools)

-- Simple state monad for pure interpretation
newtype PureM a = PureM { runPureM :: MockEnv -> (a, MockEnv) }

instance Functor PureM where
  fmap f (PureM m) = PureM $ \s -> let (a, s') = m s in (f a, s')

instance Applicative PureM where
  pure a = PureM $ \s -> (a, s)
  PureM mf <*> PureM mx = PureM $ \s ->
    let (f, s1) = mf s
        (x, s2) = mx s1
    in (f x, s2)

instance Monad PureM where
  PureM m >>= f = PureM $ \s ->
    let (a, s1) = m s
    in runPureM (f a) s1

getEnv :: PureM MockEnv
getEnv = PureM $ \s -> (s, s)

putEnv :: MockEnv -> PureM ()
putEnv s = PureM $ \_ -> ((), s)

modifyEnv :: (MockEnv -> MockEnv) -> PureM ()
modifyEnv f = PureM $ \s -> ((), f s)

-- | Pure algebra interpreting agent operations against 'MockEnv'.
pureAlgebra :: AgentAlgebra PureM
pureAlgebra = AgentAlgebra
  { interpPrompt = \msgs tools -> do
      env <- getEnv
      case mockLLMSteps env of
        (stepFn : rest) -> do
          putEnv env { mockLLMSteps = rest }
          pure (stepFn msgs tools)
        [] ->
          pure $ Right (AssistantResponse (Just "Mock finished.") [] Nothing)

  , interpTool = \call -> do
      case functionName call of
        name | name `elem` ["read_file", "ReadFile"] ->
          case parseReadFileArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (ReadFileArgs path) -> do
              env <- getEnv
              case Map.lookup path (mockFiles env) of
                Just content -> pure $ ToolSuccess content
                Nothing      -> pure $ ToolError ("File not found: " <> T.pack path)

        name | name `elem` ["write_file", "WriteFile"] ->
          case parseWriteFileArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (WriteFileArgs path content) -> do
              modifyEnv $ \env ->
                env { mockFiles = Map.insert path content (mockFiles env) }
              pure $ ToolSuccess ("Wrote " <> T.pack (show (T.length content)) <> " characters to " <> T.pack path)

        name | name `elem` ["run_command", "Bash", "bash"] ->
          case parseBashArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (BashArgs cmd _) -> do
              env <- getEnv
              let (code, out, errOut) = fromMaybe (0, "", "") (Map.lookup cmd (mockCommandOutputs env))
                  summary = T.unlines
                    [ "Exit Code: " <> T.pack (show code)
                    , "STDOUT:\n" <> if T.null out then "(empty)" else out
                    , "STDERR:\n" <> if T.null errOut then "(empty)" else errOut
                    ]
              pure $ ToolSuccess summary

        name | name `elem` ["list_dir", "ListDir"] ->
          case parseListDirArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (ListDirArgs path) -> do
              env <- getEnv
              let prefix = if path == "." || path == "./" then "" else path
                  keys = Map.keys (mockFiles env)
                  matching = filter (\k -> if null prefix then True else takeDirectory k == prefix) keys
              pure $ ToolSuccess (T.unlines (map T.pack matching))

        name | name `elem` ["replace_file_content", "Edit", "edit"] ->
          case parseEditArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (EditArgs path oldContent newContent) -> do
              env <- getEnv
              case Map.lookup path (mockFiles env) of
                Nothing -> pure $ ToolError ("File not found: " <> T.pack path)
                Just currentText ->
                  if T.null oldContent
                    then pure $ ToolError "The 'old_content' parameter cannot be empty."
                    else
                      let count = countOccurrencesUpToTwo oldContent currentText
                      in if count == 0
                        then pure $ ToolError ("Target content not found in '" <> T.pack path <> "'.")
                        else if count > 1
                          then pure $ ToolError ("Target content found " <> T.pack (show count) <> " times in '" <> T.pack path <> "'; replacement requires a unique match.")
                          else do
                            let updated = T.replace oldContent newContent currentText
                            modifyEnv $ \e -> e { mockFiles = Map.insert path updated (mockFiles e) }
                            pure $ ToolSuccess ("Successfully replaced content in " <> T.pack path <> ".")

        name | name `elem` ["find_files", "Glob", "glob"] ->
          case parseGlobArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (GlobArgs pat _) -> do
              env <- getEnv
              let keys = Map.keys (mockFiles env)
                  matching = filter (\k -> pat `T.isInfixOf` T.pack k || ("*" `T.isInfixOf` pat && not (null keys))) keys
              pure $ ToolSuccess (T.unlines (map T.pack matching))

        name | name `elem` ["grep_search", "Grep", "grep"] ->
          case parseGrepArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (GrepArgs query _ caseSens) -> do
              env <- getEnv
              let matches = concatMap (searchInFile query caseSens) (Map.toList (mockFiles env))
              pure $ ToolSuccess (T.unlines matches)

        unknown ->
          pure $ ToolError ("Unknown mock tool: " <> unknown)

  , interpLog = \ev ->
      modifyEnv $ \env -> env { mockEvents = mockEvents env ++ [ev] }

  , interpEvaluate = \cond msgs -> do
      env <- getEnv
      let (mUsage, restUsages) = case mockGoalEvaluationUsages env of
            (u : rest) -> (u, rest)
            []         -> (Nothing, [])
          (evalRes, restEvals) = case mockGoalEvaluations env of
            (evalFn : rest) -> (evalFn cond msgs, rest)
            []              -> (GoalEvaluation GoalNotYetMet "No evaluator steps left; defaulting to not yet met.", [])
          usageEvents = case mUsage of
            Just u  -> [EvGoalEvaluationUsage u]
            Nothing -> []
      putEnv env
        { mockGoalEvaluationUsages = restUsages
        , mockGoalEvaluations      = restEvals
        , mockEvents               = mockEvents env ++ usageEvents
        }
      pure evalRes

  , interpCheckPermission = \tool args -> do
      env <- getEnv
      pure (mockPermissions env tool args)

  , interpRunHook = \ev payload -> do
      env <- getEnv
      pure (mockHooks env ev payload)

  , interpSaveSession = \sinfo -> do
      modifyEnv $ \e -> e { mockSavedSessions = Map.insert (siId sinfo) sinfo (mockSavedSessions e) }
      pure (siId sinfo)

  , interpLoadSession = \sid -> do
      env <- getEnv
      pure (Map.lookup sid (mockSavedSessions env))

  , interpSpawnAgent = \role desc -> do
      let aid = AgentId ("agent_" <> role)
      modifyEnv $ \e -> e { mockRunningAgents = mockRunningAgents e ++ [AgentInfo aid role desc "idle"] }
      pure aid

  , interpSendMessage = \aid msg ->
      pure ("Delivered to " <> unAgentId aid <> ": " <> msg)

  , interpListAgents = do
      env <- getEnv
      pure (mockRunningAgents env)

  , interpCallMcpTool = \srv tool _args -> do
      env <- getEnv
      pure (fromMaybe (ToolSuccess "mcp ok") (Map.lookup (srv, tool) (mockMcpResults env)))

  , interpListMcpTools = do
      env <- getEnv
      pure (mockMcpTools env)

  , interpRunBackground = \cmd -> do
      let tid = TaskId "task_bg"
      modifyEnv $ \e -> e { mockTasks = Map.insert tid (TaskInfo tid cmd "running" "") (mockTasks e) }
      pure tid

  , interpGetTaskOutput = \tid -> do
      env <- getEnv
      pure (fromMaybe (TaskInfo tid "" "pending" "") (Map.lookup tid (mockTasks env)))

  , interpStopTask = \tid -> do
      modifyEnv $ \e -> e { mockTasks = Map.adjust (\t -> t { tiStatus = "stopped" }) tid (mockTasks e) }
      pure True

  , interpSendNotification = \title body ->
      modifyEnv $ \e -> e { mockNotifications = mockNotifications e ++ [(title, body)] }

  , interpGitStatus = do
      env <- getEnv
      pure (mockGitStatus env)

  , interpCreateWorktree = \name -> do
      let p = ".agents/worktrees/" <> T.unpack name
      modifyEnv $ \e -> e { mockWorktrees = mockWorktrees e ++ [p] }
      pure p

  , interpEnterWorktree = \path ->
      modifyEnv $ \e -> e { mockCurrentWorktree = Just path }

  , interpExitWorktree =
      modifyEnv $ \e -> e { mockCurrentWorktree = Nothing }

  , interpLoadMemory = \path -> do
      env <- getEnv
      pure (fromMaybe "" (Map.lookup path (mockFiles env)))

  , interpResolveImport = \path -> do
      env <- getEnv
      pure (fromMaybe "" (Map.lookup path (mockFiles env)))
  }

searchInFile :: Text -> Bool -> (FilePath, Text) -> [Text]
searchInFile q cs (fp, content) =
  let ls = zip [1 :: Int ..] (T.lines content)
      check (_, line) =
        if cs then q `T.isInfixOf` line else T.toLower q `T.isInfixOf` T.toLower line
  in [ T.pack fp <> ":" <> T.pack (show lineNum) <> ": " <> line | (lineNum, line) <- filter check ls ]

-- | Run an 'AgentProgram' purely with a 'MockEnv'.
runPure :: MockEnv -> AgentProgram a -> (a, MockEnv)
runPure initialEnv prog = runPureM (foldAgentProgram pureAlgebra prog) initialEnv
