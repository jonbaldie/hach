{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.Interpreter.Pure
  ( MockEnv(..)
  , emptyMockEnv
  , runPure
  , pureAlgebra
  ) where

import Agent.Core
import Agent.Tools
import Agent.Types
import Control.Monad.IO.Class ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory)

-- | State environment for pure simulation and testing of the agent.
data MockEnv = MockEnv
  { mockLLMSteps          :: ![[Message] -> [ToolDef] -> AssistantResponse]
  , mockFiles             :: !(Map FilePath Text)
  , mockCommandOutputs    :: !(Map Text (Int, Text, Text)) -- ^ (exitCode, stdout, stderr)
  , mockEvents            :: ![AgentEvent]
  , mockGoalEvaluations   :: ![Text -> [Message] -> GoalEvaluation]
  }

-- | An initial empty mock environment.
emptyMockEnv :: MockEnv
emptyMockEnv = MockEnv
  { mockLLMSteps        = []
  , mockFiles           = Map.empty
  , mockCommandOutputs  = Map.empty
  , mockEvents          = []
  , mockGoalEvaluations = []
  }

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
          pure $ AssistantResponse (Just "Mock finished.") [] Nothing

  , interpTool = \call -> do
      case functionName call of
        "read_file" ->
          case parseReadFileArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (ReadFileArgs path) -> do
              env <- getEnv
              case Map.lookup path (mockFiles env) of
                Just content -> pure $ ToolSuccess content
                Nothing      -> pure $ ToolError ("File not found: " <> T.pack path)

        "write_file" ->
          case parseWriteFileArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (WriteFileArgs path content) -> do
              modifyEnv $ \env ->
                env { mockFiles = Map.insert path content (mockFiles env) }
              pure $ ToolSuccess ("Wrote " <> T.pack (show (T.length content)) <> " characters to " <> T.pack path)

        "run_command" ->
          case parseRunCommandArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (RunCommandArgs cmd) -> do
              env <- getEnv
              let (code, out, errOut) = fromMaybe (0, "", "") (Map.lookup cmd (mockCommandOutputs env))
                  summary = T.unlines
                    [ "Exit Code: " <> T.pack (show code)
                    , "STDOUT:\n" <> if T.null out then "(empty)" else out
                    , "STDERR:\n" <> if T.null errOut then "(empty)" else errOut
                    ]
              pure $ ToolSuccess summary

        "list_dir" ->
          case parseListDirArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (ListDirArgs path) -> do
              env <- getEnv
              let prefix = if path == "." || path == "./" then "" else path
                  keys = Map.keys (mockFiles env)
                  matching = filter (\k -> if null prefix then True else takeDirectory k == prefix) keys
              pure $ ToolSuccess (T.unlines (map T.pack matching))

        "replace_file_content" ->
          case parseReplaceFileContentArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (ReplaceFileContentArgs path oldContent newContent) -> do
              env <- getEnv
              case Map.lookup path (mockFiles env) of
                Nothing -> pure $ ToolError ("File not found: " <> T.pack path)
                Just currentText ->
                  let count = T.count oldContent currentText
                  in if count == 0
                    then pure $ ToolError ("Target content not found in '" <> T.pack path <> "'.")
                    else if count > 1
                      then pure $ ToolError ("Target content found " <> T.pack (show count) <> " times in '" <> T.pack path <> "'; replacement requires a unique match.")
                      else do
                        let updated = T.replace oldContent newContent currentText
                        modifyEnv $ \e -> e { mockFiles = Map.insert path updated (mockFiles e) }
                        pure $ ToolSuccess ("Successfully replaced content in " <> T.pack path <> ".")

        "find_files" ->
          case parseFindFilesArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (FindFilesArgs pat _) -> do
              env <- getEnv
              let keys = Map.keys (mockFiles env)
                  matching = filter (\k -> pat `T.isInfixOf` T.pack k || ("*" `T.isInfixOf` pat && not (null keys))) keys
              pure $ ToolSuccess (T.unlines (map T.pack matching))

        "grep_search" ->
          case parseGrepSearchArgs call of
            Left err -> pure $ ToolError ("Parse error: " <> T.pack err)
            Right (GrepSearchArgs query _ caseSens) -> do
              env <- getEnv
              let matches = concatMap (searchInFile query caseSens) (Map.toList (mockFiles env))
              pure $ ToolSuccess (T.unlines matches)

        unknown ->
          pure $ ToolError ("Unknown mock tool: " <> unknown)

  , interpLog = \ev ->
      modifyEnv $ \env -> env { mockEvents = mockEvents env ++ [ev] }

  , interpEvaluate = \cond msgs -> do
      env <- getEnv
      case mockGoalEvaluations env of
        (evalFn : rest) -> do
          putEnv env { mockGoalEvaluations = rest }
          pure (evalFn cond msgs)
        [] ->
          pure $ GoalEvaluation GoalNotYetMet "No evaluator steps left; defaulting to not yet met."
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
