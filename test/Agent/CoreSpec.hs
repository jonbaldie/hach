{-# LANGUAGE OverloadedStrings #-}

module Agent.CoreSpec (spec) where

import Agent.Core
import Agent.Interpreter.Pure
import Agent.Tools
import Agent.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

spec :: Spec
spec = do
  let baseConfig = AgentConfig
        { cfgModel = "test-model"
        , cfgSystemPrompt = Just "You are an assistant."
        , cfgMaxTurns = 20
        }

  describe "agentLoop with Pure Interpreter" $ do
    it "completes immediately when model returns direct answer" $ do
      let step1 _ _ = Right $ AssistantResponse (Just "Hello world!") [] Nothing
          env = emptyMockEnv { mockLLMSteps = [step1] }
          initHist = [UserMsg "Hi"]
          ((result, finalHist), endEnv) = runPure env (agentLoop baseConfig [] initHist)

      result `shouldBe` AgentCompleted "Hello world!"
      length finalHist `shouldBe` 2
      last finalHist `shouldBe` AssistantMsg (Just "Hello world!") []
      -- Verify event logging
      mockEvents endEnv `shouldContain` [EvDone "Hello world!"]

    it "executes a tool call and passes observation back to model in next turn" $ do
      let toolCall1 = ToolCall
            { callId = "call_1"
            , functionName = "read_file"
            , callArgsRaw = "{\"path\":\"hello.txt\"}"
            }
          -- Turn 1: model calls read_file
          step1 _ _ = Right $ AssistantResponse Nothing [toolCall1] Nothing
          -- Turn 2: model sees file content and completes
          step2 hist _ =
            case last hist of
              ToolMsg "call_1" "read_file" content ->
                Right $ AssistantResponse (Just ("The file says: " <> content)) [] Nothing
              _ ->
                Right $ AssistantResponse (Just "Failed to get tool output") [] Nothing

          env = emptyMockEnv
            { mockLLMSteps = [step1, step2]
            , mockFiles = Map.fromList [("hello.txt", "Functional Pearl")]
            }
          initHist = [UserMsg "Read hello.txt"]
          ((result, finalHist), endEnv) = runPure env (agentLoop baseConfig allToolDefs initHist)

      result `shouldBe` AgentCompleted "The file says: Functional Pearl"
      -- History should be: [UserMsg, AssistantMsg (with tool_calls), ToolMsg (result), AssistantMsg (final)]
      length finalHist `shouldBe` 4
      case finalHist of
        [ UserMsg _
          , AssistantMsg Nothing [tc]
          , ToolMsg "call_1" "read_file" "Functional Pearl"
          , AssistantMsg (Just ans) []
          ] -> do
            callId tc `shouldBe` "call_1"
            ans `shouldBe` "The file says: Functional Pearl"
        _ -> expectationFailure ("Unexpected final history shape: " <> show finalHist)

      -- Verify events contain tool execution
      mockEvents endEnv `shouldContain` [EvToolCall "read_file" "{\"path\":\"hello.txt\"}"]

    it "supports writing files purely" $ do
      let writeCall = ToolCall
            { callId = "call_write"
            , functionName = "write_file"
            , callArgsRaw = "{\"path\":\"out.txt\",\"content\":\"Pearls in Haskell\"}"
            }
          step1 _ _ = Right $ AssistantResponse Nothing [writeCall] Nothing
          step2 _ _ = Right $ AssistantResponse (Just "Wrote successfully!") [] Nothing
          env = emptyMockEnv { mockLLMSteps = [step1, step2] }
          initHist = [UserMsg "Write out.txt"]
          ((result, _), endEnv) = runPure env (agentLoop baseConfig allToolDefs initHist)

      result `shouldBe` AgentCompleted "Wrote successfully!"
      Map.lookup "out.txt" (mockFiles endEnv) `shouldBe` Just "Pearls in Haskell"

    it "propagates token usage metadata in EvLLMResponse" $ do
      let usage = TokenUsage 150 40 190
          step1 _ _ = Right $ AssistantResponse (Just "Tokens measured") [] (Just usage)
          env = emptyMockEnv { mockLLMSteps = [step1] }
          initHist = [UserMsg "Check tokens"]
          (_, endEnv) = runPure env (agentLoop baseConfig [] initHist)

      mockEvents endEnv `shouldContain` [EvLLMResponse (Just "Tokens measured") [] (Just usage)]

    it "terminates when maximum turns are reached" $ do
      let loopConfig = baseConfig { cfgMaxTurns = 2 }
      let infiniteToolCall = ToolCall
            { callId = "loop_call"
            , functionName = "read_file"
            , callArgsRaw = "{\"path\":\"hello.txt\"}"
            }
          stepLoop _ _ = Right $ AssistantResponse Nothing [infiniteToolCall] Nothing
          -- Model keeps calling the tool infinitely
          env = emptyMockEnv
            { mockLLMSteps = repeat stepLoop
            , mockFiles = Map.fromList [("hello.txt", "data")]
            }
          ((result, _), _) = runPure env (agentLoop loopConfig allToolDefs [UserMsg "Run forever"])

      result `shouldBe` AgentMaxTurnsReached 2

    it "terminates with AgentFailed when model returns an error" $ do
      let stepError _ _ = Left "401 Unauthorized"
          env = emptyMockEnv { mockLLMSteps = [stepError] }
          initHist = [UserMsg "Fail please"]
          ((result, finalHist), endEnv) = runPure env (agentLoop baseConfig [] initHist)

      result `shouldBe` AgentFailed "401 Unauthorized"
      finalHist `shouldBe` initHist
      mockEvents endEnv `shouldContain` [EvError "401 Unauthorized"]

  describe "goalLoop with Pure Interpreter" $ do
    let goalConfig = baseConfig { cfgMaxTurns = 20 }
        condition = "All tests pass"

    it "continues when evaluator says not yet met, then stops when met" $ do
      let step1 _ _ = Right $ AssistantResponse (Just "Working on it.") [] Nothing
          step2 _ _ = Right $ AssistantResponse (Just "All tests pass now.") [] Nothing
          eval1 _ _ = GoalEvaluation GoalNotYetMet "Tests not run yet."
          eval2 _ _ = GoalEvaluation GoalMet "Tests pass."
          env = emptyMockEnv
            { mockLLMSteps = [step1, step2]
            , mockGoalEvaluations = [eval1, eval2]
            }
          initHist = [UserMsg condition]
          ((result, _, goalState), endEnv) =
            runPure env (goalLoop goalConfig [] condition defaultBlockCap initHist)

      result `shouldBe` AgentCompleted "All tests pass now."
      gsStatus goalState `shouldBe` GoalAchieved
      gsTurnCount goalState `shouldBe` 2
      mockEvents endEnv `shouldContain` [EvGoalAchieved condition]

    it "stops and marks goal failed when evaluator says impossible" $ do
      let step1 _ _ = Right $ AssistantResponse (Just "I cannot do this.") [] Nothing
          eval1 _ _ = GoalEvaluation GoalImpossible "The test framework is missing."
          env = emptyMockEnv
            { mockLLMSteps = [step1]
            , mockGoalEvaluations = [eval1]
            }
          initHist = [UserMsg condition]
          ((result, _, goalState), endEnv) =
            runPure env (goalLoop goalConfig [] condition defaultBlockCap initHist)

      result `shouldBe` AgentCompleted "I cannot do this."
      gsStatus goalState `shouldBe` GoalFailed
      gsTurnCount goalState `shouldBe` 1
      mockEvents endEnv `shouldContain` [EvGoalFailed condition "The test framework is missing."]

    it "stops with block cap warning when agent makes no progress for consecutive turns" $ do
      let step _ _ = Right $ AssistantResponse (Just "Thinking...") [] Nothing
          eval _ _ = GoalEvaluation GoalNotYetMet "Not done yet."
          cap = 2
          env = emptyMockEnv
            { mockLLMSteps = repeat step
            , mockGoalEvaluations = repeat eval
            }
          initHist = [UserMsg condition]
          ((result, _, goalState), endEnv) =
            runPure env (goalLoop goalConfig [] condition cap initHist)

      result `shouldBe` AgentCompleted "Thinking..."
      gsStatus goalState `shouldBe` GoalActive
      gsNoProgressCount goalState `shouldBe` cap
      mockEvents endEnv `shouldContain` [EvGoalBlocked condition]

    it "resets no-progress counter when agent uses tools between completions" $ do
      let toolCall = ToolCall
            { callId = "call_1"
            , functionName = "read_file"
            , callArgsRaw = "{\"path\":\"hello.txt\"}"
            }
          -- Turn 1: agent completes without tools (no progress)
          step1 _ _ = Right $ AssistantResponse (Just "Thinking.") [] Nothing
          -- Turn 2: agent calls a tool (progress — resets counter)
          step2 _ _ = Right $ AssistantResponse Nothing [toolCall] Nothing
          -- Turn 3: agent completes without tools (no progress again)
          step3 _ _ = Right $ AssistantResponse (Just "Done reading.") [] Nothing
          -- Turn 4: agent completes without tools, goal met
          step4 _ _ = Right $ AssistantResponse (Just "All done.") [] Nothing
          eval1 _ _ = GoalEvaluation GoalNotYetMet "Keep going."
          eval2 _ _ = GoalEvaluation GoalNotYetMet "Almost there."
          eval3 _ _ = GoalEvaluation GoalMet "Done."
          cap = 2
          env = emptyMockEnv
            { mockLLMSteps = [step1, step2, step3, step4]
            , mockGoalEvaluations = [eval1, eval2, eval3]
            , mockFiles = Map.fromList [("hello.txt", "data")]
            }
          initHist = [UserMsg condition]
          ((result, _, goalState), _) =
            runPure env (goalLoop goalConfig allToolDefs condition cap initHist)

      -- Without the tool call in turn 2 resetting the counter, turns 1 and 3
      -- would hit cap=2 and block.  With the reset, turn 3 only reaches
      -- no-progress=1, so turn 4 runs and the evaluator says met.
      result `shouldBe` AgentCompleted "All done."
      gsStatus goalState `shouldBe` GoalAchieved

    it "clears the goal when a turn fails with an authentication error" $ do
      let step1 _ _ = Right $ AssistantResponse (Just "[API Error]: 401 Unauthorized") [] Nothing
          env = emptyMockEnv
            { mockLLMSteps = [step1]
            , mockGoalEvaluations = []
            }
          initHist = [UserMsg condition]
          ((result, _, goalState), endEnv) =
            runPure env (goalLoop goalConfig [] condition defaultBlockCap initHist)

      result `shouldBe` AgentCompleted "[API Error]: 401 Unauthorized"
      gsStatus goalState `shouldBe` GoalFailed
      mockEvents endEnv `shouldContain`
        [EvGoalFailed condition "[API Error]: 401 Unauthorized"]

    it "clears the goal when a turn fails with a credit balance error" $ do
      let step1 _ _ = Right $ AssistantResponse (Just "[API Error]: 402 Payment required, credit balance exhausted") [] Nothing
          env = emptyMockEnv
            { mockLLMSteps = [step1]
            , mockGoalEvaluations = []
            }
          initHist = [UserMsg condition]
          ((_, _, goalState), _) =
            runPure env (goalLoop goalConfig [] condition defaultBlockCap initHist)

      gsStatus goalState `shouldBe` GoalFailed

    it "keeps the goal active when a turn fails with a transient error" $ do
      let step1 _ _ = Right $ AssistantResponse (Just "[API Error]: 429 Too many requests") [] Nothing
          env = emptyMockEnv
            { mockLLMSteps = [step1]
            , mockGoalEvaluations = []
            }
          initHist = [UserMsg condition]
          ((_, _, goalState), _) =
            runPure env (goalLoop goalConfig [] condition defaultBlockCap initHist)

      gsStatus goalState `shouldBe` GoalActive

    it "logs EvGoalSet when the goal loop starts" $ do
      let step1 _ _ = Right $ AssistantResponse (Just "Done.") [] Nothing
          eval1 _ _ = GoalEvaluation GoalMet "Done."
          env = emptyMockEnv
            { mockLLMSteps = [step1]
            , mockGoalEvaluations = [eval1]
            }
          initHist = [UserMsg condition]
          (_, endEnv) =
            runPure env (goalLoop goalConfig [] condition defaultBlockCap initHist)

      mockEvents endEnv `shouldContain` [EvGoalSet condition]

    it "logs EvGoalEvaluated for each evaluation" $ do
      let step1 _ _ = Right $ AssistantResponse (Just "Working.") [] Nothing
          eval1 _ _ = GoalEvaluation GoalNotYetMet "Not done."
          eval2 _ _ = GoalEvaluation GoalMet "Done."
          step2 _ _ = Right $ AssistantResponse (Just "Done.") [] Nothing
          env = emptyMockEnv
            { mockLLMSteps = [step1, step2]
            , mockGoalEvaluations = [eval1, eval2]
            }
          initHist = [UserMsg condition]
          (_, endEnv) =
            runPure env (goalLoop goalConfig [] condition defaultBlockCap initHist)

      mockEvents endEnv `shouldContain` [EvGoalEvaluated GoalNotYetMet "Not done."]
      mockEvents endEnv `shouldContain` [EvGoalEvaluated GoalMet "Done."]

    it "injects evaluator reason as guidance for the next turn" $ do
      let step1 _ _ = Right $ AssistantResponse (Just "Working.") [] Nothing
          step2 hist _ =
            case last hist of
              UserMsg guidance ->
                Right $ AssistantResponse (Just ("Received: " <> guidance)) [] Nothing
              _ ->
                Right $ AssistantResponse (Just "No guidance received.") [] Nothing
          eval1 _ _ = GoalEvaluation GoalNotYetMet "Run the tests."
          eval2 _ _ = GoalEvaluation GoalMet "Tests pass."
          env = emptyMockEnv
            { mockLLMSteps = [step1, step2]
            , mockGoalEvaluations = [eval1, eval2]
            }
          initHist = [UserMsg condition]
          ((result, finalHist, _), _) =
            runPure env (goalLoop goalConfig [] condition defaultBlockCap initHist)

      result `shouldBe` AgentCompleted "Received: Goal not yet met. Run the tests. Continue working toward: All tests pass"
      -- The guidance message should be in the history
      finalHist `shouldSatisfy` \h ->
        any (\case UserMsg m -> "Run the tests." `T.isInfixOf` m; _ -> False) h

  describe "GoalEvaluation JSON Parsing" $ do
    it "parses a valid met verdict" $ do
      let json = "{\"verdict\":\"met\",\"reason\":\"Tests pass.\"}"
      case Aeson.decodeStrict (TE.encodeUtf8 json) of
        Just (GoalEvaluation v r) -> do
          v `shouldBe` GoalMet
          r `shouldBe` "Tests pass."
        Nothing -> expectationFailure "Failed to parse GoalEvaluation"

    it "parses a valid not_yet_met verdict" $ do
      let json = "{\"verdict\":\"not_yet_met\",\"reason\":\"Still working.\"}"
      case Aeson.decodeStrict (TE.encodeUtf8 json) of
        Just (GoalEvaluation v _) -> v `shouldBe` GoalNotYetMet
        Nothing -> expectationFailure "Failed to parse GoalEvaluation"

    it "parses a valid impossible verdict" $ do
      let json = "{\"verdict\":\"impossible\",\"reason\":\"Missing framework.\"}"
      case Aeson.decodeStrict (TE.encodeUtf8 json) of
        Just (GoalEvaluation v _) -> v `shouldBe` GoalImpossible
        Nothing -> expectationFailure "Failed to parse GoalEvaluation"

    it "parses with a missing reason field" $ do
      let json = "{\"verdict\":\"met\"}"
      case Aeson.decodeStrict (TE.encodeUtf8 json) of
        Just (GoalEvaluation v r) -> do
          v `shouldBe` GoalMet
          r `shouldBe` ""
        Nothing -> expectationFailure "Failed to parse GoalEvaluation"

    it "fails on an unknown verdict" $ do
      let json = "{\"verdict\":\"maybe\",\"reason\":\"Unsure.\"}"
      Aeson.decodeStrict (TE.encodeUtf8 json :: BS.ByteString)
        `shouldSatisfy` \case
          Just (_ :: GoalEvaluation) -> False
          Nothing -> True

    it "ToJSON/FromJSON round-trips for GoalVerdict" $ do
      Aeson.encode GoalMet `shouldBe` "\"met\""
      Aeson.encode GoalNotYetMet `shouldBe` "\"not_yet_met\""
      Aeson.encode GoalImpossible `shouldBe` "\"impossible\""
      Aeson.decodeStrict "\"met\"" `shouldBe` Just GoalMet
      Aeson.decodeStrict "\"not_yet_met\"" `shouldBe` Just GoalNotYetMet
      Aeson.decodeStrict "\"impossible\"" `shouldBe` Just GoalImpossible

  describe "Feature Gap Closure AgentF Operations" $ do
    it "saves and loads session through AgentF" $ do
      let sinfo = SessionInfo "sess-1" "2026-09-06" "claude-3-5-sonnet" 3 0.05
          prog = do
            sid <- saveSession sinfo
            loadSession sid
          (loaded, endEnv) = runPure emptyMockEnv prog
      loaded `shouldBe` Just sinfo
      Map.lookup "sess-1" (mockSavedSessions endEnv) `shouldBe` Just sinfo

    it "spawns agents and manages agent messaging" $ do
      let prog = do
            aid <- spawnAgent "researcher" "Conducts codebase research"
            msg <- sendMessageToAgent aid "Check tests"
            agents <- listRunningAgents
            pure (aid, msg, agents)
          ((aid, msg, agents), _) = runPure emptyMockEnv prog
      aid `shouldBe` AgentId "agent_researcher"
      msg `shouldBe` "Delivered to agent_researcher: Check tests"
      length agents `shouldBe` 1

    it "handles git status and worktrees purely" $ do
      let prog = do
            st <- gitStatus
            wtPath <- createWorktree "feat/branch"
            enterWorktree wtPath
            exitWorktree
            pure (st, wtPath)
          ((st, wtPath), endEnv) = runPure emptyMockEnv prog
      gsiBranch st `shouldBe` "main"
      wtPath `shouldBe` ".agents/worktrees/feat/branch"
      mockWorktrees endEnv `shouldContain` [wtPath]

    it "manages background tasks and output retrieval" $ do
      let prog = do
            tid <- runBackground "cargo test"
            info <- getTaskOutput tid
            stopped <- stopTask tid
            pure (tid, info, stopped)
          ((tid, info, stopped), _) = runPure emptyMockEnv prog
      tid `shouldBe` TaskId "task_bg"
      tiStatus info `shouldBe` "running"
      stopped `shouldBe` True

    it "records desktop notifications" $ do
      let prog = sendNotification "Build Finished" "All 258 tests passed."
          ((), endEnv) = runPure emptyMockEnv prog
      mockNotifications endEnv `shouldBe` [("Build Finished", "All 258 tests passed.")]

    it "invokes MCP tools and lists registered tools" $ do
      let prog = do
            tools <- listMcpTools
            res <- callMcpTool "git-server" "diff" "{}"
            pure (tools, res)
          ((tools, res), _) = runPure emptyMockEnv prog
      tools `shouldBe` []
      res `shouldBe` ToolSuccess "mcp ok"

    it "loads memory and resolves imports purely" $ do
      let prog = do
            mem <- loadMemory "CLAUDE.md"
            imp <- resolveImport "doc.md"
            pure (mem, imp)
          ((mem, imp), _) = runPure emptyMockEnv prog
      mem `shouldBe` ""
      imp `shouldBe` ""

    it "blocks tool execution when PreToolUse hook denies permission" $ do
      let toolCall1 = ToolCall "c1" "write_file" "{\"path\":\"foo.txt\",\"content\":\"bar\"}"
          step1 _ _ = Right $ AssistantResponse Nothing [toolCall1] Nothing
          step2 _ _ = Right $ AssistantResponse (Just "done") [] Nothing
          denyHook _ _ = defaultHookResult { hrDecision = Just (PermDeny "PreToolUse blocked by hook") }
          env = emptyMockEnv
            { mockLLMSteps = [step1, step2]
            , mockHooks = denyHook
            }
          initHist = [UserMsg "Write foo"]
          ((result, _), endEnv) = runPure env (agentLoop baseConfig allToolDefs initHist)
      result `shouldBe` AgentCompleted "done"
      Map.lookup "foo.txt" (mockFiles endEnv) `shouldBe` Nothing
      mockEvents endEnv `shouldContain` [EvPermissionDenied "write_file" "Blocked by PreToolUse hook"]

    it "blocks tool execution when permission policy check fails" $ do
      let toolCall1 = ToolCall "c1" "write_file" "{\"path\":\"foo.txt\",\"content\":\"bar\"}"
          step1 _ _ = Right $ AssistantResponse Nothing [toolCall1] Nothing
          step2 _ _ = Right $ AssistantResponse (Just "done") [] Nothing
          env = emptyMockEnv
            { mockLLMSteps = [step1, step2]
            , mockPermissions = \_ _ -> False
            }
          initHist = [UserMsg "Write foo"]
          ((result, _), endEnv) = runPure env (agentLoop baseConfig allToolDefs initHist)
      result `shouldBe` AgentCompleted "done"
      Map.lookup "foo.txt" (mockFiles endEnv) `shouldBe` Nothing
      mockEvents endEnv `shouldContain` [EvPermissionDenied "write_file" "Permission denied by policy"]
