{-# LANGUAGE OverloadedStrings #-}

module Agent.CoreSpec (spec) where

import Agent.Core
import Agent.Interpreter.Pure
import Agent.Tools
import Agent.Types
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = do
  let baseConfig = AgentConfig
        { cfgModel = "test-model"
        , cfgSystemPrompt = Just "You are an assistant."
        , cfgMaxTurns = Just 5
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
      let loopConfig = baseConfig { cfgMaxTurns = Just 2 }
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

    it "runs past the old default of 10 turns when cfgMaxTurns is Nothing (unlimited)" $ do
      let toolCall = ToolCall
            { callId = "call_loop"
            , functionName = "read_file"
            , callArgsRaw = "{\"path\":\"hello.txt\"}"
            }
          stepLoop _ _ = Right $ AssistantResponse Nothing [toolCall] Nothing
          stepFinal _ _ = Right $ AssistantResponse (Just "Finally done!") [] Nothing
          unlimitedConfig = baseConfig { cfgMaxTurns = Nothing }
          -- 12 tool-calling turns, then a final answer on turn 13.
          env = emptyMockEnv
            { mockLLMSteps = replicate 12 stepLoop ++ [stepFinal]
            , mockFiles = Map.fromList [("hello.txt", "data")]
            }
          ((result, _), _) = runPure env (agentLoop unlimitedConfig allToolDefs [UserMsg "Run long"])

      result `shouldBe` AgentCompleted "Finally done!"

    it "never terminates with AgentMaxTurnsReached when cfgMaxTurns is Nothing" $ do
      let toolCall = ToolCall
            { callId = "call_loop"
            , functionName = "read_file"
            , callArgsRaw = "{\"path\":\"hello.txt\"}"
            }
          stepLoop _ _ = Right $ AssistantResponse Nothing [toolCall] Nothing
          stepFinal _ _ = Right $ AssistantResponse (Just "Done") [] Nothing
          unlimitedConfig = baseConfig { cfgMaxTurns = Nothing }
          env = emptyMockEnv
            { mockLLMSteps = replicate 100 stepLoop ++ [stepFinal]
            , mockFiles = Map.fromList [("hello.txt", "data")]
            }
          ((result, _), _) = runPure env (agentLoop unlimitedConfig allToolDefs [UserMsg "Run very long"])

      result `shouldNotBe` AgentMaxTurnsReached 100
      result `shouldBe` AgentCompleted "Done"

    it "terminates with AgentFailed when model returns an error" $ do
      let stepError _ _ = Left "401 Unauthorized"
          env = emptyMockEnv { mockLLMSteps = [stepError] }
          initHist = [UserMsg "Fail please"]
          ((result, finalHist), endEnv) = runPure env (agentLoop baseConfig [] initHist)

      result `shouldBe` AgentFailed "401 Unauthorized"
      finalHist `shouldBe` initHist
      mockEvents endEnv `shouldContain` [EvError "401 Unauthorized"]
