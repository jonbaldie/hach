{-# LANGUAGE OverloadedStrings #-}

module Agent.TUISpec (spec) where

import Agent.TUI.State
import Agent.TUI.Types
import Agent.Types (AgentEvent(..), ToolResult(..))
import Test.Hspec

spec :: Spec
spec = do
  let baseState = initialTuiState "meta/muse-glimmer-30b" 10

  describe "TUI Pure State Reducer (Pre-agreed Seam)" $ do
    describe "Text Input and Prompt Submission" $ do
      it "accumulates characters typed by the user" $ do
        let s1 = fst $ updateTui (EvUserKey (KeyChar 'H')) baseState
            s2 = fst $ updateTui (EvUserKey (KeyChar 'i')) s1
        tsInputBuffer s2 `shouldBe` "Hi"

      it "handles backspace correctly" $ do
        let s1 = baseState { tsInputBuffer = "Hello" }
            s2 = fst $ updateTui (EvUserKey KeyBackspace) s1
        tsInputBuffer s2 `shouldBe` "Hell"

      it "handles Ctrl+U to clear the input buffer" $ do
        let s1 = baseState { tsInputBuffer = "Clear me" }
            s2 = fst $ updateTui (EvUserKey (KeyCtrl 'u')) s1
        tsInputBuffer s2 `shouldBe` ""

      it "submits prompt on Enter and emits ActionRunAgent" $ do
        let s1 = baseState { tsInputBuffer = "Refactor module" }
            (s2, actions) = updateTui (EvUserKey KeyEnter) s1
        tsInputBuffer s2 `shouldBe` ""
        tsStatus s2 `shouldBe` StatusThinking
        tsHistory s2 `shouldBe` [DiUser "Refactor module"]
        actions `shouldBe` [ActionRunAgent "Refactor module"]

      it "ignores empty prompt submissions" $ do
        let (s1, actions) = updateTui (EvUserKey KeyEnter) baseState
        tsStatus s1 `shouldBe` StatusIdle
        actions `shouldBe` []

    describe "Focus Navigation" $ do
      it "cycles focus with Tab: Input -> History -> Tools -> Input" $ do
        tsFocus baseState `shouldBe` FocusInput
        let s1 = fst $ updateTui (EvUserKey KeyTab) baseState
        tsFocus s1 `shouldBe` FocusHistory
        let s2 = fst $ updateTui (EvUserKey KeyTab) s1
        tsFocus s2 `shouldBe` FocusTools
        let s3 = fst $ updateTui (EvUserKey KeyTab) s2
        tsFocus s3 `shouldBe` FocusInput

      it "cycles backwards with BackTab" $ do
        let s1 = fst $ updateTui (EvUserKey KeyBackTab) baseState
        tsFocus s1 `shouldBe` FocusTools

    describe "Cancellation and Quitting" $ do
      it "quits on Ctrl+Q" $ do
        let (s1, actions) = updateTui (EvUserKey (KeyCtrl 'q')) baseState
        tsShouldQuit s1 `shouldBe` True
        actions `shouldBe` [ActionQuit]

      it "quits on Ctrl+C when idle" $ do
        let (s1, actions) = updateTui (EvUserKey (KeyCtrl 'c')) baseState
        tsShouldQuit s1 `shouldBe` True
        actions `shouldBe` [ActionQuit]

      it "cancels turn on Esc or Ctrl+C when busy" $ do
        let busyState = baseState { tsStatus = StatusThinking }
            (s1, actions) = updateTui (EvUserKey KeyEsc) busyState
        tsCancelRequested s1 `shouldBe` True
        tsStatus s1 `shouldBe` StatusError "Turn cancelled by user."
        actions `shouldBe` [ActionCancelAgent]

    describe "Help Overlay" $ do
      it "toggles help on '?' when not typing in input" $ do
        let historyFocus = baseState { tsFocus = FocusHistory }
            s1 = fst $ updateTui (EvUserKey (KeyChar '?')) historyFocus
        tsShowHelp s1 `shouldBe` True
        let s2 = fst $ updateTui (EvUserKey (KeyChar '?')) s1
        tsShowHelp s2 `shouldBe` False

    describe "Agent Event Processing" $ do
      it "updates status and turn counter on EvTurnStart" $ do
        let s1 = fst $ updateTui (EvHarness (EvTurnStart 3)) baseState
        tsCurrentTurn s1 `shouldBe` 3
        tsStatus s1 `shouldBe` StatusThinking

      it "records LLM assistant text into history" $ do
        let s1 = fst $ updateTui (EvHarness (EvLLMResponse (Just "Working on it...") [])) baseState
        tsHistory s1 `shouldBe` [DiAssistant "Working on it..."]
        tsStatus s1 `shouldBe` StatusFinished

      it "creates a tool activity card on EvToolCall" $ do
        let s1 = fst $ updateTui (EvHarness (EvToolCall "read_file" "{\"path\":\"foo.hs\"}")) baseState
        tsStatus s1 `shouldBe` StatusRunningTool "read_file"
        case tsTools s1 of
          [card] -> do
            tiName card `shouldBe` "read_file"
            tiArgs card `shouldBe` "{\"path\":\"foo.hs\"}"
            tiResult card `shouldBe` Nothing
            tiExpanded card `shouldBe` False
          _ -> expectationFailure "Expected exactly one tool card"

      it "attaches result to latest tool card on EvToolResult" $ do
        let s1 = fst $ updateTui (EvHarness (EvToolCall "read_file" "{\"path\":\"foo.hs\"}")) baseState
            s2 = fst $ updateTui (EvHarness (EvToolResult "read_file" (ToolSuccess "file contents"))) s1
        case tsTools s2 of
          [card] -> tiResult card `shouldBe` Just (ToolSuccess "file contents")
          _      -> expectationFailure "Expected exactly one tool card"

      it "toggles tool expansion on Enter when FocusTools is active" $ do
        let s1 = fst $ updateTui (EvHarness (EvToolCall "run_command" "echo hi")) baseState
            toolsFocus = s1 { tsFocus = FocusTools }
            s2 = fst $ updateTui (EvUserKey KeyEnter) toolsFocus
        case tsTools s2 of
          [card] -> tiExpanded card `shouldBe` True
          _      -> expectationFailure "Expected tool card"
        let s3 = fst $ updateTui (EvUserKey KeyEnter) s2
        case tsTools s3 of
          [card] -> tiExpanded card `shouldBe` False
          _      -> expectationFailure "Expected tool card"

      it "records final completion answer on EvDone" $ do
        let s1 = fst $ updateTui (EvHarness (EvDone "Task completed successfully.")) baseState
        tsStatus s1 `shouldBe` StatusFinished
        last (tsHistory s1) `shouldBe` DiAssistant "Task completed successfully."

      it "records error notice on EvError" $ do
        let s1 = fst $ updateTui (EvHarness (EvError "API timeout")) baseState
        tsStatus s1 `shouldBe` StatusError "API timeout"
        last (tsHistory s1) `shouldBe` DiNotice "Error: API timeout"

    describe "Multi-Turn Dialogue User Flow" $ do
      it "allows typing into input box after receiving assistant response to first user message" $ do
        let s0 = baseState { tsInputBuffer = "First question" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
            s2 = fst $ updateTui (EvHarness (EvDone "First answer")) s1
        tsFocus s2 `shouldBe` FocusInput
        let (s3, _) = updateTui (EvUserKey (KeyChar 'a')) s2
        tsInputBuffer s3 `shouldBe` "a"

      it "restores FocusInput on EvError so user can retry" $ do
        let s0 = baseState { tsInputBuffer = "Failing task" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
            s2 = fst $ updateTui (EvHarness (EvError "Network failed")) s1
        tsFocus s2 `shouldBe` FocusInput

      it "accumulates full conversation history across multiple turns" $ do
        let s0 = baseState { tsInputBuffer = "First question" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
            s2 = fst $ updateTui (EvHarness (EvDone "First answer")) s1
            (s3, _) = updateTui (EvUserKey (KeyChar 'M')) s2
            (s4, _) = updateTui (EvUserKey (KeyChar '2')) s3
            (s5, actions) = updateTui (EvUserKey KeyEnter) s4
        tsHistory s5 `shouldBe` [DiUser "First question", DiAssistant "First answer", DiUser "M2"]
        actions `shouldBe` [ActionRunAgent "M2"]

    describe "Viewport Scrolling Actions" $ do
      it "emits ActionScrollHistory 1 on KeyDown in History panel" $ do
        let sHistory = baseState { tsFocus = FocusHistory }
            (_, actions) = updateTui (EvUserKey KeyDown) sHistory
        actions `shouldBe` [ActionScrollHistory 1]

      it "emits ActionScrollHistory (-1) on KeyUp in History panel" $ do
        let sHistory = baseState { tsFocus = FocusHistory }
            (_, actions) = updateTui (EvUserKey KeyUp) sHistory
        actions `shouldBe` [ActionScrollHistory (-1)]

      it "emits ActionScrollHistory 5 on KeyPageDown in History panel" $ do
        let sHistory = baseState { tsFocus = FocusHistory }
            (_, actions) = updateTui (EvUserKey KeyPageDown) sHistory
        actions `shouldBe` [ActionScrollHistory 5]

      it "emits ActionScrollHistory (-5) on KeyPageUp in History panel" $ do
        let sHistory = baseState { tsFocus = FocusHistory }
            (_, actions) = updateTui (EvUserKey KeyPageUp) sHistory
        actions `shouldBe` [ActionScrollHistory (-5)]

      it "emits ActionScrollTools 1 on KeyDown in Tools panel" $ do
        let tool1 = ToolItem "read_file" "{}" Nothing False
            tool2 = ToolItem "write_file" "{}" Nothing False
            sTools = baseState { tsFocus = FocusTools, tsTools = [tool1, tool2] }
            (_, actions) = updateTui (EvUserKey KeyDown) sTools
        actions `shouldBe` [ActionScrollTools 1]

      it "emits ActionScrollTools (-1) on KeyUp in Tools panel" $ do
        let tool1 = ToolItem "read_file" "{}" Nothing False
            tool2 = ToolItem "write_file" "{}" Nothing False
            sTools = baseState { tsFocus = FocusTools, tsTools = [tool1, tool2], tsSelectedToolIndex = 1 }
            (_, actions) = updateTui (EvUserKey KeyUp) sTools
        actions `shouldBe` [ActionScrollTools (-1)]
