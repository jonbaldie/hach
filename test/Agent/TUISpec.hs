{-# LANGUAGE OverloadedStrings #-}

module Agent.TUISpec (spec) where

import Agent.Skills (Skill(..), SkillSource(..))
import Agent.TUI.App (vtyToUserKey)
import Agent.TUI.State
import Agent.TUI.Types
import Agent.TUI.UI (formatTokens)
import Agent.Types (AgentEvent(..), TokenUsage(..), ToolResult(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
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
        let s1 = fst $ updateTui (EvHarness (EvLLMResponse (Just "Working on it...") [] Nothing)) baseState
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

      it "scrolls history on KeyPageUp and KeyPageDown even when focused in FocusInput" $ do
        let sInput = baseState { tsFocus = FocusInput }
            (_, actionsUp) = updateTui (EvUserKey KeyPageUp) sInput
            (_, actionsDown) = updateTui (EvUserKey KeyPageDown) sInput
        actionsUp `shouldBe` [ActionScrollHistory (-5)]
        actionsDown `shouldBe` [ActionScrollHistory 5]

      it "scrolls history on mouse wheel KeyScrollUp and KeyScrollDown in FocusInput" $ do
        let sInput = baseState { tsFocus = FocusInput }
            (_, actionsUp) = updateTui (EvUserKey KeyScrollUp) sInput
            (_, actionsDown) = updateTui (EvUserKey KeyScrollDown) sInput
        actionsUp `shouldBe` [ActionScrollHistory (-2)]
        actionsDown `shouldBe` [ActionScrollHistory 2]

      it "scrolls tools on KeyScrollUp and KeyScrollDown in FocusTools" $ do
        let sTools = baseState { tsFocus = FocusTools }
            (_, actionsUp) = updateTui (EvUserKey KeyScrollUp) sTools
            (_, actionsDown) = updateTui (EvUserKey KeyScrollDown) sTools
        actionsUp `shouldBe` [ActionScrollTools (-2)]
        actionsDown `shouldBe` [ActionScrollTools 2]

      it "scrolls tool activity on KeyDown even when there is only 1 tool card" $ do
        let tool1 = ToolItem "read_file" "{}" Nothing True
            sTools = baseState { tsFocus = FocusTools, tsTools = [tool1], tsSelectedToolIndex = 0 }
            (_, actions) = updateTui (EvUserKey KeyDown) sTools
        actions `shouldBe` [ActionScrollTools 1]

    describe "Vty to UserKey Event Conversion" $ do
      it "converts mouse scroll wheel up to KeyScrollUp" $ do
        vtyToUserKey (Vty.EvMouseDown 10 10 Vty.BScrollUp []) `shouldBe` Just KeyScrollUp

      it "converts mouse scroll wheel down to KeyScrollDown" $ do
        vtyToUserKey (Vty.EvMouseDown 10 10 Vty.BScrollDown []) `shouldBe` Just KeyScrollDown

      it "converts Shift+Tab variations to KeyBackTab" $ do
        vtyToUserKey (Vty.EvKey (Vty.KChar '\t') [Vty.MShift]) `shouldBe` Just KeyBackTab
        vtyToUserKey (Vty.EvKey Vty.KBackTab [Vty.MShift]) `shouldBe` Just KeyBackTab

    describe "Prompt History Navigation (Up/Down in FocusInput)" $ do
      it "recalls the previous prompt on KeyUp" $ do
        let s0 = baseState { tsPromptHistory = ["prompt 1", "prompt 2"] }
            (s1, _) = updateTui (EvUserKey KeyUp) s0
        tsInputBuffer s1 `shouldBe` "prompt 2"
        tsPromptHistoryIndex s1 `shouldBe` Just 1

      it "recalls earlier prompts on subsequent KeyUps" $ do
        let s0 = baseState { tsPromptHistory = ["prompt 1", "prompt 2", "prompt 3"] }
            (s1, _) = updateTui (EvUserKey KeyUp) s0
            (s2, _) = updateTui (EvUserKey KeyUp) s1
        tsInputBuffer s2 `shouldBe` "prompt 2"
        tsPromptHistoryIndex s2 `shouldBe` Just 1
        let (s3, _) = updateTui (EvUserKey KeyUp) s2
        tsInputBuffer s3 `shouldBe` "prompt 1"
        tsPromptHistoryIndex s3 `shouldBe` Just 0

      it "stops at the oldest prompt on repeated KeyUps" $ do
        let s0 = baseState { tsPromptHistory = ["prompt 1", "prompt 2"] }
            (s1, _) = updateTui (EvUserKey KeyUp) s0
            (s2, _) = updateTui (EvUserKey KeyUp) s1
            (s3, _) = updateTui (EvUserKey KeyUp) s2
        tsInputBuffer s3 `shouldBe` "prompt 1"
        tsPromptHistoryIndex s3 `shouldBe` Just 0

      it "moves forward in history with KeyDown and restores draft buffer" $ do
        let s0 = baseState
              { tsPromptHistory = ["prompt 1", "prompt 2"]
              , tsInputBuffer   = "my pending draft"
              }
            (s1, _) = updateTui (EvUserKey KeyUp) s0
        tsInputBuffer s1 `shouldBe` "prompt 2"
        tsPromptDraft s1 `shouldBe` "my pending draft"

        let (s2, _) = updateTui (EvUserKey KeyUp) s1
        tsInputBuffer s2 `shouldBe` "prompt 1"

        let (s3, _) = updateTui (EvUserKey KeyDown) s2
        tsInputBuffer s3 `shouldBe` "prompt 2"

        let (s4, _) = updateTui (EvUserKey KeyDown) s3
        tsInputBuffer s4 `shouldBe` "my pending draft"
        tsPromptHistoryIndex s4 `shouldBe` Nothing

      it "appends submitted prompt to tsPromptHistory on Enter and resets index" $ do
        let s0 = baseState { tsInputBuffer = "first query" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsPromptHistory s1 `shouldBe` ["first query"]
        tsPromptHistoryIndex s1 `shouldBe` Nothing

        let s2 = fst (updateTui (EvHarness (EvDone "answer")) s1)
            s3 = s2 { tsInputBuffer = "second query" }
            (s4, _) = updateTui (EvUserKey KeyEnter) s3
        tsPromptHistory s4 `shouldBe` ["first query", "second query"]
        tsPromptHistoryIndex s4 `shouldBe` Nothing

    describe "Context Window Token Usage Tracking" $ do
      it "initializes context tokens to 0 and token usage to Nothing" $ do
        tsContextTokens baseState `shouldBe` 0
        tsTokenUsage baseState `shouldBe` Nothing

      it "updates context tokens and token usage on EvLLMResponse" $ do
        let usage = TokenUsage 120 30 150
            (s1, _) = updateTui (EvHarness (EvLLMResponse (Just "Hello") [] (Just usage))) baseState
        tsContextTokens s1 `shouldBe` 150
        tsTokenUsage s1 `shouldBe` Just usage

      it "updates context tokens with latest turn on subsequent EvLLMResponse" $ do
        let usage1 = TokenUsage 120 30 150
            usage2 = TokenUsage 200 45 245
            (s1, _) = updateTui (EvHarness (EvLLMResponse Nothing [] (Just usage1))) baseState
            (s2, _) = updateTui (EvHarness (EvLLMResponse (Just "Done") [] (Just usage2))) s1
        tsContextTokens s2 `shouldBe` 245
        tsTokenUsage s2 `shouldBe` Just usage2

      it "resets context tokens to 0 when history is cleared" $ do
        let usage = TokenUsage 120 30 150
            (s1, _) = updateTui (EvHarness (EvLLMResponse (Just "Hello") [] (Just usage))) baseState
            s2 = s1 { tsFocus = FocusHistory }
            (s3, _) = updateTui (EvUserKey (KeyChar 'c')) s2
        tsContextTokens s3 `shouldBe` 0
        tsTokenUsage s3 `shouldBe` Nothing

    describe "formatTokens" $ do
      it "formats small counts without commas" $ do
        formatTokens 0 `shouldBe` "0"
        formatTokens 42 `shouldBe` "42"
        formatTokens 999 `shouldBe` "999"

      it "formats thousands and larger counts with commas" $ do
        formatTokens 1000 `shouldBe` "1,000"
        formatTokens 1520 `shouldBe` "1,520"
        formatTokens 128450 `shouldBe` "128,450"

    describe "Local Slash Commands and Skill Invocations" $ do
      it "handles /clear locally by emptying dialogue history without running agent" $ do
        let s0 = baseState { tsHistory = [DiUser "Hello"], tsInputBuffer = "/clear" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        tsHistory s1 `shouldBe` []
        tsInputBuffer s1 `shouldBe` ""
        actions `shouldBe` []

      it "handles /help locally by toggling help dialog without running agent" $ do
        let s0 = baseState { tsShowHelp = False, tsInputBuffer = "/help" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        tsShowHelp s1 `shouldBe` True
        tsInputBuffer s1 `shouldBe` ""
        actions `shouldBe` []

      it "handles /cost locally by appending token notice without running agent" $ do
        let s0 = baseState { tsContextTokens = 1500, tsInputBuffer = "/cost" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        tsInputBuffer s1 `shouldBe` ""
        actions `shouldBe` []
        tsHistory s1 `shouldSatisfy` \h -> any (\case DiNotice msg -> "1,500" `T.isInfixOf` msg; _ -> False) h

      it "handles /compact locally by appending compact notice without running agent" $ do
        let s0 = baseState { tsInputBuffer = "/compact" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        tsInputBuffer s1 `shouldBe` ""
        actions `shouldBe` []
        tsHistory s1 `shouldSatisfy` \h -> any (\case DiNotice msg -> "compacted" `T.isInfixOf` msg; _ -> False) h

      it "invokes discovered skill when user types /skill-name" $ do
        let skillA = Skill "to-spec" "Generate spec" "Spec rules here" "/p" SkillGlobal
            s0 = baseState
              { tsSkills = Map.fromList [("to-spec", skillA)]
              , tsInputBuffer = "/to-spec create auth"
              }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        actions `shouldSatisfy` \case
          [ActionRunAgent prompt] ->
            "<skill name=\"to-spec\">" `T.isInfixOf` prompt && "create auth" `T.isInfixOf` prompt
          _ -> False
        tsHistory s1 `shouldSatisfy` \h -> any (\case DiUser u -> "/to-spec create auth" `T.isInfixOf` u; _ -> False) h
