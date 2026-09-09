{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hach.TUISpec (spec) where

import Hach.Core (AgentAlgebra(..))
import Hach.Interpreter.IO (ioAlgebra, ioModel, newIOEnv)
import Hach.Skills (SkillSource(..), mkSkill)
import Hach.TUI.App
  ( buildTuiSystemPrompt
  , cancelledToolCallPlaceholder
  , dialogueToMessages
  , goalAgentConfig
  , runEnvForModel
  , initialTuiLaunch
  , runGoalWorker
  , transcriptToMessages
  , vtyToUserKey
  )
import Test.QuickCheck
import Hach.TUI.State
import Hach.TUI.Types
import Hach.TUI.UI (drawUI, formatCompactLimit, formatTokens, renderMaxTurns, tuiAttrMap)
import Hach.Types
  ( AgentConfig(..)
  , AgentEvent(..)
  , AssistantResponse(..)
  , GoalEvaluation(..)
  , GoalState(..)
  , GoalStatus(..)
  , GoalVerdict(..)
  , Message(..)
  , SessionTokenUsage(..)
  , TokenUsage(..)
  , ToolCall(..)
  , ToolResult(..)
  , contextSaturationPercent
  , initialGoalState
  , initialSessionTokenUsage
  , mkTokenUsage
  , modelContextLimit
  )
import qualified Brick.Main as M
import Control.Monad (forM_)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.PictureToSpans as PTS
import qualified Graphics.Vty.Span as Span
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Test.Hspec

renderTestRows :: TuiState -> (Int, Int) -> [T.Text]
renderTestRows st region =
  let pic = M.renderWidget (Just tuiAttrMap) (drawUI st) region
  in map flattenRow (V.toList (PTS.displayOpsForPic pic region))
  where
    flattenRow = V.foldl' step ""
    step acc op = case op of
      Span.TextSpan _ _ _ t -> acc <> TL.toStrict t
      Span.Skip n           -> acc <> T.replicate n " "
      Span.RowEnd n         -> acc <> T.replicate n " "

instance Arbitrary ToolLifecycle where
  arbitrary = oneof
    [ pure Pending
    , pure Running
    , Finished <$> oneof [ToolSuccess <$> genText, ToolError <$> genText]
    , Denied <$> genText
    , pure Cancelled
    ]
    where
      genText = T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary ToolCard where
  arbitrary = ToolCard
    <$> (T.pack <$> listOf1 (elements ['a'..'z']))
    <*> (T.pack <$> listOf1 (elements ['a'..'z']))
    <*> (T.pack <$> listOf (elements ['a'..'z']))
    <*> arbitrary
    <*> arbitrary

instance Arbitrary TranscriptItem where
  arbitrary = oneof
    [ TiUser <$> (T.pack <$> listOf (elements ['a'..'z']))
    , TiAssistant <$> (T.pack <$> listOf (elements ['a'..'z']))
    , TiSystem <$> (T.pack <$> listOf (elements ['a'..'z']))
    , TiNotice <$> (T.pack <$> listOf (elements ['a'..'z']))
    , TiToolCard <$> arbitrary
    ]

spec :: Spec
spec = do
  let baseState = initialTuiState "meta/muse-glimmer-30b" (Just 10)

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

      it "handles delete key in input buffer" $ do
        let s1 = baseState { tsInputBuffer = "Hello" }
            s2 = fst $ updateTui (EvUserKey KeyDelete) s1
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
      it "cycles focus with Tab between Input and Transcript" $ do
        tsFocus baseState `shouldBe` FocusInput
        let s1 = fst $ updateTui (EvUserKey KeyTab) baseState
        tsFocus s1 `shouldBe` FocusTranscript
        let s2 = fst $ updateTui (EvUserKey KeyTab) s1
        tsFocus s2 `shouldBe` FocusInput

      it "cycles backwards with BackTab between Input and Transcript" $ do
        let s1 = fst $ updateTui (EvUserKey KeyBackTab) baseState
        tsFocus s1 `shouldBe` FocusTranscript
        let s2 = fst $ updateTui (EvUserKey KeyBackTab) s1
        tsFocus s2 `shouldBe` FocusInput

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
        let (s1, actions) = updateTui (EvUserKey KeyEsc) busyState
        tsCancelRequested s1 `shouldBe` True
        tsStatus s1 `shouldBe` StatusError "Turn cancelled by user."
        actions `shouldBe` [ActionCancelAgent]

      it "restores FocusInput and allows typing after cancelling turn with Esc" $ do
        let s0 = baseState { tsInputBuffer = "Initial prompt" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsStatus s1 `shouldBe` StatusThinking
        tsFocus s1 `shouldBe` FocusTranscript
        let (s2, actions) = updateTui (EvUserKey KeyEsc) s1
        tsStatus s2 `shouldBe` StatusError "Turn cancelled by user."
        actions `shouldBe` [ActionCancelAgent]
        tsFocus s2 `shouldBe` FocusInput
        let (s3, _) = updateTui (EvUserKey (KeyChar 'n')) s2
            (s4, _) = updateTui (EvUserKey (KeyChar 'e')) s3
            (s5, _) = updateTui (EvUserKey (KeyChar 'w')) s4
        tsInputBuffer s5 `shouldBe` "new"

      it "restores FocusInput and allows typing after cancelling turn with Ctrl+C when busy" $ do
        let s0 = baseState { tsInputBuffer = "Initial prompt" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsStatus s1 `shouldBe` StatusThinking
        tsFocus s1 `shouldBe` FocusTranscript
        let (s2, actions) = updateTui (EvUserKey (KeyCtrl 'c')) s1
        tsStatus s2 `shouldBe` StatusError "Turn cancelled by user."
        actions `shouldBe` [ActionCancelAgent]
        tsFocus s2 `shouldBe` FocusInput
        let (s3, _) = updateTui (EvUserKey (KeyChar 'h')) s2
            (s4, _) = updateTui (EvUserKey (KeyChar 'i')) s3
        tsInputBuffer s4 `shouldBe` "hi"

      it "allows submitting a new prompt after cancelling turn with Esc" $ do
        let s0 = baseState { tsInputBuffer = "First prompt" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
            (s2, _) = updateTui (EvUserKey KeyEsc) s1
            (s3, _) = updateTui (EvUserKey (KeyChar 'r')) s2
            (s4, _) = updateTui (EvUserKey (KeyChar 'e')) s3
            (s5, _) = updateTui (EvUserKey (KeyChar 't')) s4
            (s6, _) = updateTui (EvUserKey (KeyChar 'r')) s5
            (s7, _) = updateTui (EvUserKey (KeyChar 'y')) s6
            (s8, actions) = updateTui (EvUserKey KeyEnter) s7
        tsStatus s8 `shouldBe` StatusThinking
        tsCancelRequested s8 `shouldBe` False
        tsHistory s8 `shouldBe` [DiUser "First prompt", DiUser "retry"]
        actions `shouldBe` [ActionRunAgent "retry"]

      it "resets tsCancelRequested when /goal is submitted following a cancellation" $ do
        let busyState = baseState { tsStatus = StatusThinking }
            (s1, _) = updateTui (EvUserKey KeyEsc) busyState
        tsCancelRequested s1 `shouldBe` True
        let s2 = s1 { tsInputBuffer = "/goal check tests" }
            (s3, actions) = updateTui (EvUserKey KeyEnter) s2
        tsCancelRequested s3 `shouldBe` False
        actions `shouldBe` [ActionRunGoal "check tests"]

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

      it "yields assistant text followed by N Pending cards in order on EvLLMResponse" $ do
        let calls =
              [ ToolCall "call-1" "read_file" "{\"path\":\"foo.hs\"}"
              , ToolCall "call-2" "list_dir" "{\"path\":\".\"}"
              ]
            resp = EvHarness (EvLLMResponse (Just "Investigating the repo") calls Nothing)
            (s1, _) = updateTui resp baseState
        tsTranscript s1 `shouldBe`
          [ TiAssistant "Investigating the repo"
          , TiToolCard (ToolCard "call-1" "read_file" "{\"path\":\"foo.hs\"}" Pending False)
          , TiToolCard (ToolCard "call-2" "list_dir" "{\"path\":\".\"}" Pending False)
          ]

      it "transitions Pending -> Running (with args replaced) on EvToolCall" $ do
        let calls = [ToolCall "call-1" "read_file" "{\"path\":\"foo.hs\"}"]
            s0 = fst $ updateTui (EvHarness (EvLLMResponse (Just "Starting") calls Nothing)) baseState
            (s1, _) = updateTui (EvHarness (EvToolCall "read_file" "{\"path\":\"foo_rewritten.hs\"}")) s0
        tsStatus s1 `shouldBe` StatusRunningTool "read_file"
        tsTranscript s1 `shouldBe`
          [ TiAssistant "Starting"
          , TiToolCard (ToolCard "call-1" "read_file" "{\"path\":\"foo_rewritten.hs\"}" Running False)
          ]
        tsSelectedToolIndex s1 `shouldBe` 0

      it "transitions Running -> Finished on EvToolResult" $ do
        let calls = [ToolCall "call-1" "read_file" "{\"path\":\"foo.hs\"}"]
            s0 = fst $ updateTui (EvHarness (EvLLMResponse (Just "Starting") calls Nothing)) baseState
            s1 = fst $ updateTui (EvHarness (EvToolCall "read_file" "{\"path\":\"foo.hs\"}")) s0
            (s2, _) = updateTui (EvHarness (EvToolResult "read_file" (ToolSuccess "file contents"))) s1
        tsStatus s2 `shouldBe` StatusThinking
        tsTranscript s2 `shouldBe`
          [ TiAssistant "Starting"
          , TiToolCard (ToolCard "call-1" "read_file" "{\"path\":\"foo.hs\"}" (Finished (ToolSuccess "file contents")) False)
          ]

      it "transitions Pending/Running -> Denied on permission-denied and appends notice" $ do
        let calls =
              [ ToolCall "call-1" "read_file" "{\"path\":\"secret.txt\"}"
              , ToolCall "call-2" "run_command" "rm -rf /"
              ]
            s0 = fst $ updateTui (EvHarness (EvLLMResponse (Just "Checking") calls Nothing)) baseState
            (s1, _) = updateTui (EvHarness (EvPermissionDenied "read_file" "Protected path")) s0
        tsTranscript s1 `shouldBe`
          [ TiAssistant "Checking"
          , TiToolCard (ToolCard "call-1" "read_file" "{\"path\":\"secret.txt\"}" (Denied "Protected path") False)
          , TiToolCard (ToolCard "call-2" "run_command" "rm -rf /" Pending False)
          , TiNotice "Permission denied for read_file: Protected path"
          ]

      it "resets status to Thinking on EvPermissionDenied after EvToolCall" $ do
        let calls = [ToolCall "call-1" "write_file" "{\"path\":\"config.json\"}"]
            s0 = fst $ updateTui (EvHarness (EvLLMResponse (Just "Starting") calls Nothing)) baseState
            (s1, _) = updateTui (EvHarness (EvToolCall "write_file" "{\"path\":\"config.json\"}")) s0
            (s2, _) = updateTui (EvHarness (EvPermissionDenied "write_file" "Access denied by policy")) s1
        tsStatus s1 `shouldBe` StatusRunningTool "write_file"
        tsStatus s2 `shouldBe` StatusThinking

      it "transitions all unresolved (Pending or Running) cards -> Cancelled on cancel" $ do
        let calls =
              [ ToolCall "call-1" "tool1" "arg1"
              , ToolCall "call-2" "tool2" "arg2"
              , ToolCall "call-3" "tool3" "arg3"
              ]
            s0 = fst $ updateTui (EvHarness (EvLLMResponse (Just "Running tools") calls Nothing)) baseState
            s1 = fst $ updateTui (EvHarness (EvToolCall "tool1" "arg1")) s0
            s2 = fst $ updateTui (EvHarness (EvToolResult "tool1" (ToolSuccess "done1"))) s1
            s3 = fst $ updateTui (EvHarness (EvToolCall "tool2" "arg2")) s2
            (s4, actions) = updateTui (EvUserKey KeyEsc) s3
        actions `shouldBe` [ActionCancelAgent]
        tsCancelRequested s4 `shouldBe` True
        tsTranscript s4 `shouldBe`
          [ TiAssistant "Running tools"
          , TiToolCard (ToolCard "call-1" "tool1" "arg1" (Finished (ToolSuccess "done1")) False)
          , TiToolCard (ToolCard "call-2" "tool2" "arg2" Cancelled False)
          , TiToolCard (ToolCard "call-3" "tool3" "arg3" Cancelled False)
          ]

      it "interleaves text and tool cards across two turns in exact emission order" $ do
        let (s1, _) = updateTui (EvSubmit "Read config") baseState
            (s2, _) = updateTui (EvHarness (EvLLMResponse (Just "Reading config file") [ToolCall "c1" "read_file" "config.json"] Nothing)) s1
            (s3, _) = updateTui (EvHarness (EvToolCall "read_file" "config.json")) s2
            (s4, _) = updateTui (EvHarness (EvToolResult "read_file" (ToolSuccess "{\"port\":8080}"))) s3
            (s5, _) = updateTui (EvHarness (EvLLMResponse (Just "Now starting server") [ToolCall "c2" "run_command" "serve"] Nothing)) s4
            (s6, _) = updateTui (EvHarness (EvToolCall "run_command" "serve")) s5
            (s7, _) = updateTui (EvHarness (EvToolResult "run_command" (ToolSuccess "Server started"))) s6
            (s8, _) = updateTui (EvHarness (EvDone "All operations complete.")) s7
        tsTranscript s8 `shouldBe`
          [ TiUser "Read config"
          , TiAssistant "Reading config file"
          , TiToolCard (ToolCard "c1" "read_file" "config.json" (Finished (ToolSuccess "{\"port\":8080}")) False)
          , TiAssistant "Now starting server"
          , TiToolCard (ToolCard "c2" "run_command" "serve" (Finished (ToolSuccess "Server started")) False)
          , TiAssistant "All operations complete."
          ]

      it "toggles tool expansion on Enter and Space when FocusTranscript is active" $ do
        let calls = [ToolCall "c1" "run_command" "echo hi"]
            s0 = fst $ updateTui (EvHarness (EvLLMResponse (Just "reply") calls Nothing)) baseState
            transcriptFocus = s0 { tsFocus = FocusTranscript }
            (s1, _) = updateTui (EvUserKey KeyEnter) transcriptFocus
        case [tc | TiToolCard tc <- tsTranscript s1] of
          [card] -> tcExpanded card `shouldBe` True
          _      -> expectationFailure "Expected tool card"
        let (s2, _) = updateTui (EvUserKey (KeyChar ' ')) s1
        case [tc | TiToolCard tc <- tsTranscript s2] of
          [card] -> tcExpanded card `shouldBe` False
          _      -> expectationFailure "Expected tool card"

      it "moves and clamps tool card selection on Up and Down in FocusTranscript" $ do
        let tool1 = TiToolCard (ToolCard "c1" "read_file" "{}" Pending False)
            tool2 = TiToolCard (ToolCard "c2" "write_file" "{}" Pending False)
            tool3 = TiToolCard (ToolCard "c3" "run_command" "{}" Pending False)
            s0 = baseState { tsFocus = FocusTranscript, tsTranscript = [tool1, tool2, tool3], tsSelectedToolIndex = 1 }
            (sUp, aUp) = updateTui (EvUserKey KeyUp) s0
        tsSelectedToolIndex sUp `shouldBe` 0
        aUp `shouldBe` []
        let (sUp2, aUp2) = updateTui (EvUserKey KeyUp) sUp
        tsSelectedToolIndex sUp2 `shouldBe` 0
        aUp2 `shouldBe` []
        let (sDown, aDown) = updateTui (EvUserKey KeyDown) s0
        tsSelectedToolIndex sDown `shouldBe` 2
        aDown `shouldBe` []
        let (sDown2, aDown2) = updateTui (EvUserKey KeyDown) sDown
        tsSelectedToolIndex sDown2 `shouldBe` 2
        aDown2 `shouldBe` []

      it "scrolls transcript one line on Up and Down in FocusTranscript when no tool cards exist" $ do
        let s0 = baseState { tsFocus = FocusTranscript, tsTranscript = [TiUser "hi", TiAssistant "hello"] }
            (sDown, aDown) = updateTui (EvUserKey KeyDown) s0
        aDown `shouldBe` [ActionScrollTranscript 1]
        let (_sUp, aUp) = updateTui (EvUserKey KeyUp) sDown
        aUp `shouldBe` [ActionScrollTranscript (-1)]

      it "scrolls transcript 5 lines on PgUp and PgDn in FocusTranscript" $ do
        let s0 = baseState { tsFocus = FocusTranscript }
            (sDown, aDown) = updateTui (EvUserKey KeyPageDown) s0
        aDown `shouldBe` [ActionScrollTranscript 5]
        let (_sUp, aUp) = updateTui (EvUserKey KeyPageUp) sDown
        aUp `shouldBe` [ActionScrollTranscript (-5)]

      it "clears transcript and resets context token counters on 'c' in FocusTranscript" $ do
        let tool1 = TiToolCard (ToolCard "c1" "read_file" "{}" Pending False)
            s0 = baseState
              { tsFocus = FocusTranscript
              , tsTranscript = [TiUser "hello", tool1]
              , tsSelectedToolIndex = 1
              , tsTranscriptScroll = 10
              , tsContextTokens = 1500
              , tsTokenUsage = Just (mkTokenUsage 100 200 300)
              }
            (s1, a1) = updateTui (EvUserKey (KeyChar 'c')) s0
        tsTranscript s1 `shouldBe` []
        tsSelectedToolIndex s1 `shouldBe` 0
        tsTranscriptScroll s1 `shouldBe` 0
        tsContextTokens s1 `shouldBe` 0
        tsTokenUsage s1 `shouldBe` Nothing
        a1 `shouldBe` []

      it "quits on 'q' when FocusTranscript is active and agent is idle" $ do
        let s0 = baseState { tsFocus = FocusTranscript, tsStatus = StatusIdle }
            (s1, actions) = updateTui (EvUserKey (KeyChar 'q')) s0
        tsShouldQuit s1 `shouldBe` True
        actions `shouldBe` [ActionQuit]

      it "does not quit on 'q' when FocusTranscript is active but agent is busy" $ do
        let s0 = baseState { tsFocus = FocusTranscript, tsStatus = StatusThinking }
            (s1, actions) = updateTui (EvUserKey (KeyChar 'q')) s0
        tsShouldQuit s1 `shouldBe` False
        actions `shouldBe` []

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
      it "scrolls transcript on KeyPageUp and KeyPageDown even when focused in FocusInput" $ do
        let sInput = baseState { tsFocus = FocusInput }
            (_, actionsUp) = updateTui (EvUserKey KeyPageUp) sInput
            (_, actionsDown) = updateTui (EvUserKey KeyPageDown) sInput
        actionsUp `shouldBe` [ActionScrollTranscript (-5)]
        actionsDown `shouldBe` [ActionScrollTranscript 5]

      it "scrolls transcript on mouse wheel KeyScrollUp and KeyScrollDown in FocusInput" $ do
        let sInput = baseState { tsFocus = FocusInput }
            (_, actionsUp) = updateTui (EvUserKey KeyScrollUp) sInput
            (_, actionsDown) = updateTui (EvUserKey KeyScrollDown) sInput
        actionsUp `shouldBe` [ActionScrollTranscript (-2)]
        actionsDown `shouldBe` [ActionScrollTranscript 2]

    describe "Headless Render Tests" $ do
      it "renders user text, assistant text and tool cards with strictly increasing row indices in transcript order" $ do
        let card = ToolCard "call-1" "read_file" "{\"path\":\"foo.txt\"}" (Finished (ToolSuccess "file contents")) False
            st = baseState
              { tsTranscript =
                  [ TiUser "Show me foo.txt"
                  , TiAssistant "Here is the file"
                  , TiToolCard card
                  ]
              }
            rows = renderTestRows st (100, 30)
            findRow needle = case [idx | (idx, r) <- zip [0 :: Int ..] rows, needle `T.isInfixOf` r] of
              (i:_) -> Just i
              []    -> Nothing
        let mUserRow = findRow "Show me foo.txt"
            mAsstRow = findRow "Here is the file"
            mToolRow = findRow "read_file"
        case (mUserRow, mAsstRow, mToolRow) of
          (Just userRow, Just asstRow, Just toolRow) -> do
            userRow `shouldSatisfy` (< asstRow)
            asstRow `shouldSatisfy` (< toolRow)
          _ -> expectationFailure "Expected user, assistant, and tool card rows in order"

      it "shows output when tool card is expanded and only summary when collapsed" $ do
        let cardCollapsed = ToolCard "c1" "read_file" "{\"path\":\"a\"}" (Finished (ToolSuccess "SECRET_PAYLOAD_12345")) False
            cardExpanded  = ToolCard "c1" "read_file" "{\"path\":\"a\"}" (Finished (ToolSuccess "SECRET_PAYLOAD_12345")) True
            stCollapsed = baseState { tsTranscript = [TiToolCard cardCollapsed] }
            stExpanded  = baseState { tsTranscript = [TiToolCard cardExpanded] }
            rowsCollapsed = renderTestRows stCollapsed (100, 30)
            rowsExpanded  = renderTestRows stExpanded (100, 30)
        any ("✓ success" `T.isInfixOf`) rowsCollapsed `shouldBe` True
        any ("SECRET_PAYLOAD_12345" `T.isInfixOf`) rowsCollapsed `shouldBe` False
        any ("SECRET_PAYLOAD_12345" `T.isInfixOf`) rowsExpanded `shouldBe` True

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

      it "leaves history browse mode when the recalled prompt is edited" $ do
        let s0 = baseState { tsPromptHistory = ["alpha", "beta"], tsInputBuffer = "draft" }
            (s1, _) = updateTui (EvUserKey KeyUp) s0
            (s2, _) = updateTui (EvUserKey (KeyChar 'x')) s1
            (s3, _) = updateTui (EvUserKey KeyDown) s2
        tsInputBuffer s2 `shouldBe` "betax"
        tsPromptHistoryIndex s2 `shouldBe` Nothing
        tsInputBuffer s3 `shouldBe` "betax"

      it "accepting Tab completion after Up leaves history browse mode" $ do
        let s0 = baseState { tsPromptHistory = ["alpha", "/cle"], tsInputBuffer = "draft" }
            (s1, _) = updateTui (EvUserKey KeyUp) s0
            (s2, _) = updateTui (EvUserKey KeyTab) s1
        tsInputBuffer s1 `shouldBe` "/cle"
        tsPromptHistoryIndex s1 `shouldBe` Just 1
        tsInputBuffer s2 `shouldBe` "/clear"
        tsPromptHistoryIndex s2 `shouldBe` Nothing
        tsFocus s2 `shouldBe` FocusInput

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

    describe "Context Window and Session Token Usage Tracking" $ do
      it "initializes context tokens to 0, token usage to Nothing, and session tokens to 0" $ do
        tsContextTokens baseState `shouldBe` 0
        tsTokenUsage baseState `shouldBe` Nothing
        tsSessionTokens baseState `shouldBe` initialSessionTokenUsage
        tsUsageStatus baseState `shouldBe` UsageVerified

      it "updates context tokens and session tokens on EvLLMResponse" $ do
        let usage = mkTokenUsage 120 30 150
            (s1, _) = updateTui (EvHarness (EvLLMResponse (Just "Hello") [] (Just usage))) baseState
        tsContextTokens s1 `shouldBe` 150
        tsTokenUsage s1 `shouldBe` Just usage
        stuTotalTokens (tsSessionTokens s1) `shouldBe` 150
        stuPromptTokens (tsSessionTokens s1) `shouldBe` 120
        stuCompletionTokens (tsSessionTokens s1) `shouldBe` 30
        tsUsageStatus s1 `shouldBe` UsageVerified

      it "accumulates session tokens while context tokens tracks latest turn" $ do
        let usage1 = mkTokenUsage 120 30 150
            usage2 = mkTokenUsage 200 45 245
            (s1, _) = updateTui (EvHarness (EvLLMResponse Nothing [] (Just usage1))) baseState
            (s2, _) = updateTui (EvHarness (EvLLMResponse (Just "Done") [] (Just usage2))) s1
        tsContextTokens s2 `shouldBe` 245
        tsTokenUsage s2 `shouldBe` Just usage2
        stuTotalTokens (tsSessionTokens s2) `shouldBe` 395
        stuPromptTokens (tsSessionTokens s2) `shouldBe` 320
        stuCompletionTokens (tsSessionTokens s2) `shouldBe` 75

      it "accumulates prompt cache metrics and monetary cost across turns" $ do
        let usage1 = TokenUsage 100 20 120 80 (Just 0.0015)
            usage2 = TokenUsage 200 30 230 150 (Just 0.0025)
            (s1, _) = updateTui (EvHarness (EvLLMResponse Nothing [] (Just usage1))) baseState
            (s2, _) = updateTui (EvHarness (EvLLMResponse (Just "Done") [] (Just usage2))) s1
        stuCachedTokens (tsSessionTokens s2) `shouldBe` 230
        case stuTotalCost (tsSessionTokens s2) of
          Just c  -> c `shouldSatisfy` (\v -> abs (v - 0.0040) < 0.00001)
          Nothing -> expectationFailure "Expected cumulative total cost to be present"

      it "marks UsageMissing when EvLLMResponse arrives without usage, preserving context count" $ do
        let usage = mkTokenUsage 120 30 150
            (s1, _) = updateTui (EvHarness (EvLLMResponse (Just "Hello") [] (Just usage))) baseState
            (s2, _) = updateTui (EvHarness (EvLLMResponse (Just "Follow-up") [] Nothing)) s1
        tsContextTokens s2 `shouldBe` 150
        tsUsageStatus s2 `shouldBe` UsageMissing

      it "accumulates goal evaluation tokens into session tokens without polluting context tokens" $ do
        let usage1 = mkTokenUsage 120 30 150
            evalUsage = mkTokenUsage 500 50 550
            (s1, _) = updateTui (EvHarness (EvLLMResponse (Just "Task done") [] (Just usage1))) baseState
            (s2, _) = updateTui (EvHarness (EvGoalEvaluationUsage evalUsage)) s1
        tsContextTokens s2 `shouldBe` 150
        stuTotalTokens (tsSessionTokens s2) `shouldBe` 700
        stuEvaluationTokens (tsSessionTokens s2) `shouldBe` 550

      it "resets context tokens to 0 when history is cleared with 'c' while preserving session totals" $ do
        let usage = mkTokenUsage 120 30 150
            (s1, _) = updateTui (EvHarness (EvLLMResponse (Just "Hello") [] (Just usage))) baseState
            s2 = s1 { tsFocus = FocusHistory }
            (s3, _) = updateTui (EvUserKey (KeyChar 'c')) s2
        tsContextTokens s3 `shouldBe` 0
        tsTokenUsage s3 `shouldBe` Nothing
        stuTotalTokens (tsSessionTokens s3) `shouldBe` 150

      it "resets context tokens to 0 on /clear while preserving session totals" $ do
        let usage = mkTokenUsage 120 30 150
            (s1, _) = updateTui (EvHarness (EvLLMResponse (Just "Hello") [] (Just usage))) baseState
            (s2, _) = updateTui (EvSubmit "/clear") s1
        tsContextTokens s2 `shouldBe` 0
        tsTokenUsage s2 `shouldBe` Nothing
        stuTotalTokens (tsSessionTokens s2) `shouldBe` 150

      it "resets session tokens to 0 on /cost reset" $ do
        let usage = mkTokenUsage 120 30 150
            (s1, _) = updateTui (EvHarness (EvLLMResponse (Just "Hello") [] (Just usage))) baseState
            (s2, _) = updateTui (EvSubmit "/cost reset") s1
        stuTotalTokens (tsSessionTokens s2) `shouldBe` 0

      it "generates an informative breakdown with context, session, and capacity on /cost" $ do
        let usage = TokenUsage 120 30 150 50 (Just 0.0012)
            (s1, _) = updateTui (EvHarness (EvLLMResponse (Just "Hello") [] (Just usage))) baseState
            (s2, _) = updateTui (EvSubmit "/cost") s1
        let notices = [ m | DiNotice m <- tsHistory s2 ]
        notices `shouldSatisfy` (\l -> any ("Tokens: 150 in context window" `T.isInfixOf`) l)
        notices `shouldSatisfy` (\l -> any (", $0.0012) —" `T.isInfixOf`) l)
        notices `shouldSatisfy` (\l -> any ("Session Cumulative: 150 tokens" `T.isInfixOf`) l)
        notices `shouldSatisfy` (\l -> any ("Reported API Cost: $0.0012" `T.isInfixOf`) l)

    describe "formatCompactLimit" $ do
      it "formats zero and small numbers directly" $ do
        formatCompactLimit 0 `shouldBe` "0"
        formatCompactLimit 500 `shouldBe` "500"

      it "formats thousands with k" $ do
        formatCompactLimit 8400 `shouldBe` "8.4k"
        formatCompactLimit 128000 `shouldBe` "128k"
        formatCompactLimit 200000 `shouldBe` "200k"

      it "formats millions with M" $ do
        formatCompactLimit 1000000 `shouldBe` "1M"
        formatCompactLimit 1500000 `shouldBe` "1.5M"

    describe "modelContextLimit and contextSaturationPercent" $ do
      it "maps known model families to context limits" $ do
        modelContextLimit "anthropic/claude-3.7-sonnet" `shouldBe` 200000
        modelContextLimit "openai/gpt-4o" `shouldBe` 128000
        modelContextLimit "google/gemini-2.0-flash" `shouldBe` 1000000
        modelContextLimit "unknown-custom-model" `shouldBe` 128000

      it "calculates context saturation percentage accurately" $ do
        contextSaturationPercent 160000 "anthropic/claude-3.7-sonnet" `shouldBe` 80
        contextSaturationPercent 115200 "openai/gpt-4o" `shouldBe` 90

      it "clamps context saturation percentage strictly between 0 and 100" $ do
        contextSaturationPercent 300000 "anthropic/claude-3.7-sonnet" `shouldBe` 100
        contextSaturationPercent (-50) "openai/gpt-4o" `shouldBe` 0


    describe "formatTokens" $ do
      it "formats small counts without commas" $ do
        formatTokens 0 `shouldBe` "0"
        formatTokens 42 `shouldBe` "42"
        formatTokens 999 `shouldBe` "999"

      it "formats thousands and larger counts with commas" $ do
        formatTokens 1000 `shouldBe` "1,000"
        formatTokens 1520 `shouldBe` "1,520"
        formatTokens 128450 `shouldBe` "128,450"

      it "does not overflow on minBound (abs minBound == minBound in Int)" $ do
        formatTokens (minBound :: Int) `shouldBe` "-9,223,372,036,854,775,808"

      it "formats small negative counts" $ do
        formatTokens (-5) `shouldBe` "-5"
        formatTokens (-1500) `shouldBe` "-1,500"

    describe "renderMaxTurns" $ do
      it "shows the infinity sign when turns are unlimited (Nothing)" $ do
        renderMaxTurns Nothing `shouldBe` "∞"

      it "shows the numeric limit when turns are capped" $ do
        renderMaxTurns (Just 10) `shouldBe` "10"
        renderMaxTurns (Just 42) `shouldBe` "42"

      it "renders the full header turn display as 0/∞ for the unlimited default" $ do
        let unlimitedState = initialTuiState "m" Nothing
            headerTurnText = T.pack (show (tsCurrentTurn unlimitedState)) <> "/" <> renderMaxTurns (tsMaxTurns unlimitedState)
        headerTurnText `shouldBe` "0/∞"

    describe "Local Slash Commands and Skill Invocations" $ do
      it "handles /clear locally by emptying dialogue history without running agent" $ do
        let s0 = baseState { tsTranscript = [DiUser "Hello"], tsInputBuffer = "/clear" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        tsTranscript s1 `shouldBe` []
        tsInputBuffer s1 `shouldBe` ""
        actions `shouldBe` []

      it "cancels running agent turn when /clear is submitted while busy" $ do
        let s0 = baseState { tsStatus = StatusThinking, tsTranscript = [DiUser "Hello"], tsInputBuffer = "/clear" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        tsTranscript s1 `shouldBe` []
        tsInputBuffer s1 `shouldBe` ""
        tsStatus s1 `shouldBe` StatusIdle
        actions `shouldBe` [ActionCancelAgent]

      it "drops stale EvDone after /clear while busy" $ do
        let s0 = baseState { tsStatus = StatusThinking, tsTranscript = [DiUser "Hello"], tsInputBuffer = "/clear" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
            (s2, _) = updateTui (EvHarness (EvDone "stale answer")) s1
        tsTranscript s2 `shouldBe` []

      it "drops stale EvToolCall after /clear while busy" $ do
        let s0 = baseState { tsStatus = StatusThinking, tsTranscript = [DiUser "Hello"], tsInputBuffer = "/clear" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
            (s2, _) = updateTui (EvHarness (EvToolCall "read_file" "{}")) s1
        tsTools s2 `shouldBe` []

      it "drops stale EvError after /clear while busy" $ do
        let s0 = baseState { tsStatus = StatusThinking, tsTranscript = [DiUser "Hello"], tsInputBuffer = "/clear" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
            (s2, _) = updateTui (EvHarness (EvError "stale error")) s1
        tsTranscript s2 `shouldBe` []

      it "resets tsCancelRequested when user submits a new prompt" $ do
        let s0 = baseState { tsStatus = StatusThinking, tsTranscript = [DiUser "Hello"], tsInputBuffer = "/clear" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
            s2 = s1 { tsInputBuffer = "new question" }
            (s3, _) = updateTui (EvUserKey KeyEnter) s2
        tsCancelRequested s3 `shouldBe` False
        tsTranscript s3 `shouldBe` [DiUser "new question"]

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
        let skillA = mkSkill "to-spec" "Generate spec" "Spec rules here" "/p" SkillGlobal
            s0 = baseState
              { tsSkills = Map.fromList [("to-spec", skillA)]
              , tsInputBuffer = "/to-spec create auth"
              }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        actions `shouldBe` [ActionRunAgent "/to-spec create auth"]
        tsHistory s1 `shouldSatisfy` \h ->
          any (\case DiUser u -> "/to-spec create auth" `T.isInfixOf` u; _ -> False) h
          && any (\case DiNotice n -> "Activated skill: to-spec" `T.isInfixOf` n; _ -> False) h

      it "does not duplicate user message in dialogueToMessages when skills or notices are in history" $ do
        let items = [DiUser "/to-spec auth", DiNotice "Activated skill: to-spec"]
            msgs = dialogueToMessages "system prompt" "expanded <skill> auth" items
        msgs `shouldBe` [SystemMsg "system prompt", UserMsg "expanded <skill> auth"]

    describe "Transcript Context Round-Tripping (Issue #24)" $ do
      it "rebuilds messages from transcript with user text, assistant text, cards in every lifecycle state and notices" $ do
        let cFinished = ToolCard "call_1" "read_file" "{\"path\":\"foo.txt\"}" (Finished (ToolSuccess "file contents")) False
            cDenied   = ToolCard "call_2" "write_file" "{\"path\":\"bar.txt\"}" (Denied "Permission denied by policy") False
            cPending  = ToolCard "call_3" "bash" "{\"cmd\":\"ls\"}" Pending False
            cRunning  = ToolCard "call_4" "bash" "{\"cmd\":\"pwd\"}" Running False
            cCancelled = ToolCard "call_5" "browser" "{}" Cancelled False
            items =
              [ TiUser "first user message"
              , TiNotice "Notice: skill activated"
              , TiAssistant "Let me check that for you."
              , TiToolCard cFinished
              , TiNotice "Notice: tool running"
              , TiToolCard cDenied
              , TiToolCard cPending
              , TiToolCard cRunning
              , TiToolCard cCancelled
              , TiNotice "Notice: turn complete"
              ]
            msgs = dialogueToMessages "system prompt" "follow-up prompt" items
            expected =
              [ SystemMsg "system prompt"
              , UserMsg "first user message"
              , AssistantMsg (Just "Let me check that for you.")
                  [ ToolCall "call_1" "read_file" "{\"path\":\"foo.txt\"}"
                  , ToolCall "call_2" "write_file" "{\"path\":\"bar.txt\"}"
                  , ToolCall "call_3" "bash" "{\"cmd\":\"ls\"}"
                  , ToolCall "call_4" "bash" "{\"cmd\":\"pwd\"}"
                  , ToolCall "call_5" "browser" "{}"
                  ]
              , ToolMsg "call_1" "read_file" "file contents"
              , ToolMsg "call_2" "write_file" "Permission denied by policy"
              , ToolMsg "call_3" "bash" cancelledToolCallPlaceholder
              , ToolMsg "call_4" "bash" cancelledToolCallPlaceholder
              , ToolMsg "call_5" "browser" cancelledToolCallPlaceholder
              , UserMsg "follow-up prompt"
              ]
        msgs `shouldBe` expected
        transcriptToMessages "system prompt" "follow-up prompt" items `shouldBe` expected

      it "yields an assistant message with calls when tool cards stand alone with assistant text blank" $ do
        let c1 = ToolCard "call_1" "read_file" "{\"path\":\"a.txt\"}" (Finished (ToolSuccess "alpha")) False
            c2 = ToolCard "call_2" "read_file" "{\"path\":\"b.txt\"}" (Finished (ToolSuccess "beta")) False
            itemsStandalone =
              [ TiUser "read files"
              , TiToolCard c1
              , TiToolCard c2
              ]
            msgsStandalone = dialogueToMessages "sys" "next turn" itemsStandalone
            expectedStandalone =
              [ SystemMsg "sys"
              , UserMsg "read files"
              , AssistantMsg Nothing
                  [ ToolCall "call_1" "read_file" "{\"path\":\"a.txt\"}"
                  , ToolCall "call_2" "read_file" "{\"path\":\"b.txt\"}"
                  ]
              , ToolMsg "call_1" "read_file" "alpha"
              , ToolMsg "call_2" "read_file" "beta"
              , UserMsg "next turn"
              ]
        msgsStandalone `shouldBe` expectedStandalone

        let itemsBlankAssistant =
              [ TiUser "read files"
              , TiAssistant ""
              , TiToolCard c1
              ]
            msgsBlankAssistant = dialogueToMessages "sys" "next turn" itemsBlankAssistant
            expectedBlankAssistant =
              [ SystemMsg "sys"
              , UserMsg "read files"
              , AssistantMsg Nothing [ToolCall "call_1" "read_file" "{\"path\":\"a.txt\"}"]
              , ToolMsg "call_1" "read_file" "alpha"
              , UserMsg "next turn"
              ]
        msgsBlankAssistant `shouldBe` expectedBlankAssistant

      it "produces placeholder tool message for unresolved cards and denial text for denied cards" $ do
        let cPending   = ToolCard "c_p" "bash" "ls" Pending False
            cRunning   = ToolCard "c_r" "bash" "pwd" Running False
            cCancelled = ToolCard "c_c" "bash" "top" Cancelled False
            cDenied    = ToolCard "c_d" "rm" "rf" (Denied "Policy violation: forbidden command") False
            cFinishedErr = ToolCard "c_fe" "grep" "foo" (Finished (ToolError "file not found")) False
            items =
              [ TiToolCard cPending
              , TiToolCard cRunning
              , TiToolCard cCancelled
              , TiToolCard cDenied
              , TiToolCard cFinishedErr
              ]
            msgs = dialogueToMessages "sys" "prompt" items
            expected =
              [ SystemMsg "sys"
              , AssistantMsg Nothing
                  [ ToolCall "c_p" "bash" "ls"
                  , ToolCall "c_r" "bash" "pwd"
                  , ToolCall "c_c" "bash" "top"
                  , ToolCall "c_d" "rm" "rf"
                  , ToolCall "c_fe" "grep" "foo"
                  ]
              , ToolMsg "c_p" "bash" "Tool call was cancelled before completion."
              , ToolMsg "c_r" "bash" "Tool call was cancelled before completion."
              , ToolMsg "c_c" "bash" "Tool call was cancelled before completion."
              , ToolMsg "c_d" "rm" "Policy violation: forbidden command"
              , ToolMsg "c_fe" "grep" "Error: file not found"
              , UserMsg "prompt"
              ]
        msgs `shouldBe` expected

      it "never contains an assistant tool call without a following tool message with the same id (property test)" $
        property $ \items (NonEmpty sysPrompt) (NonEmpty currentPrompt) ->
          let msgs = dialogueToMessages (T.pack sysPrompt) (T.pack currentPrompt) items
              checkCallsAnswered [] = True
              checkCallsAnswered (AssistantMsg _ calls : rest) =
                let subsequentToolIds = [cid | ToolMsg cid _ _ <- rest]
                    allPresent = all (\tc -> callId tc `elem` subsequentToolIds) calls
                in allPresent && checkCallsAnswered rest
              checkCallsAnswered (_ : rest) = checkCallsAnswered rest
          in checkCallsAnswered msgs

      it "builds context via dialogueToMessages for normal run and preserves prior tool results" $ do
        let cFinished = ToolCard "c1" "read_file" "{\"path\":\"README.md\"}" (Finished (ToolSuccess "Project documentation")) False
            priorHistory =
              [ TiUser "read README.md"
              , TiAssistant "I will read the file."
              , TiToolCard cFinished
              ]
            msgs = dialogueToMessages "system prompt" "summarise what you just read" priorHistory
        msgs `shouldBe`
          [ SystemMsg "system prompt"
          , UserMsg "read README.md"
          , AssistantMsg (Just "I will read the file.") [ToolCall "c1" "read_file" "{\"path\":\"README.md\"}"]
          , ToolMsg "c1" "read_file" "Project documentation"
          , UserMsg "summarise what you just read"
          ]

      it "builds context via dialogueToMessages for goal run and passes tool results to loop" $ do
        historySeenRef <- newIORef ([] :: [Message])
        mockIOEnv <- newIOEnv "test" "test-model" "/tmp" False
        let promptAction msgs _ = do
              writeIORef historySeenRef msgs
              pure (Right (AssistantResponse (Just "Goal achieved successfully.") [] Nothing))
            evalAction _ _ = pure (GoalEvaluation GoalMet "All conditions satisfied")
            mockAlgebra = (ioAlgebra mockIOEnv)
              { interpPrompt   = promptAction
              , interpTool     = \_ -> pure (ToolSuccess "ok")
              , interpLog      = \_ -> pure ()
              , interpEvaluate = evalAction
              }
            config = goalAgentConfig mockIOEnv "system prompt" Nothing
            cFinished = ToolCard "call_g1" "read_file" "{}" (Finished (ToolSuccess "sample goal data")) False
            historyItems =
              [ TiUser "fetch information"
              , TiAssistant "fetching"
              , TiToolCard cFinished
              ]
        runGoalWorker mockAlgebra config "All conditions satisfied" historyItems (\_ -> pure ())
        historySeen <- readIORef historySeenRef
        historySeen `shouldBe`
          [ SystemMsg "system prompt"
          , UserMsg "fetch information"
          , AssistantMsg (Just "fetching") [ToolCall "call_g1" "read_file" "{}"]
          , ToolMsg "call_g1" "read_file" "sample goal data"
          , UserMsg "All conditions satisfied"
          ]

    describe "Transcript Auto-Scroll Policy and Thinking Indicator (Issue #25)" $ do
      describe "Auto-Scroll Policy (shouldAutoScroll)" $ do
        it "produces a scroll-to-end for appending events when focus is on the prompt input" $ do
          let inputState = baseState { tsFocus = FocusInput }
              appendingEvents =
                [ EvLLMResponse (Just "hello") [] Nothing
                , EvDone "finished answer"
                , EvError "fatal error"
                , EvToolCall "bash" "ls -la"
                , EvToolResult "bash" (ToolSuccess "ok")
                , EvPermissionDenied "write_file" "protected path"
                , EvHookTriggered "PreToolUse" "ok"
                , EvSessionSaved "sess.jsonl"
                , EvNotificationSent "done"
                , EvGoalEvaluated GoalMet "condition met"
                , EvGoalAchieved "all tests pass"
                , EvGoalFailed "condition" "failure"
                , EvGoalBlocked "blocked"
                ]
          forM_ appendingEvents $ \ev -> do
            isTranscriptAppendingEvent ev `shouldBe` True
            shouldAutoScroll inputState ev `shouldBe` True

        it "does not produce a scroll-to-end for appending events when focus is on the transcript" $ do
          let transcriptState = baseState { tsFocus = FocusTranscript }
              appendingEvents =
                [ EvLLMResponse (Just "hello") [] Nothing
                , EvDone "finished answer"
                , EvError "fatal error"
                , EvToolCall "bash" "ls -la"
                , EvToolResult "bash" (ToolSuccess "ok")
                , EvPermissionDenied "write_file" "protected path"
                , EvHookTriggered "PreToolUse" "ok"
                , EvSessionSaved "sess.jsonl"
                , EvNotificationSent "done"
                , EvGoalEvaluated GoalMet "condition met"
                , EvGoalAchieved "all tests pass"
                , EvGoalFailed "condition" "failure"
                , EvGoalBlocked "blocked"
                ]
          forM_ appendingEvents $ \ev -> do
            isTranscriptAppendingEvent ev `shouldBe` True
            shouldAutoScroll transcriptState ev `shouldBe` False

        it "does not produce a scroll-to-end for non-appending events in either focus mode" $ do
          let inputState = baseState { tsFocus = FocusInput }
              transcriptState = baseState { tsFocus = FocusTranscript }
              nonAppendingEvents =
                [ EvTurnComplete 1
                , EvGoalSet "condition"
                , EvToolCallDelta "delta"
                ]
          forM_ nonAppendingEvents $ \ev -> do
            isTranscriptAppendingEvent ev `shouldBe` False
            shouldAutoScroll inputState ev `shouldBe` False
            shouldAutoScroll transcriptState ev `shouldBe` False

      describe "Thinking Indicator Headless Render" $ do
        it "renders the thinking line under the last item when status is thinking" $ do
          let thinkingState = baseState
                { tsStatus = StatusThinking
                , tsTranscript =
                    [ TiUser "analyze repo"
                    , TiAssistant "I am analyzing."
                    ]
                }
              rows = renderTestRows thinkingState (120, 40)
          any ("Thinking..." `T.isInfixOf`) rows `shouldBe` True

          let findRow needle = case [idx | (idx, r) <- zip [0 :: Int ..] rows, needle `T.isInfixOf` r] of
                (i:_) -> Just i
                []    -> Nothing
              mUserRow = findRow "analyze repo"
              mAsstRow = findRow "I am analyzing."
              mThinkingRow = findRow "Thinking..."
          case (mUserRow, mAsstRow, mThinkingRow) of
            (Just uRow, Just aRow, Just tRow) -> do
              uRow `shouldSatisfy` (< aRow)
              aRow `shouldSatisfy` (< tRow)
            _ ->
              expectationFailure "Expected User, Assistant, and Thinking line in rows in order"

        it "does not render the thinking line when status is idle or finished" $ do
          let idleState = baseState
                { tsStatus = StatusIdle
                , tsTranscript = [TiUser "hello", TiAssistant "world"]
                }
              finishedState = baseState
                { tsStatus = StatusFinished
                , tsTranscript = [TiUser "hello", TiAssistant "world"]
                }
          any ("Thinking..." `T.isInfixOf`) (renderTestRows idleState (120, 40)) `shouldBe` False
          any ("Thinking..." `T.isInfixOf`) (renderTestRows finishedState (120, 40)) `shouldBe` False

        it "renders the thinking line even when transcript has no items yet" $ do
          let emptyThinkingState = baseState
                { tsStatus = StatusThinking
                , tsTranscript = []
                }
              rows = renderTestRows emptyThinkingState (120, 40)
          any ("Thinking..." `T.isInfixOf`) rows `shouldBe` True

    describe "/goal command" $ do
      it "sets a goal and emits ActionRunGoal with the condition" $ do
        let s0 = baseState { tsInputBuffer = "/goal all tests pass" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        tsInputBuffer s1 `shouldBe` ""
        tsStatus s1 `shouldBe` StatusThinking
        actions `shouldBe` [ActionRunGoal "all tests pass"]
        tsGoalState s1 `shouldSatisfy` \case
          Just gs -> gsCondition gs == "all tests pass" && gsStatus gs == GoalActive
          Nothing -> False

      it "adds a goal-set notice to history" $ do
        let s0 = baseState { tsInputBuffer = "/goal all tests pass" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsHistory s1 `shouldSatisfy` \h ->
          any (\case DiNotice msg -> "Goal set: all tests pass" `T.isInfixOf` msg; _ -> False) h

      it "shows goal status when /goal is typed with no args" $ do
        let gs = initialGoalState "all tests pass"
            s0 = baseState { tsInputBuffer = "/goal", tsGoalState = Just gs }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        actions `shouldBe` []
        tsHistory s1 `shouldSatisfy` \h ->
          any (\case DiNotice msg -> "Goal active: all tests pass" `T.isInfixOf` msg; _ -> False) h

      it "shows no goal set when /goal is typed with no active goal" $ do
        let s0 = baseState { tsInputBuffer = "/goal" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        actions `shouldBe` []
        tsHistory s1 `shouldSatisfy` \h ->
          any (\case DiNotice msg -> "No goal set" `T.isInfixOf` msg; _ -> False) h

      it "clears the goal with /goal clear and shows confirmation" $ do
        let gs = initialGoalState "all tests pass"
            s0 = baseState { tsInputBuffer = "/goal clear", tsGoalState = Just gs }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        actions `shouldBe` []
        tsGoalState s1 `shouldSatisfy` \case
          Just gs' -> gsStatus gs' == GoalCleared
          Nothing -> False
        tsHistory s1 `shouldSatisfy` \h ->
          any (\case DiNotice msg -> "Goal cleared: all tests pass" `T.isInfixOf` msg; _ -> False) h

      it "clears the goal when multiple spaces precede clear (e.g. /goal  clear)" $ do
        let gs = initialGoalState "all tests pass"
            s0 = baseState { tsInputBuffer = "/goal  clear", tsGoalState = Just gs }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        actions `shouldBe` []
        tsGoalState s1 `shouldSatisfy` \case
          Just gs' -> gsStatus gs' == GoalCleared
          Nothing -> False
        tsHistory s1 `shouldSatisfy` \h ->
          any (\case DiNotice msg -> "Goal cleared: all tests pass" `T.isInfixOf` msg; _ -> False) h

      it "supports stop as an alias for clear" $ do
        let gs = initialGoalState "my goal"
            s0 = baseState { tsInputBuffer = "/goal stop", tsGoalState = Just gs }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsGoalState s1 `shouldSatisfy` \case
          Just gs' -> gsStatus gs' == GoalCleared
          Nothing -> False

      it "supports off as an alias for clear" $ do
        let gs = initialGoalState "my goal"
            s0 = baseState { tsInputBuffer = "/goal off", tsGoalState = Just gs }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsGoalState s1 `shouldSatisfy` \case
          Just gs' -> gsStatus gs' == GoalCleared
          Nothing -> False

      it "supports reset as an alias for clear" $ do
        let gs = initialGoalState "my goal"
            s0 = baseState { tsInputBuffer = "/goal reset", tsGoalState = Just gs }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsGoalState s1 `shouldSatisfy` \case
          Just gs' -> gsStatus gs' == GoalCleared
          Nothing -> False

      it "supports none as an alias for clear" $ do
        let gs = initialGoalState "my goal"
            s0 = baseState { tsInputBuffer = "/goal none", tsGoalState = Just gs }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsGoalState s1 `shouldSatisfy` \case
          Just gs' -> gsStatus gs' == GoalCleared
          Nothing -> False

      it "supports cancel as an alias for clear" $ do
        let gs = initialGoalState "my goal"
            s0 = baseState { tsInputBuffer = "/goal cancel", tsGoalState = Just gs }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsGoalState s1 `shouldSatisfy` \case
          Just gs' -> gsStatus gs' == GoalCleared
          Nothing -> False

      it "prints no goal set when clearing with no active goal" $ do
        let s0 = baseState { tsInputBuffer = "/goal clear" }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsHistory s1 `shouldSatisfy` \h ->
          any (\case DiNotice msg -> "No goal set" `T.isInfixOf` msg; _ -> False) h

      it "clears the goal when /clear is used" $ do
        let gs = initialGoalState "my goal"
            s0 = baseState { tsInputBuffer = "/clear", tsGoalState = Just gs }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsGoalState s1 `shouldBe` Nothing

      it "rejects a condition longer than 4000 characters" $ do
        let longCondition = T.replicate 4001 "x"
            s0 = baseState { tsInputBuffer = "/goal " <> longCondition }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        actions `shouldBe` []
        tsGoalState s1 `shouldBe` Nothing
        tsHistory s1 `shouldSatisfy` \h ->
          any (\case DiNotice msg -> "too long" `T.isInfixOf` msg; _ -> False) h

      it "replaces the existing goal when a new one is set" $ do
        let gs = initialGoalState "old goal"
            s0 = baseState { tsInputBuffer = "/goal new goal", tsGoalState = Just gs }
            (s1, _) = updateTui (EvUserKey KeyEnter) s0
        tsGoalState s1 `shouldSatisfy` \case
          Just gs' -> gsCondition gs' == "new goal"
          Nothing -> False

      it "does not treat /goal stop the server as a clear command" $ do
        let s0 = baseState { tsInputBuffer = "/goal stop the server" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        actions `shouldBe` [ActionRunGoal "stop the server"]
        tsGoalState s1 `shouldSatisfy` \case
          Just gs -> gsCondition gs == "stop the server"
          Nothing -> False

      it "does not treat /goal clear the cache as a clear command" $ do
        let s0 = baseState { tsInputBuffer = "/goal clear the cache" }
            (s1, actions) = updateTui (EvUserKey KeyEnter) s0
        actions `shouldBe` [ActionRunGoal "clear the cache"]
        tsGoalState s1 `shouldSatisfy` \case
          Just gs -> gsCondition gs == "clear the cache"
          Nothing -> False

    describe "Goal Event Processing" $ do
      it "sets goal state on EvGoalSet" $ do
        let s1 = fst $ updateTui (EvHarness (EvGoalSet "tests pass")) baseState
        tsGoalState s1 `shouldSatisfy` \case
          Just gs -> gsCondition gs == "tests pass" && gsStatus gs == GoalActive
          Nothing -> False

      it "updates goal state on EvGoalEvaluated" $ do
        let gs = initialGoalState "tests pass"
            s0 = baseState { tsGoalState = Just gs }
            s1 = fst $ updateTui (EvHarness (EvGoalEvaluated GoalNotYetMet "Keep going.")) s0
        case tsGoalState s1 of
          Just gs' -> do
            gsLastVerdict gs' `shouldBe` Just GoalNotYetMet
            gsLastReason gs' `shouldBe` Just "Keep going."
            gsTurnCount gs' `shouldBe` 1
          Nothing -> expectationFailure "Expected goal state"

      it "marks goal achieved on EvGoalAchieved" $ do
        let gs = initialGoalState "tests pass"
            s0 = baseState { tsGoalState = Just gs }
            s1 = fst $ updateTui (EvHarness (EvGoalAchieved "tests pass")) s0
        case tsGoalState s1 of
          Just gs' -> gsStatus gs' `shouldBe` GoalAchieved
          Nothing -> expectationFailure "Expected goal state"

      it "marks goal failed on EvGoalFailed" $ do
        let gs = initialGoalState "tests pass"
            s0 = baseState { tsGoalState = Just gs }
            s1 = fst $ updateTui (EvHarness (EvGoalFailed "tests pass" "missing framework")) s0
        case tsGoalState s1 of
          Just gs' -> gsStatus gs' `shouldBe` GoalFailed
          Nothing -> expectationFailure "Expected goal state"

      it "clears goal state on EvGoalCleared" $ do
        let gs = initialGoalState "tests pass"
            s0 = baseState { tsGoalState = Just gs }
            s1 = fst $ updateTui (EvHarness (EvGoalCleared "tests pass")) s0
        tsGoalState s1 `shouldBe` Nothing

    describe "Skill Invocation Autocomplete (Tab accepts ghost text)" $ do
      let goal = mkSkill "goal" "Goal skill" "body" "/p" SkillGlobal
          skillState = baseState { tsSkills = Map.fromList [("goal", goal)] }

      it "accepts the inline completion on Tab when a skill prefix is being typed" $ do
        let s0 = skillState { tsInputBuffer = "/go" }
            (s1, actions) = updateTui (EvUserKey KeyTab) s0
        tsInputBuffer s1 `shouldBe` "/goal"
        actions `shouldBe` []

      it "keeps focus on the input box when Tab accepts a completion" $ do
        let s0 = skillState { tsInputBuffer = "/go" }
            (s1, _) = updateTui (EvUserKey KeyTab) s0
        tsFocus s1 `shouldBe` FocusInput

      it "falls back to cycling focus on Tab when no completion is available" $ do
        let s0 = skillState { tsInputBuffer = "ordinary text" }
            (s1, _) = updateTui (EvUserKey KeyTab) s0
        tsFocus s1 `shouldBe` FocusHistory

      it "falls back to cycling focus on Tab when the slash-command is fully typed" $ do
        let s0 = skillState { tsInputBuffer = "/goal" }
            (s1, _) = updateTui (EvUserKey KeyTab) s0
        tsFocus s1 `shouldBe` FocusHistory
        tsInputBuffer s1 `shouldBe` "/goal"

      it "falls back to cycling focus on Tab when the input buffer is empty" $ do
        let (s1, _) = updateTui (EvUserKey KeyTab) skillState
        tsFocus s1 `shouldBe` FocusHistory

      it "preserves leading prompt text when accepting a completion" $ do
        let s0 = skillState { tsInputBuffer = "please run /go" }
            (s1, _) = updateTui (EvUserKey KeyTab) s0
        tsInputBuffer s1 `shouldBe` "please run /goal"

      it "cycles focus on Tab from non-input panels even with skills present" $ do
        let s0 = skillState { tsFocus = FocusTranscript, tsInputBuffer = "/go" }
            (s1, _) = updateTui (EvUserKey KeyTab) s0
        tsFocus s1 `shouldBe` FocusInput

      it "completes a built-in slash command with no skills present" $ do
        let s0 = baseState { tsInputBuffer = "/cle" }
            (s1, _) = updateTui (EvUserKey KeyTab) s0
        tsInputBuffer s1 `shouldBe` "/clear"
        tsFocus s1 `shouldBe` FocusInput

    describe "Goal Execution Unlimited Turns in TUI (Bug Repro)" $ do
      it "defaults cfgMaxTurns to Nothing (infinity) in goalAgentConfig" $ do
        mockIOEnv <- newIOEnv "test" "test-model" "/tmp" False
        cfgMaxTurns (goalAgentConfig mockIOEnv "sys" Nothing) `shouldBe` Nothing

      it "respects explicit turn cap when configured in goalAgentConfig" $ do
        mockIOEnv <- newIOEnv "test" "test-model" "/tmp" False
        cfgMaxTurns (goalAgentConfig mockIOEnv "sys" (Just 5)) `shouldBe` Just 5

      it "does not terminate with 'Maximum turns reached (20)' on turn 20 when default turn limit is infinity" $ do
        mockIOEnv <- newIOEnv "test" "test-model" "/tmp" False
        eventsRef <- newIORef []
        let mockTool = ToolCall "call_1" "read_file" "{}"
            promptAction msgs _ = do
              let toolTurns = length [() | ToolMsg{} <- msgs]
              if toolTurns < 21
                then pure $ Right (AssistantResponse Nothing [mockTool] Nothing)
                else pure $ Right (AssistantResponse (Just "All tests pass.") [] Nothing)
            evalAction _ _ = pure (GoalEvaluation GoalMet "Done.")
            mockAlgebra = (ioAlgebra mockIOEnv)
              { interpPrompt   = promptAction
              , interpTool     = \_ -> pure (ToolSuccess "ok")
              , interpLog      = \_ -> pure ()
              , interpEvaluate = evalAction
              }
            config = goalAgentConfig mockIOEnv "sys" Nothing
        runGoalWorker mockAlgebra config "All tests pass" [] (\ev -> modifyIORef' eventsRef (ev :))
        events <- readIORef eventsRef
        events `shouldNotContain` [EvError "Maximum turns reached (20)"]
        events `shouldContain` [EvDone "All tests pass."]

    describe "runGoalWorker terminal event handling (Issue #49)" $ do
      it "emits EvDone instead of EvError when goal is blocked (GoalActive)" $ do
        mockIOEnv <- newIOEnv "test" "test-model" "/tmp" False
        eventsRef <- newIORef []
        let promptAction _ _ = pure $ Right (AssistantResponse (Just "Completed progress.") [] Nothing)
            evalAction _ _ = pure (GoalEvaluation GoalNotYetMet "Need more work")
            mockAlgebra = (ioAlgebra mockIOEnv)
              { interpPrompt   = promptAction
              , interpTool     = \_ -> pure (ToolSuccess "ok")
              , interpLog      = \ev -> modifyIORef' eventsRef (ev :)
              , interpEvaluate = evalAction
              }
            config = goalAgentConfig mockIOEnv "sys" (Just 5)
        runGoalWorker mockAlgebra config "reach condition" [] (\ev -> modifyIORef' eventsRef (ev :))
        events <- readIORef eventsRef
        events `shouldNotContain` [EvError "Completed progress."]
        events `shouldContain` [EvDone "Completed progress."]

      it "emits EvDone instead of EvError when goal is impossible (GoalFailed)" $ do
        mockIOEnv <- newIOEnv "test" "test-model" "/tmp" False
        eventsRef <- newIORef []
        let promptAction _ _ = pure $ Right (AssistantResponse (Just "Goal cannot be met.") [] Nothing)
            evalAction _ _ = pure (GoalEvaluation GoalImpossible "Reason impossible")
            mockAlgebra = (ioAlgebra mockIOEnv)
              { interpPrompt   = promptAction
              , interpTool     = \_ -> pure (ToolSuccess "ok")
              , interpLog      = \ev -> modifyIORef' eventsRef (ev :)
              , interpEvaluate = evalAction
              }
            config = goalAgentConfig mockIOEnv "sys" (Just 5)
        runGoalWorker mockAlgebra config "impossible condition" [] (\ev -> modifyIORef' eventsRef (ev :))
        events <- readIORef eventsRef
        events `shouldNotContain` [EvError "Goal cannot be met."]
        events `shouldContain` [EvDone "Goal cannot be met."]


    describe "Built-in Slash Commands" $ do
      it "registers all standard built-in slash commands" $ do
        let expected =
              [ "/clear", "/help", "/cost", "/compact", "/goal", "/exit", "/quit"
              , "/model", "/config", "/context", "/resume", "/plan", "/diff"
              , "/tasks", "/theme", "/status", "/memory", "/init", "/permissions"
              , "/fewer-permission-prompts", "/doctor", "/copy", "/reload-skills"
              , "/mcp", "/plugin"
              ]
        mapM_ (\cmd -> cmd `shouldSatisfy` (`elem` builtinCommands)) expected

      it "handles /exit and /quit by emitting ActionQuit" $ do
        let (sExit, aExit) = updateTui (EvSubmit "/exit") baseState
        tsShouldQuit sExit `shouldBe` True
        aExit `shouldBe` [ActionQuit]

        let (sQuit, aQuit) = updateTui (EvSubmit "/quit") baseState
        tsShouldQuit sQuit `shouldBe` True
        aQuit `shouldBe` [ActionQuit]

      it "handles /model query and /model switch" $ do
        let (sQuery, _) = updateTui (EvSubmit "/model") baseState
        tsHistory sQuery `shouldContain` [DiNotice ("Current model: " <> tsModelName baseState)]

        let (sSwitch, _) = updateTui (EvSubmit "/model anthropic/claude-3.5-sonnet") baseState
        tsModelName sSwitch `shouldBe` "anthropic/claude-3.5-sonnet"
        tsHistory sSwitch `shouldContain` [DiNotice "Model switched to: anthropic/claude-3.5-sonnet"]

      it "keeps the next agent run's model in sync with /model (Issue #61)" $ do
        ioEnv <- newIOEnv "test" "startup-model" "/tmp" False
        let st0 = initialTuiState (ioModel ioEnv) (Just 10)
            (st1, _) = updateTui (EvSubmit "/model anthropic/claude-3.5-sonnet") st0
            runEnv = runEnvForModel (tsModelName st1) ioEnv
            cfg = goalAgentConfig runEnv "sys" (Just 10)
        tsModelName st1 `shouldBe` "anthropic/claude-3.5-sonnet"
        cfgModel cfg `shouldBe` "anthropic/claude-3.5-sonnet"
        -- The interpreter builds the wire request from the run env's model, so
        -- request and config must agree on the switched model.
        ioModel runEnv `shouldBe` cfgModel cfg

      it "handles /config" $ do
        let (s, _) = updateTui (EvSubmit "/config") baseState
        tsHistory s `shouldSatisfy` \h -> any (\case DiNotice n -> "Configuration:" `T.isInfixOf` n; _ -> False) h

      it "handles /context" $ do
        let (s, _) = updateTui (EvSubmit "/context") baseState
        tsHistory s `shouldSatisfy` \h -> any (\case DiNotice n -> "Context tokens:" `T.isInfixOf` n; _ -> False) h

      it "handles /plan mode" $ do
        let (s, _) = updateTui (EvSubmit "/plan") baseState
        tsHistory s `shouldContain` [DiNotice "Plan mode activated. Read-only actions allowed."]

      it "handles /diff" $ do
        let (s, _) = updateTui (EvSubmit "/diff") baseState
        tsHistory s `shouldContain` [DiNotice "Git working tree diff inspected."]

      it "handles /tasks" $ do
        let (s, _) = updateTui (EvSubmit "/tasks") baseState
        tsHistory s `shouldContain` [DiNotice "Task list: No active background tasks."]

      it "handles /theme" $ do
        let (s, _) = updateTui (EvSubmit "/theme") baseState
        tsHistory s `shouldContain` [DiNotice "Theme: dark"]

      it "handles /status" $ do
        let (s, _) = updateTui (EvSubmit "/status") baseState
        tsHistory s `shouldSatisfy` \h -> any (\case DiNotice n -> "Status:" `T.isInfixOf` n; _ -> False) h

      it "handles /memory and /init" $ do
        let (sMem, _) = updateTui (EvSubmit "/memory") baseState
        tsHistory sMem `shouldContain` [DiNotice "Project memory instructions active."]

        let (sInit, _) = updateTui (EvSubmit "/init") baseState
        tsHistory sInit `shouldContain` [DiNotice "Initialized CLAUDE.md guidelines template."]

      it "handles /permissions and /fewer-permission-prompts" $ do
        let (sPerm, _) = updateTui (EvSubmit "/permissions") baseState
        tsHistory sPerm `shouldContain` [DiNotice "Permissions policy: default"]

        let (sFew, _) = updateTui (EvSubmit "/fewer-permission-prompts") baseState
        tsHistory sFew `shouldContain` [DiNotice "Permissions set to acceptEdits: Auto-approving file edits."]

      it "handles /doctor, /copy, /reload-skills, /mcp, and /plugin" $ do
        let (sDoc, _) = updateTui (EvSubmit "/doctor") baseState
        tsHistory sDoc `shouldContain` [DiNotice "Doctor: All systems operational."]

        let (sCopy, _) = updateTui (EvSubmit "/copy") baseState
        tsHistory sCopy `shouldContain` [DiNotice "Last response copied to clipboard."]

        let (sReload, _) = updateTui (EvSubmit "/reload-skills") baseState
        tsHistory sReload `shouldSatisfy` \h -> any (\case DiNotice n -> "Skills reloaded:" `T.isInfixOf` n; _ -> False) h

        let (sMcp, _) = updateTui (EvSubmit "/mcp") baseState
        tsHistory sMcp `shouldContain` [DiNotice "MCP: Model Context Protocol servers loaded."]

        let (sPlug, _) = updateTui (EvSubmit "/plugin") baseState
        tsHistory sPlug `shouldContain` [DiNotice "Plugins: 0 loaded"]

    describe "CLI Initial Prompt Launch (initialTuiLaunch)" $ do
      it "dispatches ActionRunGoal when CLI initial prompt is /goal" $ do
        let (s, actions) = initialTuiLaunch (Just "/goal all tests green") baseState
        actions `shouldBe` [ActionRunGoal "all tests green"]
        case tsGoalState s of
          Just gs -> gsCondition gs `shouldBe` "all tests green"
          Nothing -> expectationFailure "Expected tsGoalState to be initialized"

      it "dispatches no actions and updates UI when CLI initial prompt is /help" $ do
        let (s, actions) = initialTuiLaunch (Just "/help") baseState
        actions `shouldBe` []
        tsShowHelp s `shouldBe` True

      it "dispatches ActionQuit when CLI initial prompt is /quit" $ do
        let (_, actions) = initialTuiLaunch (Just "/quit") baseState
        actions `shouldBe` [ActionQuit]

      it "dispatches ActionRunAgent when CLI initial prompt is standard text" $ do
        let (_, actions) = initialTuiLaunch (Just "investigate memory usage") baseState
        actions `shouldBe` [ActionRunAgent "investigate memory usage"]

      it "returns empty actions for Nothing or whitespace prompt" $ do
        let (_, a1) = initialTuiLaunch Nothing baseState
        a1 `shouldBe` []
        let (_, a2) = initialTuiLaunch (Just "   ") baseState
        a2 `shouldBe` []

    describe "buildTuiSystemPrompt" $ do
      let tuiSandbox = "dist-newstyle/test-sandbox-tui-prompt"
      around_ (\action -> do
        createDirectoryIfMissing True tuiSandbox
        action
        removeDirectoryRecursive tuiSandbox) $ do
        it "builds base prompt when no guidelines or extra prompt" $ do
          prompt <- buildTuiSystemPrompt tuiSandbox Nothing
          prompt `shouldSatisfy` ("expert autonomous coding assistant" `T.isInfixOf`)

        it "appends custom instructions when optAppendSystemPrompt is passed" $ do
          prompt <- buildTuiSystemPrompt tuiSandbox (Just "Always reply in uppercase")
          prompt `shouldSatisfy` ("expert autonomous coding assistant" `T.isInfixOf`)
          prompt `shouldSatisfy` ("Always reply in uppercase" `T.isInfixOf`)

        it "combines AGENT.md guidelines and optAppendSystemPrompt" $ do
          TIO.writeFile (tuiSandbox </> "AGENT.md") "Follow strict types."
          prompt <- buildTuiSystemPrompt tuiSandbox (Just "Reply in JSON only")
          prompt `shouldSatisfy` ("Follow strict types." `T.isInfixOf`)
          prompt `shouldSatisfy` ("Reply in JSON only" `T.isInfixOf`)

