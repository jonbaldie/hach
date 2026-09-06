{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Agent.PropertySpec (spec) where

import Agent.Env
import Agent.OpenRouter
import Agent.Tools
import Agent.TUI.State
import Agent.TUI.Types
import Agent.Types
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck

-- Arbitrary instances for Property-Based Testing
instance Arbitrary UserKey where
  arbitrary = oneof
    [ KeyChar <$> arbitraryPrintableChar
    , pure KeyEnter
    , pure KeyBackspace
    , pure KeyDelete
    , pure KeyTab
    , pure KeyBackTab
    , pure KeyEsc
    , pure KeyUp
    , pure KeyDown
    , pure KeyPageUp
    , pure KeyPageDown
    , pure KeyScrollUp
    , pure KeyScrollDown
    , pure KeyF1
    , KeyCtrl <$> elements ['a'..'z']
    ]

instance Arbitrary TuiEvent where
  arbitrary = oneof
    [ EvUserKey <$> arbitrary
    , EvSubmit . T.pack <$> arbitrary
    , EvHarness . EvTurnStart <$> choose (1, 20)
    , EvHarness . EvDone . T.pack <$> arbitrary
    , EvHarness . EvError . T.pack <$> arbitrary
    ]

spec :: Spec
spec = do
  describe "Property-Based Testing & Vulnerability Search" $ do

    describe "Security & Workspace Isolation (Path Traversal)" $ do
      it "prevents reading files outside the workspace via ../ traversal" $ do
        cwd <- Dir.getCurrentDirectory
        let sandboxDir = cwd </> ".test-sandbox-sec"
            wsDir = sandboxDir </> "workspace"
            secretFile = sandboxDir </> "secret.txt"
        Dir.createDirectoryIfMissing True wsDir
        writeFile secretFile "SUPER_SECRET_DATA"

        -- Attempt path traversal
        res <- executeReadFile wsDir (ReadFileArgs "../secret.txt")
        Dir.removeDirectoryRecursive sandboxDir

        case res of
          ToolError _ -> pure ()
          ToolSuccess content ->
            expectationFailure ("SECURITY BUG: Path traversal succeeded! Leaked: " <> T.unpack content)

      it "prevents reading files outside workspace via absolute paths" $ do
        cwd <- Dir.getCurrentDirectory
        let sandboxDir = cwd </> ".test-sandbox-sec-abs"
            wsDir = sandboxDir </> "workspace"
            secretFile = sandboxDir </> "secret.txt"
        Dir.createDirectoryIfMissing True wsDir
        writeFile secretFile "SUPER_SECRET_DATA"

        -- Attempt absolute path reading
        res <- executeReadFile wsDir (ReadFileArgs secretFile)
        Dir.removeDirectoryRecursive sandboxDir

        case res of
          ToolError _ -> pure ()
          ToolSuccess content ->
            expectationFailure ("SECURITY BUG: Absolute path outside workspace allowed! Leaked: " <> T.unpack content)

      it "prevents writing files outside the workspace via ../ traversal" $ do
        cwd <- Dir.getCurrentDirectory
        let sandboxDir = cwd </> ".test-sandbox-sec-write"
            wsDir = sandboxDir </> "workspace"
        Dir.createDirectoryIfMissing True wsDir

        -- Attempt path traversal write
        res <- executeWriteFile wsDir (WriteFileArgs "../pwned.txt" "pwned")
        Dir.removeDirectoryRecursive sandboxDir

        case res of
          ToolError _ -> pure ()
          ToolSuccess _ ->
            expectationFailure "SECURITY BUG: Write path traversal succeeded!"

    describe "CLI Argument Parser Invariants (QuickCheck)" $ do
      it "never crashes on arbitrary list of arguments" $ property $ \args ->
        case parseCliArgs args of
          Left _  -> True
          Right _ -> True

      it "always sets optNoTui = True when --no-tui is present" $ property $ \wordsBefore wordsAfter ->
        let args = wordsBefore ++ ["--no-tui"] ++ wordsAfter
        in case parseCliArgs args of
             Left _     -> True
             Right opts -> optNoTui opts == True

    describe "OpenRouter Error Handling" $ do
      it "handles string error payloads without falling back to JSON parse failure" $ do
        let raw = "{\"error\": \"Unauthorized access\"}"
        case parseChatResponse (LBS.fromStrict raw) of
          Left err ->
            err `shouldBe` "OpenRouter API error: Unauthorized access"
          Right _ ->
            expectationFailure "Expected failure for error payload"

    describe "TUI State Machine Invariants under Fuzzing" $ do
      it "preserves state invariants across arbitrary sequences of user keys" $ property $ \(events :: [TuiEvent]) ->
        let initialState = initialTuiState "meta/muse-glimmer-30b" (Just 10)
            finalState = foldl (\s ev -> fst (updateTui ev s)) initialState events
        in tsHistoryScroll finalState >= 0
           && tsSelectedToolIndex finalState >= 0
           && (null (tsTools finalState) || tsSelectedToolIndex finalState < length (tsTools finalState))

      it "quits on 'q' when History or Tools panel is focused (User Story 19)" $ do
        let sHistory = (initialTuiState "m" (Just 10)) { tsFocus = FocusHistory }
            (s1, actions1) = updateTui (EvUserKey (KeyChar 'q')) sHistory
        tsShouldQuit s1 `shouldBe` True
        actions1 `shouldBe` [ActionQuit]

        let sTools = (initialTuiState "m" (Just 10)) { tsFocus = FocusTools }
            (s2, actions2) = updateTui (EvUserKey (KeyChar 'q')) sTools
        tsShouldQuit s2 `shouldBe` True
        actions2 `shouldBe` [ActionQuit]

      it "toggles help overlay on KeyF1 from any focus mode" $ do
        let s0 = initialTuiState "m" (Just 10)
        tsShowHelp s0 `shouldBe` False
        let (s1, _) = updateTui (EvUserKey KeyF1) s0
        tsShowHelp s1 `shouldBe` True
        let (s2, _) = updateTui (EvUserKey KeyF1) s1
        tsShowHelp s2 `shouldBe` False
