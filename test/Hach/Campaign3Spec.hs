{-# LANGUAGE OverloadedStrings #-}

-- | Third-pass coverage-guided property campaign.
--
-- Waves 1–2 covered globs, permission aliases, skill tokens, worktree
-- names, role alternation, containment, MCP names, and task ids.  This
-- wave hunts the remaining TUI / cost / CLI / codec seams:
--
-- * TUI cost estimate vs 'estimateCostUsd' identity
-- * slash-command completion for built-ins and skills
-- * auto-scroll coverage of every transcript-appending event
-- * prompt-history browse abandoned on edit
-- * CLI --print / --exec / --permission-mode
-- * JSON codecs still untested in earlier waves
-- * hook exit-2 protocol, env-file round-trips, porcelain partition
module Hach.Campaign3Spec (spec) where

import Hach.Env
import Hach.Git
import Hach.Hooks
import Hach.OpenRouter
import Hach.Permissions
import Hach.Sessions
import Hach.Settings
import Hach.Skills
import Hach.TUI.State
import Hach.TUI.Types
import Hach.Types

import Data.List (nub)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- * Campaign knobs
-- ---------------------------------------------------------------------------

vigorous :: Testable prop => prop -> Property
vigorous = withNumTests 2000

-- ---------------------------------------------------------------------------
-- * Generators
-- ---------------------------------------------------------------------------

genSeg :: Gen String
genSeg = listOf1 (elements ['a'..'z'])

genTxt :: Gen Text
genTxt = T.pack <$> listOf (elements ['a'..'z'])

genNonEmptyTxt :: Gen Text
genNonEmptyTxt = T.pack <$> listOf1 (elements ['a'..'z'])

genModel :: Gen Text
genModel = T.pack <$> elements
  [ "anthropic/claude-3-opus"
  , "anthropic/claude-3.5-sonnet"
  , "anthropic/claude-3-haiku"
  , "openai/gpt-4o-mini"
  , "openai/gpt-4o"
  , "deepseek/deepseek-chat"
  , "meta/muse-glimmer-30b"
  , "claude-unknown-tier"
  ]

genBuiltinPrefix :: Gen (Text, Text)
genBuiltinPrefix = do
  cmd <- elements builtinCommands
  n   <- choose (2, max 2 (T.length cmd))  -- at least "/x"
  let typed = T.take n cmd
  pure (typed, cmd)

genSkillName :: Gen Text
genSkillName = T.pack <$> listOf1 (elements ['a'..'z'])

genCatalog :: Gen SkillCatalog
genCatalog = do
  n      <- choose (0, 5)
  skills <- vectorOf n $ do
    name <- genSkillName
    inv  <- arbitrary
    pure (mkSkill name "d" "body" "/p" SkillGlobal) { skillUserInvocable = inv }
  pure (Map.fromList [(skillName s, s) | s <- skills])

-- ---------------------------------------------------------------------------
-- * Coverage labels
-- ---------------------------------------------------------------------------

costFamily :: Text -> String
costFamily m
  | "opus"     `T.isInfixOf` lower = "opus"
  | "sonnet"   `T.isInfixOf` lower = "sonnet"
  | "haiku"    `T.isInfixOf` lower = "haiku"
  | "gpt-4o-mini" `T.isInfixOf` lower = "gpt-4o-mini"
  | "gpt-4o"   `T.isInfixOf` lower = "gpt-4o"
  | "claude"   `T.isInfixOf` lower = "claude-fallback"
  | "deepseek" `T.isInfixOf` lower = "deepseek"
  | otherwise                      = "default"
  where
    lower = T.toLower m

appendingProfile :: AgentEvent -> String
appendingProfile = \case
  EvLLMResponse{}      -> "llm"
  EvDone{}             -> "done"
  EvError{}            -> "error"
  EvToolCall{}         -> "tool-call"
  EvToolResult{}       -> "tool-result"
  EvPermissionDenied{} -> "perm"
  EvHookTriggered{}    -> "hook"
  EvSessionSaved{}     -> "session"
  EvNotificationSent{} -> "notice"
  EvGoalEvaluated{}    -> "goal-eval"
  EvGoalAchieved{}     -> "goal-ok"
  EvGoalFailed{}       -> "goal-fail"
  EvGoalBlocked{}      -> "goal-block"
  _                    -> "other"

-- ---------------------------------------------------------------------------
-- * Properties
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "CGPT wave-3: cost estimator identity" $ do
    it "/cost estimated line uses estimateCostUsd (not a second rate table)" $ vigorous $
      forAll genModel $ \model ->
      forAll (choose (1, 200000) :: Gen Int) $ \p ->
      forAll (choose (1, 200000) :: Gen Int) $ \c ->
        let stu = initialSessionTokenUsage
              { stuPromptTokens     = p
              , stuCompletionTokens = c
              , stuTotalTokens      = p + c
              }
            st0 = (initialTuiState model Nothing) { tsSessionTokens = stu }
            (st1, _) = updateTui (EvSubmit "/cost") st0
            expected = estimateCostUsd model p c
            needle = T.pack (printf "%.4f" expected)
            notices = [ n | TiNotice n <- tsTranscript st1 ]
        in collect (costFamily model) $
             any (needle `T.isInfixOf`) notices

    it "reproducer: claude-3-opus is billed at opus rates, not generic claude" $
      estimateCostUsd "anthropic/claude-3-opus" 1000000 1000000 `shouldBe` 90.0

    it "reproducer: gpt-4o-mini is not billed as generic gpt-4" $
      estimateCostUsd "openai/gpt-4o-mini" 1000000 1000000 `shouldBe` 0.75

    it "reproducer: generic gpt-4 uses the folded-back $2.50/$10.00 table" $ do
      estimateCostUsd "openai/gpt-4" 1000000 1000000 `shouldBe` 12.5
      estimateCostUsd "openai/gpt-4o" 1000000 1000000 `shouldBe` 20.0

  describe "CGPT wave-3: slash-command completion" $ do
    it "a proper prefix of a built-in completes to a built-in" $ vigorous $
      forAll genBuiltinPrefix $ \(typed, _cmd) ->
        let suffix = inputSlashCompletion Map.empty builtinCommands typed
        in collect (T.unpack typed) $
             case suffix of
               Nothing -> typed `elem` builtinCommands
               Just s  -> (typed <> s) `elem` builtinCommands

    it "completed text is always a known candidate (builtin or invocable skill)" $ vigorous $
      forAll genCatalog $ \cat ->
      forAll (elements ("/x" : builtinCommands)) $ \typed ->
        let n = max 2 (min (T.length typed) 4)
            prefix = T.take n typed
            candidates = builtinCommands
              ++ [ "/" <> skillName s | s <- Map.elems cat, skillUserInvocable s ]
        in case inputSlashCompletion cat builtinCommands prefix of
             Nothing -> True
             Just s  -> (prefix <> s) `elem` candidates

    it "non-invocable skills never contribute a completion" $ vigorous $
      forAll genSkillName $ \name ->
        T.length name > 1 ==>
          let s = (mkSkill name "" "" "" SkillGlobal) { skillUserInvocable = False }
              cat = Map.singleton name s
              typed = "/" <> T.take 1 name
          in inputSlashCompletion cat [] typed === Nothing

    it "reproducer: /cle completes to /clear with an empty skill catalog" $
      inputSlashCompletion Map.empty builtinCommands "/cle" `shouldBe` Just "ar"

    it "reproducer: /co completes to the lex-least builtin (/compact)" $
      inputSlashCompletion Map.empty builtinCommands "/co"
        `shouldBe` Just "mpact"

  describe "CGPT wave-3: auto-scroll covers every appending constructor" $ do
    it "notice-appending harness events auto-scroll when input is focused" $ vigorous $
      forAll genAppendingEvent $ \ev ->
        let st = (initialTuiState "m" (Just 10)) { tsFocus = FocusInput }
        in collect (appendingProfile ev) $
             isTranscriptAppendingEvent ev
             && shouldAutoScroll st ev

    it "the same events never auto-scroll when the transcript is focused" $ vigorous $
      forAll genAppendingEvent $ \ev ->
        let st = (initialTuiState "m" (Just 10)) { tsFocus = FocusTranscript }
        in not (shouldAutoScroll st ev)

    it "reproducer: EvHookTriggered / EvSessionSaved / EvNotificationSent / EvToolResult append" $ do
      isTranscriptAppendingEvent (EvHookTriggered "PreToolUse" "ok") `shouldBe` True
      isTranscriptAppendingEvent (EvSessionSaved "s.jsonl") `shouldBe` True
      isTranscriptAppendingEvent (EvNotificationSent "hi") `shouldBe` True
      isTranscriptAppendingEvent (EvToolResult "bash" (ToolSuccess "ok")) `shouldBe` True

  describe "CGPT wave-3: prompt-history browse is abandoned on edit" $ do
    it "typing after Up clears the history index and keeps the edited buffer" $ vigorous $
      forAll (listOf1 genNonEmptyTxt) $ \hist ->
      forAll (elements ['a'..'z']) $ \c ->
        let st0 = (initialTuiState "m" Nothing)
                    { tsFocus = FocusInput, tsPromptHistory = hist }
            (st1, _) = updateTui (EvUserKey KeyUp) st0
            (st2, _) = updateTui (EvUserKey (KeyChar c)) st1
        in collect (length hist) $
             tsPromptHistoryIndex st1 == Just (length hist - 1)
             && tsPromptHistoryIndex st2 == Nothing
             && tsInputBuffer st2 == tsInputBuffer st1 `T.snoc` c

    it "backspace after Up also leaves browse mode" $ vigorous $
      forAll (listOf1 genNonEmptyTxt) $ \hist ->
        let st0 = (initialTuiState "m" Nothing)
                    { tsFocus = FocusInput, tsPromptHistory = hist }
            (st1, _) = updateTui (EvUserKey KeyUp) st0
            (st2, _) = updateTui (EvUserKey KeyBackspace) st1
        in tsPromptHistoryIndex st2 === Nothing

    it "reproducer: Up then 'x' then Down does not jump to another history entry" $ do
      let st0 = (initialTuiState "m" Nothing)
                  { tsFocus = FocusInput
                  , tsPromptHistory = ["alpha", "beta"]
                  , tsInputBuffer = "draft"
                  }
          (st1, _) = updateTui (EvUserKey KeyUp) st0   -- beta
          (st2, _) = updateTui (EvUserKey (KeyChar 'x')) st1
          (st3, _) = updateTui (EvUserKey KeyDown) st2
      tsInputBuffer st2 `shouldBe` "betax"
      tsPromptHistoryIndex st2 `shouldBe` Nothing
      -- Down with no active index is a no-op
      tsInputBuffer st3 `shouldBe` "betax"

  describe "CGPT wave-3: CLI print/exec/permission-mode" $ do
    it "--print always implies --no-tui and optPrint" $ vigorous $
      forAll (listOf (elements ["foo", "bar", "x"])) $ \wordsBefore ->
      forAll (listOf (elements ["foo", "bar", "y"])) $ \wordsAfter ->
        case parseCliArgs (wordsBefore ++ ["--print"] ++ wordsAfter) of
          Right opts -> optPrint opts && optNoTui opts
          Left _     -> True

    it "--exec CMD implies --no-tui and records the command" $ vigorous $
      forAll genSeg $ \cmd ->
        case parseCliArgs ["--exec", cmd] of
          Right opts ->
            property (optNoTui opts && optExec opts == Just (T.pack cmd))
          Left err -> counterexample err False

    it "--permission-mode accepts every documented alias" $ vigorous $
      forAll (elements
        [ ("default", ModeDefault)
        , ("acceptEdits", ModeAcceptEdits)
        , ("accept-edits", ModeAcceptEdits)
        , ("plan", ModePlan)
        , ("auto", ModeAuto)
        , ("dontAsk", ModeDontAsk)
        , ("dont-ask", ModeDontAsk)
        , ("bypassPermissions", ModeBypassPermissions)
        , ("bypass-permissions", ModeBypassPermissions)
        ]) $ \(alias, mode) ->
          case parseCliArgs ["--permission-mode", alias] of
            Right opts -> optPermissionMode opts === Just mode
            Left err   -> counterexample err False

  describe "CGPT wave-3: remaining JSON codecs" $ do
    it "SessionInfo round-trips" $ vigorous $
      forAll genNonEmptyTxt $ \sid ->
      forAll genNonEmptyTxt $ \created ->
      forAll genNonEmptyTxt $ \model ->
      forAll (choose (0, 50) :: Gen Int) $ \turns ->
      forAll (elements [0, 0.25, 1, 2.5, 10] :: Gen Double) $ \cost ->
        let info = SessionInfo sid created model turns cost
        in Aeson.decode (Aeson.encode info) === Just info

    it "Settings with empty collections round-trip" $
      Aeson.decode (Aeson.encode defaultSettings) `shouldBe` Just defaultSettings

    it "GitStatusInfo round-trips" $ vigorous $
      forAll genNonEmptyTxt $ \branch ->
      forAll arbitrary $ \clean ->
        let info = GitStatusInfo branch clean ["a.hs"] ["b.hs"]
        in Aeson.decode (Aeson.encode info) === Just info

    it "HookResult round-trips" $ vigorous $
      forAll (oneof [pure Nothing, Just . PermAsk <$> genTxt]) $ \dec ->
        let r = defaultHookResult { hrDecision = dec, hrAdditionalContext = Just "x" }
        in Aeson.decode (Aeson.encode r) === Just r

  describe "CGPT wave-3: hooks, env files, porcelain" $ do
    it "parseHookOutput 2 with a JSON deny is a PermDeny" $ vigorous $
      forAll genNonEmptyTxt $ \reason ->
        let payload = Aeson.encode $ object
              [ "permissionDecision" .= object
                  [ "decision" .= ("deny" :: Text)
                  , "reason"   .= reason
                  ]
              ]
            res = parseHookOutput 2 (TE.decodeUtf8 (LBS.toStrict payload))
        in hrDecision res === Just (PermDeny reason)

    it "parseHookOutput 2 with non-JSON text is a deny of that text" $ vigorous $
      forAll genNonEmptyTxt $ \raw ->
        not ("{" `T.isPrefixOf` raw) ==>
          hrDecision (parseHookOutput 2 raw) === Just (PermDeny raw)

    it "parseEnvContent then lookup recovers every simple KEY=value pair" $ vigorous $
      forAll (listOf1 $ (,) <$> genNonEmptyTxt <*> genTxt) $ \pairs ->
        let lastWins = Map.toList (Map.fromList pairs)
            body = T.unlines [ k <> "=" <> v | (k, v) <- lastWins ]
            parsed = parseEnvContent body
        in collect (length lastWins) $
             all (\(k, v) -> Map.lookup k parsed == Just (T.strip v)) lastWins

    it "porcelain entries are partitioned into modified XOR untracked" $ vigorous $
      forAll (listOf genSeg) $ \modFiles ->
      forAll (listOf genSeg) $ \untFiles ->
        let mods = nub modFiles
            unts = filter (`notElem` mods) (nub untFiles)
            raw = T.unlines $
              ["## main"]
              ++ [ " M " <> T.pack f | f <- mods ]
              ++ [ "?? " <> T.pack f | f <- unts ]
            st = parsePorcelainStatus "main" raw
        in null [ f | f <- gsiModified st, f `elem` gsiUntracked st ]
           && gsiModified st == mods
           && gsiUntracked st == unts

    it "extractPathArg reads path/file/target/filePath and nothing else" $ vigorous $
      forAll genSeg $ \p ->
      forAll (elements ["path", "file", "target", "filePath"]) $ \k ->
        extractPathArg (object [Key.fromText (T.pack k) .= T.pack p]) === Just p

  describe "CGPT wave-3: OpenRouter error envelopes never succeed" $ do
    it "a top-level string error is reported, not parsed as a completion" $ vigorous $
      forAll genNonEmptyTxt $ \msg ->
        let body = Aeson.encode $ object ["error" .= msg]
        in case parseChatResponse body of
             Left err -> "OpenRouter API error:" `T.isPrefixOf` err
             Right _  -> False

    it "empty choices is a Left" $
      case parseChatResponse "{\"choices\":[]}" of
        Left _  -> pure ()
        Right r -> expectationFailure ("empty choices succeeded: " <> show r)

  describe "CGPT wave-3: skill / settings / compaction identities" $ do
    it "mergeSkills: workspace name wins" $ vigorous $
      forAll genSkillName $ \name ->
        let g = mkSkill name "g" "global" "/g" SkillGlobal
            w = mkSkill name "w" "work" "/w" SkillWorkspace
        in Map.lookup name (mergeSkills [g] [w]) === Just w

    it "injectSkillsIntoPrompt is identity on an empty skill list" $ vigorous $
      forAll genTxt $ \prompt ->
        injectSkillsIntoPrompt [] prompt === prompt

    it "formatHistoryForCompaction contains every user turn" $ vigorous $
      forAll (listOf genNonEmptyTxt) $ \users ->
        let hist = map UserMsg users
            formatted = formatHistoryForCompaction hist
        in all (`T.isInfixOf` formatted) users

    it "makeCompactedHistory is SystemMsg? ++ one UserMsg summary" $ vigorous $
      forAll (oneof [pure Nothing, Just <$> genNonEmptyTxt]) $ \mSys ->
      forAll genNonEmptyTxt $ \summary ->
        let hist = makeCompactedHistory mSys summary
        in case (mSys, hist) of
             (Nothing, [UserMsg u]) -> summary `T.isInfixOf` u
             (Just s, [SystemMsg s', UserMsg u]) ->
               s' == s && summary `T.isInfixOf` u
             _ -> False

    it "parseSkillFile requires a name and preserves user-invocable=false" $
      let raw = "---\nname: secret\nuser-invocable: false\n---\nbody\n"
      in case parseSkillFile SkillGlobal "/p" raw of
           Right s -> do
             skillName s `shouldBe` "secret"
             skillUserInvocable s `shouldBe` False
           Left err -> expectationFailure err

genAppendingEvent :: Gen AgentEvent
genAppendingEvent = elements
  [ EvLLMResponse (Just "x") [] Nothing
  , EvDone "done"
  , EvError "err"
  , EvToolCall "bash" "ls"
  , EvToolResult "bash" (ToolSuccess "ok")
  , EvPermissionDenied "write_file" "no"
  , EvHookTriggered "PreToolUse" "ok"
  , EvSessionSaved "s.jsonl"
  , EvNotificationSent "hi"
  , EvGoalEvaluated GoalMet "ok"
  , EvGoalAchieved "c"
  , EvGoalFailed "c" "no"
  , EvGoalBlocked "c"
  ]
