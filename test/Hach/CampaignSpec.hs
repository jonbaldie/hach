{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Vigorous coverage-guided property-based testing (CGPT) campaign.
--
-- Targets the security- and invariant-sensitive seams that are not already
-- covered by "Hach.GoalFuzzSpec": glob matching, permission classification,
-- skill invocation parsing, worktree name validation, transcript-to-message
-- role alternation, CLI flag wiring, JSON codecs, and token accounting.
--
-- Coverage is signalled with QuickCheck 'collect' labels so shrinking keeps
-- cases that open new constructor / path combinations.
module Hach.CampaignSpec (spec) where

import Hach.Env
import Hach.Git
import Hach.Memory
import Hach.Notifications
import Hach.Permissions
import Hach.Settings
import Hach.Skills
import Hach.Tasks
import Hach.Tools
import Hach.TUI.App (dialogueToMessages)
import Hach.TUI.Types
import Hach.Types

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- * Campaign knobs
-- ---------------------------------------------------------------------------

vigorous :: Testable prop => prop -> Property
vigorous = withNumTests 2000

-- ---------------------------------------------------------------------------
-- * Shared generators
-- ---------------------------------------------------------------------------

genSeg :: Gen String
genSeg = listOf1 (elements ['a'..'z'])

genFileName :: Gen String
genFileName = do
  base <- genSeg
  ext  <- elements [".hs", ".txt", ".md", ""]
  pure (base <> ext)

genRelPath :: Gen FilePath
genRelPath = do
  n     <- choose (1, 4)
  segs  <- vectorOf n genFileName
  pure (foldr1 (\a b -> a <> "/" <> b) segs)

instance Arbitrary Text where
  arbitrary = T.pack <$> listOf (elements ['a'..'z'])

instance Arbitrary PermissionMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RuleAction where
  arbitrary = elements [RuleAllow, RuleAsk, RuleDeny]

instance Arbitrary PermissionRule where
  arbitrary = PermissionRule
    <$> arbitrary
    <*> (oneof [pure Nothing, Just . T.pack <$> genSeg])
    <*> (oneof [pure Nothing, Just . T.pack <$> genRelPath])

instance Arbitrary ToolCall where
  arbitrary = ToolCall
    <$> (T.pack <$> listOf1 (elements ['a'..'z']))
    <*> (T.pack <$> listOf1 (elements ['a'..'z']))
    <*> (T.pack <$> listOf (elements ['{','}','"','a','1',':']))

instance Arbitrary Message where
  arbitrary = oneof
    [ SystemMsg    <$> genTxt
    , UserMsg      <$> genTxt
    , AssistantMsg <$> (oneof [pure Nothing, Just <$> genTxt]) <*> arbitrary
    , ToolMsg      <$> genTxt <*> genTxt <*> genTxt
    ]
    where
      genTxt = T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary ToolResult where
  arbitrary = oneof
    [ ToolSuccess <$> (T.pack <$> listOf (elements ['a'..'z']))
    , ToolError   <$> (T.pack <$> listOf1 (elements ['a'..'z']))
    ]

instance Arbitrary GoalVerdict where
  arbitrary = elements [GoalMet, GoalNotYetMet, GoalImpossible]

instance Arbitrary PermissionDecision where
  arbitrary = oneof
    [ pure PermAllow
    , PermAsk  <$> (T.pack <$> listOf (elements ['a'..'z']))
    , PermDeny <$> (T.pack <$> listOf (elements ['a'..'z']))
    ]

instance Arbitrary HookEvent where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OutputStyle where
  arbitrary = oneof
    [ pure StyleDefault
    , pure StyleConcise
    , pure StyleExplanatory
    , pure StyleCodeOnly
    , StyleCustom <$> (T.pack <$> listOf1 (elements ['a'..'z']))
        `suchThat` (`notElem` ["default", "concise", "explanatory", "code_only"])
    ]

instance Arbitrary ToolLifecycle where
  arbitrary = oneof
    [ pure Pending
    , pure Running
    , Finished <$> arbitrary
    , Denied <$> (T.pack <$> listOf1 (elements ['a'..'z']))
    , pure Cancelled
    ]

instance Arbitrary ToolCard where
  arbitrary = ToolCard
    <$> (T.pack <$> listOf1 (elements ['a'..'z']))
    <*> (T.pack <$> listOf1 (elements ['a'..'z']))
    <*> (T.pack <$> listOf (elements ['a'..'z']))
    <*> arbitrary
    <*> arbitrary

instance Arbitrary TranscriptItem where
  arbitrary = frequency
    [ (3, TiUser      <$> genTxt)
    , (3, TiAssistant <$> genTxt)
    , (1, TiSystem    <$> genTxt)
    , (1, TiNotice    <$> genTxt)
    , (3, TiToolCard  <$> arbitrary)
    ]
    where
      genTxt = T.pack <$> listOf (elements ['a'..'z'])

-- ---------------------------------------------------------------------------
-- * Coverage labels
-- ---------------------------------------------------------------------------

globShape :: Text -> FilePath -> String
globShape pat fp
  | "**/" `T.isInfixOf` pat && '/' `notElem` fp = "**/ vs flat-name"
  | "**/" `T.isInfixOf` pat && '/' `elem` fp    = "**/ vs nested"
  | "*"   `T.isInfixOf` pat && '/' `elem` fp    = "*-glob vs nested"
  | otherwise                                  = "literal/other"

-- ---------------------------------------------------------------------------
-- * Intended permission classification
-- ---------------------------------------------------------------------------

readOnlyAliases :: [Text]
readOnlyAliases =
  [ "read_file", "list_dir", "ListDir", "listdir"
  , "find_files", "Glob", "glob"
  , "grep_search", "Grep", "grep"
  , "WebFetch", "web_fetch", "webfetch"
  , "WebSearch", "web_search", "websearch"
  , "ListAgents", "list_agents", "listagents"
  , "TaskList", "task_list", "tasklist"
  , "TaskGet", "task_get", "taskget"
  ]

writeAliases :: [Text]
writeAliases =
  [ "write_file", "replace_file_content"
  , "Edit", "edit"
  , "TodoWrite", "todo_write", "todowrite"
  , "TaskCreate", "task_create", "taskcreate"
  , "TaskUpdate", "task_update", "taskupdate"
  ]

commandAliases :: [Text]
commandAliases =
  [ "run_command", "Bash", "bash"
  , "TaskStop", "task_stop", "taskstop"
  , "EnterWorktree", "enter_worktree", "enterworktree"
  , "ExitWorktree", "exit_worktree", "exitworktree"
  ]

isAllow :: PermissionDecision -> Bool
isAllow PermAllow = True
isAllow _         = False

isAsk :: PermissionDecision -> Bool
isAsk (PermAsk _) = True
isAsk _           = False

isDeny :: PermissionDecision -> Bool
isDeny (PermDeny _) = True
isDeny _            = False

emptyArgs :: Value
emptyArgs = object []

-- ---------------------------------------------------------------------------
-- * Skill catalog generator
-- ---------------------------------------------------------------------------

genSkillName :: Gen Text
genSkillName = T.pack <$> listOf1 (elements ['a'..'z'])

genSkill :: Gen Skill
genSkill = do
  name <- genSkillName
  inv  <- arbitrary
  pure (mkSkill name "d" "body" "/p" SkillGlobal) { skillUserInvocable = inv }

genCatalog :: Gen SkillCatalog
genCatalog = do
  n      <- choose (1, 5)
  skills <- vectorOf n genSkill
  pure (Map.fromList [(skillName s, s) | s <- skills])

-- Word tokens: either a skill slash-token or an unrelated word that may
-- share a prefix with a skill (the corruption case).
genPromptWords :: SkillCatalog -> Gen [Text]
genPromptWords cat = listOf1 $ oneof
  [ elements (Map.keys cat) >>= \n ->
      oneof [ pure ("/" <> n)
            , pure ("/" <> n <> "-bar")
            , pure ("/" <> n <> "/path")
            , T.pack <$> listOf1 (elements ['a'..'z'])
            ]
  , T.pack <$> listOf1 (elements ['a'..'z'])
  ]

-- ---------------------------------------------------------------------------
-- * Worktree name generators
-- ---------------------------------------------------------------------------

genValidWorktreeName :: Gen Text
genValidWorktreeName = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
  rest  <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_."))
  let raw = T.pack (first : rest)
  -- Reject internally-generated names that contain ".." or start with '-'.
  if "-" `T.isPrefixOf` raw || ".." `T.isInfixOf` raw
    then genValidWorktreeName
    else pure raw

genFlagLikeName :: Gen Text
genFlagLikeName = T.pack <$> elements
  ["--force", "-b", "--orphan", "-f", "--detach", "-B"]

-- ---------------------------------------------------------------------------
-- * Properties
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "CGPT campaign: matchGlob directory-boundary **/ " $ do
    it "**/ never matches a suffix inside a single path segment" $ vigorous $
      forAll genFileName $ \suffix ->
      forAll (listOf1 (elements ['a'..'z'])) $ \prefix ->
        let pat    = T.pack ("**/" <> suffix)
            target = prefix <> suffix
        in collect (globShape pat target) $
             not (matchGlob pat target)

    it "**/suffix matches the bare suffix (zero directories)" $ vigorous $
      forAll genFileName $ \suffix ->
        matchGlob (T.pack ("**/" <> suffix)) suffix

    it "**/suffix matches after a complete directory segment" $ vigorous $
      forAll genFileName $ \suffix ->
      forAll genSeg $ \dir ->
        matchGlob (T.pack ("**/" <> suffix)) (dir <> "/" <> suffix)

    it "single-star * never crosses a directory separator" $ vigorous $
      forAll genSeg $ \dir ->
      forAll genFileName $ \name ->
        not (matchGlob (T.pack (dir <> "/*")) (dir <> "/sub/" <> name))

    it "literal patterns match only themselves" $ vigorous $
      forAll genRelPath $ \p ->
        matchGlob (T.pack p) p

    -- Headline reproducers from the **/ substring bug.
    it "reproducers: **/c vs abc, **/bar.txt vs foobar.txt, src/**/secret.txt vs src/nonsecret.txt" $ do
      matchGlob "**/c" "abc" `shouldBe` False
      matchGlob "**/bar.txt" "foobar.txt" `shouldBe` False
      matchGlob "src/**/secret.txt" "src/nonsecret.txt" `shouldBe` False
      matchGlob "src/**/*.hs" "src/foo.hs" `shouldBe` True
      matchGlob "src/**/*.hs" "src/a/b/foo.hs" `shouldBe` True

  describe "CGPT campaign: permission tool-alias classification" $ do
    it "Plan mode allows every read-only alias (incl. snake_case)" $ vigorous $
      forAll (elements readOnlyAliases) $ \tool ->
        collect (T.unpack tool) $
          isAllow (evalPermission ModePlan [] tool emptyArgs)

    it "Default mode allows every read-only alias" $ vigorous $
      forAll (elements readOnlyAliases) $ \tool ->
        isAllow (evalPermission ModeDefault [] tool emptyArgs)

    it "AcceptEdits auto-approves every write alias" $ vigorous $
      forAll (elements writeAliases) $ \tool ->
        let args = object ["path" .= ("src/Foo.hs" :: String)]
        in collect (T.unpack tool) $
             isAllow (evalPermission ModeAcceptEdits [] tool args)

    it "AcceptEdits asks for every command alias" $ vigorous $
      forAll (elements commandAliases) $ \tool ->
        collect (T.unpack tool) $
          isAsk (evalPermission ModeAcceptEdits [] tool emptyArgs)

    it "BypassPermissions allows any tool name" $ vigorous $
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \tool ->
        evalPermission ModeBypassPermissions [] tool emptyArgs === PermAllow

    it "DontAsk allows any tool name" $ vigorous $
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \tool ->
        evalPermission ModeDontAsk [] tool emptyArgs === PermAllow

    it "Plan mode denies write and command aliases" $ vigorous $
      forAll (elements (writeAliases ++ commandAliases)) $ \tool ->
        let args = object ["path" .= ("src/Foo.hs" :: String)]
        in isDeny (evalPermission ModePlan [] tool args)

    it "protected-path writes are denied in AcceptEdits" $ vigorous $
      forAll (elements ([".git/config", ".claude/settings.json", ".agents/x"] :: [String])) $ \p ->
      forAll (elements ["write_file", "edit", "todo_write"]) $ \tool ->
        let args = object ["path" .= p]
        in isDeny (evalPermission ModeAcceptEdits [] tool args)

    it "cyclePermissionMode is a 6-cycle covering every mode" $
      let cycle6 = take 6 (iterate cyclePermissionMode ModeDefault)
      in nub cycle6 `shouldMatchList` [minBound .. maxBound :: PermissionMode]

    -- Headline reproducers from the snake_case alias bug.
    it "reproducers: web_fetch / task_create snake_case aliases" $ do
      evalPermission ModeDefault [] "web_fetch" emptyArgs `shouldBe` PermAllow
      isDeny (evalPermission ModePlan [] "web_fetch" emptyArgs) `shouldBe` False
      isAllow (evalPermission ModeAcceptEdits [] "task_create" emptyArgs) `shouldBe` True

  describe "CGPT campaign: parseSkillInvocations token integrity" $ do
    it "cleaned words equal original words minus exact invoked slash-tokens" $ vigorous $
      forAll genCatalog $ \cat ->
      forAll (genPromptWords cat) $ \ws ->
        let input   = T.unwords ws
            (cleaned, skills) = parseSkillInvocations cat input
            invoked = ["/" <> skillName s | s <- skills]
            expected = filter (`notElem` invoked) (T.words input)
        in collect (length skills) $
             T.words cleaned === expected

    it "never activates a skill with skillUserInvocable = False" $ vigorous $
      forAll genCatalog $ \cat ->
      forAll (genPromptWords cat) $ \ws ->
        let (_, skills) = parseSkillInvocations cat (T.unwords ws)
        in all skillUserInvocable skills

    it "a prefix-sharing neighbour word is left intact when the skill is invoked" $
      let cat = Map.singleton "foo" (mkSkill "foo" "" "" "" SkillGlobal)
          (cleaned, skills) = parseSkillInvocations cat "check /foo-bar and /foo"
      in do
        map skillName skills `shouldBe` ["foo"]
        cleaned `shouldBe` "check /foo-bar and"

    it "non-invocable skills are ignored even when the slash token is present" $
      let s = (mkSkill "secret" "" "" "" SkillGlobal) { skillUserInvocable = False }
          cat = Map.singleton "secret" s
          (cleaned, skills) = parseSkillInvocations cat "/secret do the thing"
      in do
        skills `shouldBe` []
        cleaned `shouldBe` "/secret do the thing"

  describe "CGPT campaign: worktree name flag-injection safety" $ do
    it "accepted names never have surrounding whitespace" $ vigorous $
      forAll (T.pack <$> listOf (elements (['a'..'z'] ++ " -_./"))) $ \n ->
        not (isValidWorktreeName n) || n == T.strip n

    it "accepted names never start with a dash after stripping" $ vigorous $
      forAll (T.pack <$> listOf (elements (['a'..'z'] ++ " -_."))) $ \n ->
        not (isValidWorktreeName n) || not ("-" `T.isPrefixOf` T.strip n)

    it "whitespace-prefixed flag names are always rejected" $ vigorous $
      forAll (listOf1 (elements " \t")) $ \ws ->
      forAll genFlagLikeName $ \flag ->
        collect (T.unpack flag) $
          not (isValidWorktreeName (T.pack ws <> flag))

    it "well-formed names are accepted" $ vigorous $
      forAll genValidWorktreeName $ \n ->
        isValidWorktreeName n

    it "path separators, colon, and '..' are rejected" $ vigorous $
      forAll genValidWorktreeName $ \n ->
      forAll (elements ["../" <> T.unpack n, T.unpack n <> "/x", T.unpack n <> "\\x", "foo:bar"]) $ \bad ->
        not (isValidWorktreeName (T.pack bad))

    -- Headline reproducers from the whitespace-prefix flag-injection bug.
    it "reproducers: \" --force\" and \" -b\" are rejected" $ do
      isValidWorktreeName " --force" `shouldBe` False
      isValidWorktreeName " -b" `shouldBe` False
      isValidWorktreeName "--force" `shouldBe` False
      isValidWorktreeName "feat-x" `shouldBe` True

  describe "CGPT campaign: dialogueToMessages role alternation" $ do
    it "never emits two consecutive AssistantMsg" $ vigorous $
      forAll arbitrary $ \(items :: [TranscriptItem]) ->
      forAll (listOf1 (elements ['a'..'z'])) $ \sys ->
      forAll (listOf1 (elements ['a'..'z'])) $ \prompt ->
        let msgs = dialogueToMessages (T.pack sys) (T.pack prompt) items
            pairs = zip msgs (drop 1 msgs)
            isAsst AssistantMsg{} = True
            isAsst _              = False
            bad = [(x, y) | (x, y) <- pairs, isAsst x, isAsst y]
        in collect (length [() | AssistantMsg{} <- msgs]) $
             null bad

    it "always starts with SystemMsg and ends with a UserMsg carrying the current prompt" $ vigorous $
      forAll arbitrary $ \(items :: [TranscriptItem]) ->
        let msgs = dialogueToMessages "sys" "follow-up" items
        in case msgs of
             (SystemMsg "sys" : rest@(_:_)) ->
               case last rest of
                 -- A leftover prior user turn is merged into the new prompt
                 -- so the suffix is always the current prompt text.
                 UserMsg u -> "follow-up" `T.isSuffixOf` u
                 _         -> False
             _ -> False

    it "reproducer: consecutive DiAssistant items collapse instead of adjoining" $ do
      let items = [DiUser "u1", DiAssistant "a1", DiAssistant "a2", DiUser "u2"]
          msgs  = dialogueToMessages "sys" "u2" items
      msgs `shouldBe`
        [ SystemMsg "sys"
        , UserMsg "u1"
        , AssistantMsg (Just "a1\n\na2") []
        , UserMsg "u2"
        ]

  describe "CGPT campaign: CLI parser invariants" $ do
    it "never crashes on arbitrary argument lists" $ vigorous $
      \args -> case parseCliArgs args of
        Left _  -> True
        Right _ -> True

    it "--max-turns N (N>0) is always captured" $ vigorous $
      forAll (choose (1, 1000) :: Gen Int) $ \n ->
        case parseCliArgs ["--max-turns", show n] of
          Right opts -> optMaxTurns opts === Just n
          Left err   -> counterexample err False

    it "--max-turns=N (N>0) is always captured" $ vigorous $
      forAll (choose (1, 1000) :: Gen Int) $ \n ->
        case parseCliArgs ["--max-turns=" <> show n] of
          Right opts -> optMaxTurns opts === Just n
          Left err   -> counterexample err False

    it "non-positive --max-turns is rejected" $ vigorous $
      forAll (choose (-20, 0) :: Gen Int) $ \n ->
        case parseCliArgs ["--max-turns", show n] of
          Left _  -> True
          Right _ -> False

    it "--no-tui is sticky regardless of surrounding words" $ vigorous $
      \(wordsBefore :: [String]) (wordsAfter :: [String]) ->
        let cleanBefore = filter (/= "--") wordsBefore
            args = cleanBefore ++ ["--no-tui"] ++ wordsAfter
        in case parseCliArgs args of
             Left _     -> True
             Right opts -> optNoTui opts

    it "CLI model override wins over OS env and .env" $ vigorous $
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \cli ->
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \osM ->
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \key ->
        let envText = "OPENROUTER_API_KEY=" <> key <> "\nOPENROUTER_MODEL=from-dotenv\n"
        in case resolveConfigWith (Just cli) (Just key) (Just osM) (Just envText) of
             Right cfg -> envModel cfg === cli
             Left err  -> counterexample err False

  describe "CGPT campaign: JSON codec round-trips" $ do
    it "Message" $ vigorous $
      \(m :: Message) -> Aeson.decode (Aeson.encode m) === Just m

    it "ToolCall" $ vigorous $
      \(t :: ToolCall) -> Aeson.decode (Aeson.encode t) === Just t

    it "ToolResult" $ vigorous $
      \(t :: ToolResult) -> Aeson.decode (Aeson.encode t) === Just t

    it "PermissionMode" $ vigorous $
      \(m :: PermissionMode) -> Aeson.decode (Aeson.encode m) === Just m

    it "PermissionDecision" $ vigorous $
      \(d :: PermissionDecision) -> Aeson.decode (Aeson.encode d) === Just d

    it "GoalVerdict" $ vigorous $
      \(v :: GoalVerdict) -> Aeson.decode (Aeson.encode v) === Just v

    it "HookEvent" $ vigorous $
      \(e :: HookEvent) -> Aeson.decode (Aeson.encode e) === Just e

    it "OutputStyle (non-reserved custom)" $ vigorous $
      \(s :: OutputStyle) -> Aeson.decode (Aeson.encode s) === Just s

    it "PermissionRule" $ vigorous $
      \(r :: PermissionRule) -> Aeson.decode (Aeson.encode r) === Just r

  describe "CGPT campaign: token accounting & saturation" $ do
    it "contextSaturationPercent is always in 0..100" $ vigorous $
      \(tokens :: Int) (NonEmpty model) ->
        let p = contextSaturationPercent tokens (T.pack model)
        in p >= 0 && p <= 100

    it "addUsageToSession is monotonic in every token counter" $ vigorous $
      \(p :: NonNegative Int) (c :: NonNegative Int) (t :: NonNegative Int) (cached :: NonNegative Int) isEval ->
        let u = (mkTokenUsage (getNonNegative p) (getNonNegative c) (getNonNegative t))
                  { tuCachedTokens = getNonNegative cached }
            s0 = initialSessionTokenUsage
            s1 = addUsageToSession u isEval s0
        in stuPromptTokens s1 == tuPromptTokens u
           && stuCompletionTokens s1 == tuCompletionTokens u
           && stuTotalTokens s1 == tuTotalTokens u
           && stuCachedTokens s1 == tuCachedTokens u
           && (if isEval then stuEvaluationTokens s1 == tuTotalTokens u
                         else stuEvaluationTokens s1 == 0)

    it "classifyCompletion without the [API Error] prefix is GoalNoError" $ vigorous $
      forAll (T.pack <$> listOf (elements (['a'..'z'] ++ " "))) $ \txt ->
        not ("[API Error]: " `T.isPrefixOf` txt) ==>
          classifyCompletion txt === GoalNoError

    it "goalArgIsClear is true iff the stripped first word is a bare alias" $ vigorous $
      forAll (elements goalClearAliases) $ \alias ->
      forAll (listOf (elements " \t")) $ \lead ->
      forAll (listOf (elements " \t")) $ \trail ->
      forAll (listOf (elements ['a'..'z'])) $ \extra ->
        let arg = T.pack lead <> alias <> T.pack trail
              <> if null extra then "" else " " <> T.pack extra
        in goalArgIsClear arg === null extra

  describe "CGPT campaign: settings merge & AppleScript escape" $ do
    it "mergeSettings later Maybe-fields win" $ vigorous $
      \(m1 :: Maybe Text) (m2 :: Maybe Text) ->
        let a = defaultSettings { setModel = m1 }
            b = defaultSettings { setModel = m2 }
            laterWins x y = case x of
              Just _  -> x
              Nothing -> y
        in setModel (mergeSettings a b) === laterWins m2 m1

    it "escapeAppleScript leaves no raw unescaped double-quote" $ vigorous $
      forAll (T.pack <$> listOf (elements (['a'..'z'] ++ "\\\" \n"))) $ \raw ->
        let esc = escapeAppleScript raw
            -- After escaping, every remaining '"' is preceded by a backslash.
            ok = go (T.unpack esc)
            go [] = True
            go ('\\':_:xs) = go xs
            go ('"':_) = False
            go (_:xs) = go xs
        in collect (T.length raw) ok

  describe "CGPT campaign: task store & memory rules" $ do
    it "createTask then getTask returns the created title" $ vigorous $
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \title ->
        let (task, store) = createTask emptyTaskStore title
        in getTask store (taskId task) === Just task

    it "updateTask changes only the named task's status" $ vigorous $
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \title ->
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \st ->
        let (task, store) = createTask emptyTaskStore title
            store'        = updateTask store (taskId task) st
        in fmap taskStatus (getTask store' (taskId task)) === Just st

    it "a rule with no patterns matches every file list" $ vigorous $
      forAll (listOf genRelPath) $ \fps ->
        let rule = Rule "r.md" [] "body"
        in ruleMatchesFiles rule fps

    it "a rule with a literal pattern matches only that exact path" $ vigorous $
      forAll genRelPath $ \fp ->
      forAll (listOf genRelPath) $ \others ->
        let rule = Rule "r.md" [T.pack fp] "body"
        in ruleMatchesFiles rule others === (fp `elem` others)

  describe "CGPT campaign: porcelain status & co-author trailer" $ do
    it "appendCoAuthor is idempotent" $ vigorous $
      forAll (T.pack <$> listOf (elements (['a'..'z'] ++ " "))) $ \msg ->
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \author ->
        let first  = appendCoAuthor msg author
            second = appendCoAuthor first author
        in first === second

    it "branch names containing dots survive porcelain parsing" $ vigorous $
      forAll (elements ["release-1.0", "v2.0", "feature/fix.2.3"]) $ \b ->
        let raw = "## " <> b <> "...origin/" <> b <> "\n"
        in gsiBranch (parsePorcelainStatus "HEAD" raw) === b

    it "empty porcelain body is a clean tree on the default branch" $
      let st = parsePorcelainStatus "main" ""
      in do
        gsiClean st `shouldBe` True
        gsiBranch st `shouldBe` "main"

  describe "CGPT campaign: focus cycle & tool-output truncation" $ do
    it "nextFocus . prevFocus = id" $ do
      nextFocus (prevFocus FocusInput) `shouldBe` FocusInput
      nextFocus (prevFocus FocusTranscript) `shouldBe` FocusTranscript

    it "truncateToolOutput never grows a short input" $ vigorous $
      forAll (T.pack <$> listOf (elements (['a'..'z'] ++ "\n"))) $ \raw ->
        T.length raw <= 100 ==>
          truncateToolOutput raw === raw
