{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Second-pass coverage-guided property campaign.
--
-- Wave 1 ("Hach.CampaignSpec") covered globs, permission aliases, skill
-- tokens, worktree names, assistant-role alternation, CLI flags, and
-- JSON codecs.  This wave targets the remaining high-value seams:
-- workspace containment for memory @import and skill {{file:}} expansion,
-- task-id uniqueness, MCP tool-name invertibility, user-role
-- alternation, and path collapse.
module Hach.Campaign2Spec (spec) where

import Hach.Core
import Hach.Env
import Hach.Hooks
import Hach.Interpreter.Pure
import Hach.MCP
import Hach.Memory
import Hach.Paths
import Hach.Sessions
import Hach.Skills
import Hach.Subagents
import Hach.Tasks
import Hach.TUI.App (dialogueToMessages)
import Hach.TUI.Types
import Hach.Types

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck

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

genLifecycle :: Gen ToolLifecycle
genLifecycle = oneof
  [ pure Pending
  , pure Running
  , Finished <$> oneof
      [ ToolSuccess <$> genTxt
      , ToolError <$> (T.pack <$> listOf1 (elements ['a'..'z']))
      ]
  , Denied <$> (T.pack <$> listOf1 (elements ['a'..'z']))
  , pure Cancelled
  ]

genToolCard :: Gen ToolCard
genToolCard = ToolCard
  <$> (T.pack <$> listOf1 (elements ['a'..'z']))
  <*> (T.pack <$> listOf1 (elements ['a'..'z']))
  <*> genTxt
  <*> genLifecycle
  <*> arbitrary

genTranscriptItem :: Gen TranscriptItem
genTranscriptItem = frequency
  [ (4, TiUser      <$> genTxt)
  , (3, TiAssistant <$> genTxt)
  , (1, TiSystem    <$> genTxt)
  , (1, TiNotice    <$> genTxt)
  , (2, TiToolCard  <$> genToolCard)
  ]

genTranscript :: Gen [TranscriptItem]
genTranscript = listOf genTranscriptItem

-- MCP name parts must be non-empty, must not contain the '__' delimiter,
-- and must not start or end with '_' (either edge fuses with '__' into '___').
genMcpPart :: Gen Text
genMcpPart =
  (T.pack <$> listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ "-_")))
    `suchThat` (not . T.null)
    `suchThat` (not . T.isInfixOf "__")
    `suchThat` (not . T.isPrefixOf "_")
    `suchThat` (not . T.isSuffixOf "_")

genCustomTaskId :: Gen Text
genCustomTaskId = oneof
  [ T.pack <$> listOf1 (elements ['a'..'z'])
  , do n <- choose (1, 8 :: Int)
       pure ("task-" <> T.pack (show n))
  ]

-- ---------------------------------------------------------------------------
-- * Coverage labels
-- ---------------------------------------------------------------------------

mcpProfile :: Text -> Text -> String
mcpProfile server tool
  | "_" `T.isInfixOf` server || "_" `T.isInfixOf` tool = "underscore"
  | T.any (`elem` ['0'..'9']) server                   = "numeric-server"
  | otherwise                                          = "plain"

taskIdProfile :: [Text] -> String
taskIdProfile ids
  | any ("task-" `T.isPrefixOf`) ids = "occupies-generated-namespace"
  | otherwise                        = "custom-only"

-- ---------------------------------------------------------------------------
-- * Properties
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "CGPT wave-2: MCP tool-name invertibility" $ do
    it "format/parse is an inverse for delimiter-free parts" $ vigorous $
      forAll genMcpPart $ \server ->
      forAll genMcpPart $ \tool ->
        collect (mcpProfile server tool) $
          parseMcpToolName (formatMcpToolName server tool) === Just (server, tool)

    it "names with extra __ segments do not parse as a server/tool pair" $ vigorous $
      forAll genMcpPart $ \a ->
      forAll genMcpPart $ \b ->
      forAll genMcpPart $ \c ->
        parseMcpToolName ("mcp__" <> a <> "__" <> b <> "__" <> c) === Nothing

    it "reproducer: mcp__my__server__query is rejected rather than split early" $
      parseMcpToolName "mcp__my__server__query" `shouldBe` Nothing

    it "reproducer: mcp__sqlite__query still round-trips" $
      parseMcpToolName (formatMcpToolName "sqlite" "query")
        `shouldBe` Just ("sqlite", "query")

    it "a server name ending in _ or a tool name starting with _ fuses the delimiter and is rejected" $ do
      parseMcpToolName (formatMcpToolName "srv_" "tool") `shouldBe` Nothing
      parseMcpToolName (formatMcpToolName "srv" "_tool") `shouldBe` Nothing

  describe "CGPT wave-2: task-id uniqueness" $ do
    it "createTask never overwrites an existing custom id" $ vigorous $
      forAll (listOf1 genCustomTaskId) $ \ids ->
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \title ->
        let store0 = foldl (\s i -> snd (createTaskWithId s i "held")) emptyTaskStore ids
            (task, store1) = createTask store0 title
        in collect (taskIdProfile ids) $
             all (`Map.member` store1) (Map.keys store0)
             && not (Map.member (taskId task) store0)
             && getTask store1 (taskId task) == Just task
             && all (\i -> getTask store1 i == getTask store0 i) (Map.keys store0)

    it "reproducer: custom task-2 then createTask keeps the original" $ do
      let (held, store0) = createTaskWithId emptyTaskStore "task-2" "held"
          (created, store1) = createTask store0 "fresh"
      taskId held `shouldBe` "task-2"
      taskId created `shouldNotBe` "task-2"
      getTask store1 "task-2" `shouldBe` Just held
      fmap taskTitle (getTask store1 (taskId created)) `shouldBe` Just "fresh"

  describe "CGPT wave-2: dialogueToMessages user-role alternation" $ do
    it "never emits two consecutive UserMsg" $ vigorous $
      forAll genTranscript $ \items ->
        let msgs = dialogueToMessages "sys" "follow-up" items
            pairs = zip msgs (drop 1 msgs)
            isUser UserMsg{} = True
            isUser _         = False
            bad = [(x, y) | (x, y) <- pairs, isUser x, isUser y]
        in collect (length [() | UserMsg{} <- msgs]) $
             null bad

    it "reproducer: adjacent DiUser items collapse instead of adjoining" $ do
      let items = [DiUser "u1", DiUser "u2", DiAssistant "a1"]
          msgs  = dialogueToMessages "sys" "next" items
      msgs `shouldBe`
        [ SystemMsg "sys"
        , UserMsg "u1\n\nu2"
        , AssistantMsg (Just "a1") []
        , UserMsg "next"
        ]

  describe "CGPT wave-2: collapseLogicalPath" $ do
    it "collapsing a path twice is identity" $ vigorous $
      forAll (listOf1 genSeg) $ \segs ->
        let p = foldr1 (\a b -> a <> "/" <> b) segs
            collapsed = collapseLogicalPath p
        in collapseLogicalPath collapsed === collapsed

    it "a trailing /.. drops the last non-root segment" $ vigorous $
      forAll genSeg $ \a ->
      forAll genSeg $ \b ->
        let p = "/" <> a <> "/" <> b <> "/.."
        in collapseLogicalPath p === ("/" <> a)

    it "reproducer: /workspace/../etc/passwd collapses to /etc/passwd" $
      collapseLogicalPath "/workspace/../etc/passwd" `shouldBe` "/etc/passwd"

  describe "CGPT wave-2: CLI -- terminator" $ do
    it "--no-tui before -- sets the flag; after -- it is a prompt word" $ vigorous $
      forAll (listOf (elements ["foo", "bar", "x"])) $ \wordsBefore ->
      forAll (listOf (elements ["foo", "bar", "y"])) $ \wordsAfter ->
        case parseCliArgs (wordsBefore ++ ["--no-tui", "--"] ++ wordsAfter) of
          Right opts -> optNoTui opts
          Left _     -> False

    it "--no-tui after -- is not a flag" $
      case parseCliArgs ["--", "--no-tui"] of
        Right opts -> do
          optNoTui opts `shouldBe` False
          optPrompt opts `shouldBe` Just "--no-tui"
        Left err -> expectationFailure err

  describe "CGPT wave-2: cost, hooks, and subagent bounds" $ do
    it "estimateCostUsd is non-negative for non-negative token counts" $ vigorous $
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \model ->
      forAll (choose (0, 1000000) :: Gen Int) $ \p ->
      forAll (choose (0, 1000000) :: Gen Int) $ \c ->
        estimateCostUsd model p c >= 0

    it "canSpawnSubagent is false at or above either bound" $ vigorous $
      forAll (choose (0, 8) :: Gen Int) $ \depth ->
      forAll (choose (1, 8) :: Gen Int) $ \maxD ->
      forAll (choose (0, 25) :: Gen Int) $ \running ->
        let ok = canSpawnSubagent depth maxD running
        in collect (if ok then "allowed" else "blocked" :: String) $
             ok === (depth < maxD && running < defaultMaxConcurrency)

    it "wildcard hook matchers always match" $ vigorous $
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \tool ->
        let h = HookHandler (HookCommand "x") (Just "*") False
        in filterMatchingHandlers (Just tool) [h] === [h]

    it "parseHookOutput 0 is always the default pass" $ vigorous $
      forAll (T.pack <$> listOf (elements ['a'..'z'])) $ \out ->
        parseHookOutput 0 out === defaultHookResult

    it "TokenUsage JSON round-trips for finite fields" $ vigorous $
      forAll (choose (0, 10000) :: Gen Int) $ \p ->
      forAll (choose (0, 10000) :: Gen Int) $ \c ->
        let u = mkTokenUsage p c (p + c)
        in Aeson.decode (Aeson.encode u) === Just u

  describe "CGPT wave-2: agentLoop history is monotone" $ do
    it "final history is at least as long as the initial history" $ vigorous $
      forAll (choose (1, 6) :: Gen Int) $ \n ->
      forAll (choose (1, 8) :: Gen Int) $ \maxTurns ->
        let steps = replicate n
              (\_ _ -> Right (AssistantResponse (Just "ok") [] Nothing))
            env = emptyMockEnv { mockLLMSteps = steps }
            cfg = AgentConfig "m" (Just "sys") (Just maxTurns)
            initHist = [UserMsg "hi"]
            ((_, finalHist), _) = runPure env (agentLoop cfg [] initHist)
        in length finalHist >= length initHist

  describe "CGPT wave-2: memory @import workspace containment" $ do
    let sandbox = "dist-newstyle/test-campaign-memory"
    around_ (\action -> do
      createDirectoryIfMissing True sandbox
      action
      removeDirectoryRecursive sandbox) $ do

      it "relative imports inside the sandbox still expand" $ do
        TIO.writeFile (sandbox </> "base.md") "Base\n@import sub.md\n"
        TIO.writeFile (sandbox </> "sub.md") "Sub\n"
        resolved <- resolveMemoryImports sandbox 4 (sandbox </> "base.md")
        T.unpack resolved `shouldContain` "Base"
        T.unpack resolved `shouldContain` "Sub"
        T.unpack resolved `shouldNotContain` "Import denied"

      it "absolute imports outside the sandbox are denied" $ do
        TIO.writeFile (sandbox </> "base.md") "@import /etc/passwd\nKeep\n"
        resolved <- resolveMemoryImports sandbox 4 (sandbox </> "base.md")
        T.unpack resolved `shouldContain` "Import denied"
        T.unpack resolved `shouldNotContain` "root:"

      it "../ traversal that leaves the sandbox is denied" $ do
        let inner = sandbox </> "inner"
        createDirectoryIfMissing True inner
        TIO.writeFile (sandbox </> "secret.txt") "SANDBOX-SECRET"
        -- secret is still inside sandbox; escape to a sibling of sandbox
        TIO.writeFile (inner </> "base.md") "@import ../../Campaign2Spec.hs\n"
        resolved <- resolveMemoryImports sandbox 4 (inner </> "base.md")
        T.unpack resolved `shouldContain` "Import denied"
        T.unpack resolved `shouldNotContain` "module Hach.Campaign2Spec"

  describe "CGPT wave-2: skill {{file:}} workspace containment" $ do
    let sandbox = "dist-newstyle/test-campaign-skills"
    around_ (\action -> do
      createDirectoryIfMissing True sandbox
      action
      removeDirectoryRecursive sandbox) $ do

      it "relative file placeholders inside the sandbox still expand" $ do
        TIO.writeFile (sandbox </> "note.txt") "hello-from-sandbox"
        res <- injectDynamicContext sandbox "X {{file:note.txt}} Y"
        res `shouldBe` "X hello-from-sandbox Y\n"

      it "absolute file placeholders outside the sandbox are left intact" $ do
        res <- injectDynamicContext sandbox "X {{file:/etc/passwd}} Y"
        T.unpack res `shouldContain` "{{file:/etc/passwd}}"
        T.unpack res `shouldNotContain` "root:"

      it "../ traversal that leaves the sandbox is left intact" $ do
        res <- injectDynamicContext sandbox "X {{file:../../test/Hach/Campaign2Spec.hs}} Y"
        T.unpack res `shouldContain` "{{file:../../test/Hach/Campaign2Spec.hs}}"
        T.unpack res `shouldNotContain` "module Hach.Campaign2Spec"

      it "resolveWorkspacePath rejects absolute and escaping relative paths" $ do
        root <- canonicalizePath sandbox
        absRes <- resolveWorkspacePath root "/etc/passwd"
        case absRes of
          Left _  -> pure ()
          Right p -> expectationFailure ("absolute path allowed: " <> p)
        escRes <- resolveWorkspacePath root "../../test/Hach/Campaign2Spec.hs"
        case escRes of
          Left _  -> pure ()
          Right p -> expectationFailure ("escaping path allowed: " <> p)
        okRes <- resolveWorkspacePath root "note.txt"
        case okRes of
          Right _ -> pure ()
          Left err -> expectationFailure err
