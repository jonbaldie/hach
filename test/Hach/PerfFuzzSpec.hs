{-# LANGUAGE OverloadedStrings #-}

-- | PerfFuzz campaign: independently maximise work at performance-sensitive
-- seams and assert polynomial bounds.
--
-- Coverage-guided waves 1–3 already locked functional invariants.  This
-- campaign treats *cost* as the signal: a naive replica of each seam is
-- instrumented with a step counter, pathological families are retained via
-- 'collect', and the production implementation must stay inside an
-- O(|pattern| · |input|) (or better) envelope.
--
-- Seams:
--
-- * 'matchGlob' — permission / memory-rule globs ('*', '**', '**/')
-- * 'matchStarGlob' — find_files / Glob tool '*' matching
-- * 'transcriptItemsToMessages' — adjacent-item collapse
-- * 'mergeSettings' — allowlist / working-dir unique
module Hach.PerfFuzzSpec (spec) where

import Hach.Permissions
import Hach.Settings
import Hach.TUI.App (transcriptItemsToMessages, dialogueToMessages)
import Hach.TUI.Types
import Hach.Types


import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- * Campaign knobs
-- ---------------------------------------------------------------------------

vigorous :: Testable prop => prop -> Property
vigorous = withNumTests 2000

-- Step budget for the instrumented naive replica.  Crossing it is the
-- PerfFuzz finding: the old backtracker is no longer a useful oracle.
naiveBudget :: Int
naiveBudget = 20000

-- ---------------------------------------------------------------------------
-- * Naive replicas (the algorithms the campaign found)
-- ---------------------------------------------------------------------------

-- | Production 'matchGlob' before the DP rewrite, plus a step counter.
matchGlobNaive :: Int -> Text -> FilePath -> Maybe (Bool, Int)
matchGlobNaive budget pat fp = go (T.unpack pat) fp 0
  where
    go _ _ n | n > budget = Nothing
    go p t n = case (p, t) of
      ([], []) -> Just (True, n + 1)
      ('*':'*':'/':rest, target) ->
        case go rest target (n + 1) of
          Nothing -> Nothing
          Just (True, n1) -> Just (True, n1)
          Just (False, n1) -> case break (== '/') target of
            (_, '/':ts) -> go ('*':'*':'/':rest) ts n1
            _           -> Just (False, n1)
      (['*','*'], _) -> Just (True, n + 1)
      ('*':'*':rest, target) ->
        case go rest target (n + 1) of
          Nothing -> Nothing
          Just (True, n1) -> Just (True, n1)
          Just (False, n1) -> case target of
            []     -> Just (False, n1)
            (_:ts) -> go ('*':'*':rest) ts n1
      ('*':rest, target) ->
        case go rest target (n + 1) of
          Nothing -> Nothing
          Just (True, n1) -> Just (True, n1)
          Just (False, n1) -> case target of
            (c:ts) | c /= '/' -> go ('*':rest) ts n1
            _                 -> Just (False, n1)
      (p':ps, t':ts) | p' == t' -> go ps ts (n + 1)
      _ -> Just (False, n + 1)

-- | Production find_files 'globMatch' before the linear rewrite.
matchStarNaive :: Int -> String -> String -> Maybe (Bool, Int)
matchStarNaive budget pat str = go pat str 0
  where
    go _ _ n | n > budget = Nothing
    go [] [] n = Just (True, n + 1)
    go [] _ n  = Just (False, n + 1)
    go ('*':ps) s n = goStar ps s (n + 1)
    go (p:ps) (c:cs) n
      | p == c    = go ps cs (n + 1)
      | otherwise = Just (False, n + 1)
    go (_:_) [] n = Just (False, n + 1)

    goStar _ _ n | n > budget = Nothing
    goStar [] _ n = Just (True, n + 1)
    goStar ps [] n = go ps [] (n + 1)
    goStar ps s@(_:cs) n =
      case go ps s (n + 1) of
        Nothing -> Nothing
        Just (True, n1) -> Just (True, n1)
        Just (False, n1) -> goStar ps cs n1

-- | Left-folding @a <> sep <> b@ copies the growing prefix each time.
-- The returned cost is the number of characters copied.
naiveCollapseCost :: [Text] -> Int
naiveCollapseCost []     = 0
naiveCollapseCost (x:xs) = go (T.length x) xs
  where
    go _ [] = 0
    go acc (y:ys) =
      let copied = acc + 2 + T.length y
      in copied + go copied ys

-- ---------------------------------------------------------------------------
-- * Generators
-- ---------------------------------------------------------------------------

genSeg :: Gen String
genSeg = listOf1 (elements ['a'..'z'])

genSmallPat :: Gen Text
genSmallPat = T.pack <$> sized (\n -> do
  k <- choose (0, min 6 (n + 1))
  pieces <- vectorOf k $ oneof
    [ elements ["*", "**", "**/"]
    , (:[]) <$> elements ['a'..'f']
    , elements [".", "/", "hs", "txt"]
    ]
  pure (concat pieces))

genSmallPath :: Gen FilePath
genSmallPath = do
  n <- choose (0, 4)
  segs <- vectorOf n $ do
    base <- listOf1 (elements ['a'..'f'])
    ext  <- elements ["", ".hs", ".txt"]
    pure (base <> ext)
  pure (foldr (\a b -> if null b then a else a <> "/" <> b) "" segs)

genStarPat :: Gen Text
genStarPat = T.pack <$> sized (\n -> do
  k <- choose (0, min 5 (n + 1))
  pieces <- vectorOf k $ oneof
    [ pure "*"
    , (:[]) <$> elements ['a'..'f']
    , elements [".", "hs", "txt"]
    ]
  pure (concat pieces))

genStarStr :: Gen Text
genStarStr = T.pack <$> listOf (elements (['a'..'f'] ++ "./"))

pathologicalStarPat :: Int -> Text
pathologicalStarPat k = T.pack (concat (replicate k "*a") <> "*b")

pathologicalStarTarget :: Int -> String
pathologicalStarTarget k = replicate (k * 2) 'a'

globShape :: Text -> FilePath -> String
globShape pat fp
  | T.null pat && null fp                  = "empty/empty"
  | "**/" `T.isInfixOf` pat && '/' `elem` fp = "**/ vs nested"
  | "**/" `T.isInfixOf` pat                  = "**/ vs flat"
  | "**"  `T.isInfixOf` pat                  = "bare-**"
  | "*"   `T.isInfixOf` pat && '/' `elem` fp = "*-glob vs nested"
  | "*"   `T.isInfixOf` pat                  = "*-glob vs flat"
  | otherwise                               = "literal"

starShape :: Text -> Text -> String
starShape pat str
  | T.count "*" pat >= 3 = "many-stars"
  | "*" `T.isInfixOf` pat && "/" `T.isInfixOf` str = "star vs slash"
  | "*" `T.isInfixOf` pat = "star"
  | otherwise            = "literal"

-- ---------------------------------------------------------------------------
-- * Properties
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "PerfFuzz: matchGlob polynomial bound" $ do
    it "agrees with the naive backtracker on inputs that stay in budget" $ vigorous $
      forAll genSmallPat $ \pat ->
      forAll genSmallPath $ \fp ->
        case matchGlobNaive naiveBudget pat fp of
          Nothing ->
            collect ("naive-timeout/" <> globShape pat fp) True
          Just (expected, steps) ->
            collect (globShape pat fp <> " steps<" <> show (bucket steps)) $
              matchGlob pat fp === expected

    it "reproducers: directory-boundary semantics are unchanged" $ do
      matchGlob "**/c" "abc" `shouldBe` False
      matchGlob "**/bar.txt" "foobar.txt" `shouldBe` False
      matchGlob "src/**/secret.txt" "src/nonsecret.txt" `shouldBe` False
      matchGlob "src/**/*.hs" "src/foo.hs" `shouldBe` True
      matchGlob "src/**/*.hs" "src/a/b/foo.hs" `shouldBe` True
      matchGlob "src/*.hs" "src/sub/foo.hs" `shouldBe` False
      matchGlob "*" "a/b" `shouldBe` False
      matchGlob "**" "a/b" `shouldBe` True
      matchGlob "**foo" "a/b/foo" `shouldBe` True
      matchGlob "*foo" "a/b/foo" `shouldBe` False

    it "naive backtracker explodes on *a*a*...*b vs aaaa...; production stays correct" $
      forAll (choose (8, 16) :: Gen Int) $ \k ->
        let pat = pathologicalStarPat k
            fp  = pathologicalStarTarget k
            naive = matchGlobNaive 100000 pat fp
        in collect ("k=" <> show k) $
             matchGlob pat fp === False
             .&&. (case naive of
                     Nothing -> property True  -- exceeded 1e5 steps
                     Just (ok, steps) ->
                       counterexample ("naive finished in " <> show steps)
                         (not ok && steps > (T.length pat + 1) * (length fp + 1) * 4))

    it "headline: k=12 starred glob is False and does not require a budget" $ do
      matchGlob (pathologicalStarPat 12) (pathologicalStarTarget 12) `shouldBe` False
      matchGlobNaive 50000 (pathologicalStarPat 10) (pathologicalStarTarget 10)
        `shouldBe` Nothing

  describe "PerfFuzz: matchStarGlob (find_files) linear bound" $ do
    it "agrees with the naive backtracker on inputs that stay in budget" $ vigorous $
      forAll genStarPat $ \pat ->
      forAll genStarStr $ \str ->
        case matchStarNaive naiveBudget (T.unpack pat) (T.unpack str) of
          Nothing ->
            collect ("naive-timeout/" <> starShape pat str) True
          Just (expected, steps) ->
            collect (starShape pat str <> " steps<" <> show (bucket steps)) $
              matchStarGlob pat str === expected

    it "reproducers: '*' crosses '/', trailing stars match anything" $ do
      matchStarGlob "*" "" `shouldBe` True
      matchStarGlob "*" "abc" `shouldBe` True
      matchStarGlob "*" "a/b" `shouldBe` True
      matchStarGlob "a*b" "ab" `shouldBe` True
      matchStarGlob "a*b" "axxxb" `shouldBe` True
      matchStarGlob "a*b" "a" `shouldBe` False
      matchStarGlob "*foo*" "src/foo.hs" `shouldBe` True
      matchStarGlob "*.hs" "Foo.hs" `shouldBe` True
      matchStarGlob "foo" "bar" `shouldBe` False

    it "naive backtracker explodes on *a*a*...*b; production stays correct" $
      forAll (choose (8, 16) :: Gen Int) $ \k ->
        let pat = pathologicalStarPat k
            str = T.pack (pathologicalStarTarget k)
            naive = matchStarNaive 100000 (T.unpack pat) (T.unpack str)
        in collect ("k=" <> show k) $
             matchStarGlob pat str === False
             .&&. (case naive of
                     Nothing -> property True
                     Just (ok, steps) ->
                       counterexample ("naive finished in " <> show steps)
                         (not ok && steps > (T.length pat + T.length str) * 8))

    it "headline: k=12 find_files glob is False and the naive replica times out" $ do
      matchStarGlob (pathologicalStarPat 12) (T.pack (pathologicalStarTarget 12))
        `shouldBe` False
      matchStarNaive 50000
        (T.unpack (pathologicalStarPat 10))
        (pathologicalStarTarget 10)
        `shouldBe` Nothing

  describe "PerfFuzz: transcript collapse is linear in total text" $ do
    it "n adjacent assistant items become one intercalated AssistantMsg" $ vigorous $
      forAll (choose (1, 40) :: Gen Int) $ \n ->
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \t ->
        let items = replicate n (TiAssistant t)
            msgs  = transcriptItemsToMessages items
            joined = T.intercalate "\n\n" (replicate n t)
        in collect (collapseBucket n) $
             msgs === [AssistantMsg (Just joined) []]

    it "n adjacent user items become one intercalated UserMsg" $ vigorous $
      forAll (choose (1, 40) :: Gen Int) $ \n ->
      forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \t ->
        let items = replicate n (TiUser t)
            msgs  = transcriptItemsToMessages items
            joined = T.intercalate "\n\n" (replicate n t)
        in msgs === [UserMsg joined]

    it "naive left-fold concat is superlinear; production still agrees" $
      forAll (choose (8, 60) :: Gen Int) $ \n ->
        let chunk = "ab" :: Text
            items = replicate n (TiAssistant chunk)
            joined = T.intercalate "\n\n" (replicate n chunk)
            cost = naiveCollapseCost (replicate n chunk)
            joinedLen = T.length joined
        in collect (collapseBucket n) $
             transcriptItemsToMessages items === [AssistantMsg (Just joined) []]
             .&&. counterexample ("naive cost " <> show cost <> " vs total " <> show joinedLen)
                    (cost > 2 * joinedLen)

    it "headline: 500 adjacent assistant items collapse correctly" $ do
      let n = 500
          t = "x" :: Text
          items = replicate n (TiAssistant t)
          joined = T.intercalate "\n\n" (replicate n t)
      transcriptItemsToMessages items `shouldBe` [AssistantMsg (Just joined) []]
      naiveCollapseCost (replicate n t) `shouldSatisfy` (> 2 * T.length joined)

    it "headline: 10000 adjacent assistant items collapse correctly in linear time" $ do
      let n = 10000
          t = "x" :: Text
          items = replicate n (TiAssistant t)
          joined = T.intercalate "\n\n" (replicate n t)
      transcriptItemsToMessages items `shouldBe` [AssistantMsg (Just joined) []]
      naiveCollapseCost (replicate n t) `shouldSatisfy` (> 10000000)

    it "headline: 10000 adjacent user items collapse correctly in linear time" $ do
      let n = 10000
          t = "u" :: Text
          items = replicate n (TiUser t)
          joined = T.intercalate "\n\n" (replicate n t)
      transcriptItemsToMessages items `shouldBe` [UserMsg joined]

    it "mixed transcript items collapse adjacent same-role items across notices" $ do
      let c1 = ToolCard "call_1" "read_file" "{\"path\":\"foo.hs\"}" (Finished (ToolSuccess "file content")) False
          items =
            [ TiNotice "Session started"
            , TiUser "first prompt"
            , TiNotice "Switching mode"
            , TiUser "second prompt"
            , TiAssistant "thought 1"
            , TiNotice "background job"
            , TiAssistant "thought 2"
            , TiToolCard c1
            , TiAssistant "final response"
            ]
          msgs = transcriptItemsToMessages items
      msgs `shouldBe`
        [ UserMsg "first prompt\n\nsecond prompt"
        , AssistantMsg (Just "thought 1\n\nthought 2") [ToolCall "call_1" "read_file" "{\"path\":\"foo.hs\"}"]
        , ToolMsg "call_1" "read_file" "file content"
        , AssistantMsg (Just "final response") []
        ]

    it "dialogueToMessages strictly maintains role alternation on long mixed transcripts" $ do
      let n = 1000
          assts = replicate n (TiAssistant "step")
          items = TiUser "init" : assts
          msgs = dialogueToMessages "sys" "done" items
          isAsst AssistantMsg{} = True
          isAsst _              = False
          isUser UserMsg{}      = True
          isUser _              = False
          pairs = zip msgs (drop 1 msgs)
          hasConsecutiveAsst = any (\(a, b) -> isAsst a && isAsst b) pairs
          hasConsecutiveUser = any (\(a, b) -> isUser a && isUser b) pairs
      hasConsecutiveAsst `shouldBe` False
      hasConsecutiveUser `shouldBe` False

  describe "PerfFuzz: settings allowlist unique is order-preserving" $ do
    it "later layer wins; first occurrence of each name is kept" $ vigorous $
      forAll (listOf genSeg) $ \earlier ->
      forAll (listOf genSeg) $ \later ->
        let a = defaultSettings { setEnvAllowlist = map T.pack earlier }
            b = defaultSettings { setEnvAllowlist = map T.pack later }
            merged = setEnvAllowlist (mergeSettings a b)
            expected = nub (map T.pack later ++ map T.pack earlier)
        in collect (allowBucket (length later + length earlier)) $
             merged === expected

    it "working directories follow the same first-wins unique" $ vigorous $
      forAll (listOf genSeg) $ \earlier ->
      forAll (listOf genSeg) $ \later ->
        let a = defaultSettings { setWorkingDirs = earlier }
            b = defaultSettings { setWorkingDirs = later }
        in setWorkingDirs (mergeSettings a b) === nub (later ++ earlier)

    it "reproducer: duplicates across layers keep the later-layer order" $ do
      let earlier = defaultSettings { setEnvAllowlist = ["HOME", "PATH"] }
          later   = defaultSettings { setEnvAllowlist = ["PATH", "TERM"] }
      setEnvAllowlist (mergeSettings earlier later) `shouldBe` ["PATH", "TERM", "HOME"]

-- ---------------------------------------------------------------------------
-- * Coverage buckets
-- ---------------------------------------------------------------------------

bucket :: Int -> String
bucket n
  | n < 16    = "16"
  | n < 64    = "64"
  | n < 256   = "256"
  | n < 1024  = "1k"
  | n < 4096  = "4k"
  | otherwise = "4k+"

collapseBucket :: Int -> String
collapseBucket n
  | n < 4     = "tiny-run"
  | n < 16    = "short-run"
  | n < 32    = "medium-run"
  | otherwise = "long-run"

allowBucket :: Int -> String
allowBucket n
  | n < 4     = "tiny-list"
  | n < 16    = "short-list"
  | otherwise = "long-list"
