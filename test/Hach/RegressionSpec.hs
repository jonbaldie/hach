{-# LANGUAGE OverloadedStrings #-}

module Hach.RegressionSpec (spec) where

import Hach.Env
import Hach.Git (parsePorcelainStatus)
import Hach.Paths (isProtectedPath)
import Hach.Permissions (matchRule)
import Hach.Settings (Settings(..), defaultSettings, mergeSettings)
import Hach.Tools (countOccurrencesUpToTwo)
import Hach.Types

import qualified Data.Aeson as Aeson
import Data.Char (toLower, toUpper)
import Data.List (intercalate, nub)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "High-level regression laws" $ do
  describe "startupIntent" $
    it "prefers help, then version, then exec, then init, then headless" $
      forAll genStartup $ \opts ->
        startupIntent opts === expectedIntent opts

  describe "parseEffortLevel" $ do
    it "accepts the canonical name in any case with surrounding whitespace" $
      forAll (elements [minBound .. maxBound]) $ \level ->
        forAll genPad $ \lead ->
          forAll genPad $ \trail ->
            forAll (genCaseFlip (effortLevelName level)) $ \spelled ->
              parseEffortLevel (lead <> spelled <> trail) === Right level

    it "rejects a token that is not a supported effort name" $
      forAll genUnknownEffort $ \token ->
        parseEffortLevel token
          === Left
            ( "Unsupported effort_level: "
                <> T.unpack (T.strip token)
                <> ". Supported values: "
                <> T.unpack (T.intercalate ", " supportedEffortLevels)
            )

  describe "EffortLevel JSON" $
    it "binds both codecs to effortLevelName" $
      forAll (elements [minBound .. maxBound]) $ \level ->
        ( Aeson.encode level
        , Aeson.decode (Aeson.encode (effortLevelName level))
        )
          === ( Aeson.encode (effortLevelName level)
             , Just level
             )

  describe "addUsageToSession" $
    it "sums two turns onto an empty session and counts evaluation tokens only when asked" $
      forAll genUsage $ \u1 ->
        forAll genUsage $ \u2 ->
          forAll arbitrary $ \e1 ->
            forAll arbitrary $ \e2 ->
              let s1 = addUsageToSession u1 e1 initialSessionTokenUsage
                  s2 = addUsageToSession u2 e2 s1
                  eval = (if e1 then tuTotalTokens u1 else 0) + (if e2 then tuTotalTokens u2 else 0)
              in ( stuPromptTokens s2
                 , stuCompletionTokens s2
                 , stuTotalTokens s2
                 , stuCachedTokens s2
                 , stuEvaluationTokens s2
                 , stuTotalCost s2
                 )
                 === ( tuPromptTokens u1 + tuPromptTokens u2
                     , tuCompletionTokens u1 + tuCompletionTokens u2
                     , tuTotalTokens u1 + tuTotalTokens u2
                     , tuCachedTokens u1 + tuCachedTokens u2
                     , eval
                     , combineCosts (tuCost u1) (tuCost u2)
                     )

  describe "matchRule" $ do
    it "does not apply a path-scoped rule when the call has no path" $
      forAll genAction $ \action ->
        forAll genTool $ \tool ->
          forAll genGlob $ \glob ->
            matchRule (PermissionRule action (Just "*") (Just glob)) tool Nothing
              === (Nothing :: Maybe PermissionDecision)

    it "matches a tool name case-insensitively and records that name in the decision" $
      forAll genAction $ \action ->
        forAll genTool $ \tool ->
          forAll (genCaseFlip tool) $ \spelled ->
            matchRule (PermissionRule action (Just spelled) Nothing) tool Nothing
              === Just (decisionFor action tool)

    it "lets a star or an omitted tool match any call" $
      forAll genAction $ \action ->
        forAll genTool $ \tool ->
          let open = Just (decisionFor action tool)
          in ( matchRule (PermissionRule action (Just "*") Nothing) tool Nothing
             , matchRule (PermissionRule action Nothing Nothing) tool Nothing
             )
             === (open, open)

    it "does not match a different tool name" $
      forAll genAction $ \action ->
        forAll genTool $ \ruleTool ->
          forAll (genTool `suchThat` (/= ruleTool)) $ \callTool ->
            matchRule (PermissionRule action (Just ruleTool) Nothing) callTool Nothing
              === (Nothing :: Maybe PermissionDecision)

  describe "mergeSettings" $
    it "appends permission rules, later layer first" $
      forAll (listOf genRule) $ \earlierRules ->
        forAll (listOf genRule) $ \laterRules ->
          let earlier = defaultSettings { setPermissionRules = earlierRules }
              later = defaultSettings { setPermissionRules = laterRules }
          in setPermissionRules (mergeSettings earlier later)
               === laterRules ++ earlierRules

  describe "isProtectedPath" $
    it "protects a path exactly when one segment is .git, .claude, .agents, or .agent" $
      forAll genPathSegments $ \segments ->
        isProtectedPath (intercalate "/" segments)
          === any (`elem` protectedSegments) segments

  describe "resolveWorkingDirs" $
    it "drops empty directories and keeps the first spelling, flags before settings" $
      forAll genDirs $ \flagDirs ->
        forAll genDirs $ \settingsDirs ->
          resolveWorkingDirs flagDirs (defaultSettings { setWorkingDirs = settingsDirs })
            === nub (filter (not . null) (flagDirs ++ settingsDirs))

  describe "resolveMaxBudgetUsd" $ do
    it "keeps the CLI ceiling, including a negative or infinite one" $
      forAll genFlagBudget $ \flag ->
        forAll genMaybeBudget $ \settingsBudget ->
          resolveMaxBudgetUsd (Just flag) (defaultSettings { setMaxBudgetUsd = settingsBudget })
            === Just flag

    it "keeps a NaN CLI ceiling instead of the settings value" $
      forAll genMaybeBudget $ \settingsBudget ->
        fmap isNaN (resolveMaxBudgetUsd (Just (0 / 0)) (defaultSettings { setMaxBudgetUsd = settingsBudget }))
          === Just True

    it "drops a negative or non-finite settings ceiling and keeps a finite non-negative one" $
      forAll genRejectedBudget $ \bad ->
        forAll genNonNegBudget $ \good ->
          ( resolveMaxBudgetUsd Nothing (defaultSettings { setMaxBudgetUsd = Just bad })
          , resolveMaxBudgetUsd Nothing (defaultSettings { setMaxBudgetUsd = Just good })
          , resolveMaxBudgetUsd Nothing defaultSettings
          )
          === (Nothing, Just good, Nothing)

  describe "countOccurrencesUpToTwo" $ do
    it "returns 0 for an empty needle" $
      forAll genHay $ \hay ->
        countOccurrencesUpToTwo "" hay === 0

    it "never reports more than two matches" $
      forAll genRawNeedle $ \needle ->
        forAll genHay $ \hay ->
          let found = countOccurrencesUpToTwo needle hay
          in found === 0 .||. found === 1 .||. found === 2

    it "agrees with a full count, capped at two, when the needle cannot overlap itself" $
      forAll genNonOverlapNeedle $ \needle ->
        forAll genHay $ \hay ->
          countOccurrencesUpToTwo needle hay === min 2 (T.count needle hay)

  describe "parsePorcelainStatus" $ do
    it "records only the destination of a rename or copy" $
      forAll genPathName $ \old ->
        forAll genPathName $ \new ->
          forAll (elements ["R ", "C ", "RM"]) $ \code ->
            let line = T.pack code <> " " <> T.pack old <> " -> " <> T.pack new
                info = parsePorcelainStatus "main" line
            in (gsiModified info, gsiUntracked info) === ([new], [])

    it "strips quotes around a path that contains a space" $
      forAll genPathName $ \leftName ->
        forAll genPathName $ \rightName ->
          let shown = leftName ++ " " ++ rightName
              line = " M \"" <> T.pack shown <> "\""
              info = parsePorcelainStatus "main" line
          in gsiModified info === [shown]

genStartup :: Gen CliOptions
genStartup = do
  help <- arbitrary
  version <- arbitrary
  exec <- oneof [pure Nothing, Just . T.pack <$> listOf (elements ['a'..'z'])]
  doInit <- arbitrary
  noTui <- arbitrary
  pure defaultCliOptions
    { optHelp = help
    , optVersion = version
    , optExec = exec
    , optInit = doInit
    , optNoTui = noTui
    }

expectedIntent :: CliOptions -> StartupIntent
expectedIntent opts =
  case concat
        [ [IntentHelp | optHelp opts]
        , [IntentVersion | optVersion opts]
        , map IntentExec (maybeToList (optExec opts))
        , [IntentInit | optInit opts]
        , [IntentHeadless | optNoTui opts]
        ] of
    (intent : _) -> intent
    [] -> IntentTui

genPad :: Gen T.Text
genPad = T.pack <$> listOf (elements " \t\n\r\v\f")

genCaseFlip :: T.Text -> Gen T.Text
genCaseFlip raw =
  fmap T.pack $ mapM (\c -> elements [toLower c, toUpper c]) (T.unpack raw)

genUnknownEffort :: Gen T.Text
genUnknownEffort =
  suchThat (T.pack <$> listOf1 (elements (['a'..'z'] ++ " -_"))) $ \token ->
    let stripped = T.toLower (T.strip token)
    in not (T.null stripped) && stripped `notElem` supportedEffortLevels

genUsage :: Gen TokenUsage
genUsage = do
  prompt <- choose (0, 40)
  completion <- choose (0, 40)
  totalTokens <- choose (0, 80)
  cached <- choose (0, 20)
  cost <- oneof [pure Nothing, Just . fromIntegral <$> choose (0 :: Int, 30)]
  pure (TokenUsage prompt completion totalTokens cached cost)

combineCosts :: Maybe Double -> Maybe Double -> Maybe Double
combineCosts Nothing Nothing = Nothing
combineCosts (Just a) Nothing = Just a
combineCosts Nothing (Just b) = Just b
combineCosts (Just a) (Just b) = Just (a + b)

genAction :: Gen RuleAction
genAction = elements [RuleAllow, RuleAsk, RuleDeny]

genTool :: Gen T.Text
genTool = T.pack <$> listOf1 (elements ['a'..'z'])

genGlob :: Gen T.Text
genGlob = do
  stem <- listOf1 (elements ['a'..'z'])
  pure (T.pack stem <> "/*")

genRule :: Gen PermissionRule
genRule = PermissionRule <$> genAction <*> oneof [pure Nothing, Just <$> genTool] <*> oneof [pure Nothing, Just <$> genGlob]

decisionFor :: RuleAction -> T.Text -> PermissionDecision
decisionFor action tool = case action of
  RuleAllow -> PermAllow
  RuleAsk -> PermAsk ("Approval required by rule for " <> tool)
  RuleDeny -> PermDeny ("Denied by permission rule for " <> tool)

protectedSegments :: [String]
protectedSegments = [".git", ".claude", ".agents", ".agent"]

lookalikeSegments :: [String]
lookalikeSegments =
  [ ".gitignore"
  , ".gitattributes"
  , ".gitmodules"
  , ".github"
  , ".gitlab-ci.yml"
  , ".git.bak"
  , "git"
  , "claude"
  , ".claudex"
  , "agents"
  , "agent"
  , "a.agent"
  , "not.git"
  , "..git"
  , ".agentx"
  , "foo.git"
  , "."
  , "src"
  , "README.md"
  ]

genPathSegments :: Gen [String]
genPathSegments =
  resize 5 $ listOf1 $ frequency
    [ (1, elements protectedSegments)
    , (3, elements lookalikeSegments)
    ]

genDirs :: Gen [FilePath]
genDirs = resize 4 $ listOf $ elements ["", "/tmp/a", "/tmp/b", "docs", "/srv/shared"]

genFinite :: Gen Double
genFinite = suchThat arbitrary (\d -> not (isNaN d) && not (isInfinite d))

genFlagBudget :: Gen Double
genFlagBudget = oneof [genFinite, pure (1 / 0), pure ((-1) / 0)]

genNonNegBudget :: Gen Double
genNonNegBudget = suchThat genFinite (>= 0)

genRejectedBudget :: Gen Double
genRejectedBudget = oneof
  [ suchThat (negate . abs <$> genFinite) (< 0)
  , pure (1 / 0)
  , pure ((-1) / 0)
  , pure (0 / 0)
  ]

genMaybeBudget :: Gen (Maybe Double)
genMaybeBudget = oneof [pure Nothing, Just <$> arbitrary]

genHay :: Gen T.Text
genHay = do
  n <- choose (0, 12)
  T.pack <$> vectorOf n (elements ['a'..'c'])

genRawNeedle :: Gen T.Text
genRawNeedle = do
  n <- choose (0, 3)
  T.pack <$> vectorOf n (elements ['a'..'c'])

genNonOverlapNeedle :: Gen T.Text
genNonOverlapNeedle = suchThat (do
  n <- choose (1, 3)
  T.pack <$> vectorOf n (elements ['a'..'c'])) (not . overlapsItself)

overlapsItself :: T.Text -> Bool
overlapsItself needle =
  any (\i -> T.drop i needle `T.isPrefixOf` needle) [1 .. T.length needle - 1]

genPathName :: Gen String
genPathName = listOf1 (elements ['a'..'z'])
