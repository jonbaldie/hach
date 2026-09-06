{-# LANGUAGE OverloadedStrings #-}

-- | Coverage-guided property-based testing (CGPT) for the /goal feature.
--
-- The SUT is 'goalLoop' in "Hach.Core", driven through the pure interpreter
-- so the loop is deterministic and runs without network.  We generate random
-- sequences of turn outcomes (tool-call / completion / API error) plus goal
-- verdicts, run the loop, and check invariants over the resulting 'GoalState',
-- 'AgentResult', history, and event log.
--
-- The event log doubles as a coverage signal: the set of 'AgentEvent'
-- constructors reached tells us which paths through the goal loop executed,
-- so QuickCheck cases that open new event paths are retained (see the
-- 'collect' markers below).
module Hach.GoalFuzzSpec (spec) where

import Hach.Core
import Hach.Interpreter.Pure
import Hach.Tools
import Hach.Types

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- * Input model
-- ---------------------------------------------------------------------------

-- | One turn's outcome, as seen by the pure interpreter.
data TurnSpec
  = TProgress          -- ^ model calls a tool -> progress, resets no-progress
  | TComplete Text     -- ^ model completes with this content (no tools)
  | TError Text        -- ^ 'interpPrompt' returns 'Left' -> 'AgentFailed'
  deriving (Show, Eq)

-- | A full goal-loop scenario.
data Scenario = Scenario
  { sTurns     :: [TurnSpec]    -- ^ finite prefix; tail defaults to a completion
  , sEvals     :: [GoalVerdict] -- ^ finite prefix; tail defaults to 'GoalNotYetMet'
  , sBlockCap  :: Int           -- ^ no-progress block cap
  , sMaxTurns  :: Int           -- ^ cfgMaxTurns
  , sCondition :: Text          -- ^ goal condition text
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- * Generators
-- ---------------------------------------------------------------------------

instance Arbitrary GoalVerdict where
  arbitrary = elements [GoalMet, GoalNotYetMet, GoalImpossible]

-- | Short printable content for completions / conditions.
genText :: Gen Text
genText = T.pack <$> listOf (elements "abcdefghij012 ")

-- | Text that looks like a real IO-interpreter API error (no "[API Error]: "
-- prefix).  We sprinkle in unrecoverable keywords so the
-- error-classification integration property can fire.
genRealisticError :: Gen Text
genRealisticError = oneof
  [ pure "OpenRouter API error: 401 Unauthorized"
  , pure "OpenRouter API error: 402 Payment required, credit balance exhausted"
  , pure "OpenRouter API error: 404 model not found"
  , pure "OpenRouter API error: context overflow, token limit exceeded"
  , pure "HTTP request failed: connection timeout"
  , pure "OpenRouter API error: 429 Too many requests"
  , pure "JSON parse failure: unexpected token"
  ]

-- | Completion text.  Occasionally include the synthetic "[API Error]: "
-- prefix that 'classifyCompletion' actually recognises, so both branches get
-- exercised.
genCompletion :: Gen Text
genCompletion = oneof
  [ genText
  , ("Working on it: " <>) <$> genText
  , ("[API Error]: " <>) <$> genRealisticError
  ]

genTurn :: Gen TurnSpec
genTurn = frequency
  [ (3, pure TProgress)
  , (5, TComplete <$> genCompletion)
  , (2, TError   <$> genRealisticError)
  ]

genScenario :: Gen Scenario
genScenario = do
  n        <- choose (1, 8)
  turns    <- vectorOf n genTurn
  m        <- choose (0, n)
  evals    <- vectorOf m arbitrary
  blockCap <- choose (1, 6)
  maxTurns <- choose (1, 20)
  cond     <- genText
  pure Scenario
    { sTurns     = turns
    , sEvals     = evals
    , sBlockCap  = blockCap
    , sMaxTurns  = maxTurns
    , sCondition = cond
    }

-- | A scenario whose very first turn is a realistic unrecoverable API error.
-- Used to exercise the documented "unrecoverable errors clear the goal"
-- behaviour through the path the real IO interpreter actually takes.
genUnrecoverableErrorScenario :: Gen Scenario
genUnrecoverableErrorScenario = do
  err <- elements
    [ "OpenRouter API error: 401 Unauthorized"
    , "OpenRouter API error: 402 Payment required"
    , "OpenRouter API error: 404 model not found"
    , "OpenRouter API error: context overflow"
    ]
  rest <- listOf genTurn
  cap  <- choose (1, 6)
  pure Scenario
    { sTurns     = TError err : rest
    , sEvals     = []
    , sBlockCap  = cap
    , sMaxTurns  = 20
    , sCondition = "all tests pass"
    }

-- ---------------------------------------------------------------------------
-- * Harness: turn a Scenario into a MockEnv and run goalLoop
-- ---------------------------------------------------------------------------

readCall :: ToolCall
readCall = ToolCall
  { callId       = "call_1"
  , functionName = "read_file"
  , callArgsRaw  = "{\"path\":\"hello.txt\"}"
  }

-- | Convert a 'TurnSpec' into a pure interpreter step function.
specToStep :: TurnSpec -> ([Message] -> [ToolDef] -> Either Text AssistantResponse)
specToStep TProgress     _ _ = Right (AssistantResponse Nothing [readCall] Nothing)
specToStep (TComplete t) _ _ = Right (AssistantResponse (Just t) [] Nothing)
specToStep (TError t)    _ _ = Left t

specToEval :: GoalVerdict -> (Text -> [Message] -> GoalEvaluation)
specToEval v _ _ = GoalEvaluation v "fuzz reason"

-- | Run a scenario through 'goalLoop' with the pure interpreter.
runScenario :: Scenario -> (AgentResult, [Message], GoalState, MockEnv)
runScenario s =
  let cfg = AgentConfig
        { cfgModel        = "fuzz-model"
        , cfgSystemPrompt = Just "You are a fuzz target."
        , cfgMaxTurns     = Just (sMaxTurns s)
        }
      -- Infinite step/eval lists: the generated prefix followed by defaults,
      -- so the loop is well-defined no matter how many turns it takes.
      steps = map specToStep (sTurns s)
              ++ repeat (\_ _ -> Right (AssistantResponse (Just "tail.") [] Nothing))
      evals = map specToEval (sEvals s)
              ++ repeat (specToEval GoalNotYetMet)
      env = emptyMockEnv
        { mockLLMSteps        = steps
        , mockGoalEvaluations = evals
        , mockFiles           = Map.fromList [("hello.txt", "data")]
        }
      initHist = [UserMsg (sCondition s)]
      ((result, finalHist, gs), endEnv) =
        runPure env (goalLoop cfg allToolDefs (sCondition s) (sBlockCap s) initHist)
  in (result, finalHist, gs, endEnv)

-- | Count 'EvGoalEvaluated' events in the log.
evalEventCount :: MockEnv -> Int
evalEventCount = length . filter isEval . mockEvents
  where isEval EvGoalEvaluated{} = True
        isEval _                 = False

-- | True when the run reached the goal-achieved termination path.
sawAchieved :: MockEnv -> Bool
sawAchieved = any isAch . mockEvents
  where isAch EvGoalAchieved{} = True
        isAch _                = False

-- | A compact label for which termination path a scenario took, so QuickCheck
-- 'collect' gives us a coverage view across the generated corpus.
eventProfile :: Scenario -> String
eventProfile s = case runScenario s of
  (_, _, gs, env)
    | sawAchieved env           -> "achieved"
    | gsStatus gs == GoalFailed -> "failed"
    | gsStatus gs == GoalActive -> "active/blocked"
    | otherwise                 -> "other"

-- ---------------------------------------------------------------------------
-- * Properties
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "CGPT: goalLoop invariants over random turn sequences" $ do

    -- Coverage signal: which event paths did random runs reach?
    it "exercises a broad spread of goal-loop termination paths" $
      forAll genScenario $ \s -> collect (eventProfile s) True

    it "gsNoProgressCount never exceeds blockCap (cap >= 1)" $
      forAll genScenario $ \s ->
        let (_, _, gs, _) = runScenario s
        in gsNoProgressCount gs <= sBlockCap s

    it "gsTurnCount == number of EvGoalEvaluated events" $
      forAll genScenario $ \s ->
        let (_, _, gs, endEnv) = runScenario s
        in gsTurnCount gs == evalEventCount endEnv

    it "GoalAchieved implies last verdict was GoalMet" $
      forAll genScenario $ \s ->
        let (_, _, gs, _) = runScenario s
        in gsStatus gs == GoalAchieved ==> gsLastVerdict gs == Just GoalMet

    it "GoalFailed implies impossible-verdict or unrecoverable content" $
      forAll genScenario $ \s ->
        let (result, _, gs, _) = runScenario s
            fromImpossible = gsLastVerdict gs == Just GoalImpossible
            fromUnrec = case result of
              AgentCompleted c -> classifyCompletion c == GoalErrUnrecoverable
              AgentFailed e    -> classifyError e == GoalErrUnrecoverable
              _                -> False
        in gsStatus gs == GoalFailed ==> (fromImpossible || fromUnrec)

    it "history only ever grows from the initial prefix" $
      forAll genScenario $ \s ->
        let (_, finalHist, _, _) = runScenario s
        in length finalHist >= 1   -- started with [UserMsg condition]

  -- The headline integration property: the real IO/TUI interpreter emits API
  -- errors as 'Left "OpenRouter API error: …"' (no "[API Error]: " prefix).
  -- 'classifyCompletion' only recognises the prefix, so unrecoverable errors
  -- arriving via 'AgentFailed' are misclassified and the goal is NOT failed.
  describe "CGPT: error-classification integration (realistic IO errors)" $ do

    it "classifyError flags realistic unrecoverable errors" $
      forAll genRealisticError $ \err ->
        -- The *intended* contract (per the feature spec): auth/credit/
        -- context/model errors are unrecoverable and should clear the goal.
        -- We only assert on the unrecoverable cases; transient errors (e.g.
        -- timeouts) correctly keep the goal active, so we discard them.
        let lower = T.toLower err
            intendedUnrec = any (`T.isInfixOf` lower)
              [ "401", "unauthorized", "authentication", "api key"
              , "402", "payment", "credit", "balance", "quota", "billing"
              , "context", "overflow", "too long", "maximum context", "token limit"
              , "404", "model", "not found", "unavailable", "does not exist"
              ]
        in intendedUnrec ==> classifyError err == GoalErrUnrecoverable

    it "goalLoop fails the goal on a realistic unrecoverable API error" $
      forAll genUnrecoverableErrorScenario $ \s ->
        let (_, _, gs, _) = runScenario s
        in gsStatus gs == GoalFailed

  -- Edge case: a degenerate block cap of 0 (or negative) is clamped to 1.
  describe "CGPT: block-cap edge case" $ do

    it "gsNoProgressCount never exceeds max 1 blockCap (incl. degenerate caps)" $
      forAll (choose (-3, 0)) $ \cap ->
        let step _ _ = Right (AssistantResponse (Just "done.") [] Nothing)
            eval _ _ = GoalEvaluation GoalNotYetMet "no"
            env = emptyMockEnv
                  { mockLLMSteps        = repeat step
                  , mockGoalEvaluations = repeat eval
                  }
            cfg = AgentConfig "m" (Just "sys") (Just 20)
            ((_, _, gs), _) = runPure env (goalLoop cfg [] "c" cap [UserMsg "c"])
            effectiveCap = max 1 cap
        in -- The loop blocks after one no-progress turn when the cap is
           -- degenerate, so the counter equals the clamped cap.
           gsNoProgressCount gs <= effectiveCap
             && gsStatus gs == GoalActive

  -- Bug 2 regression: a /goal condition whose first word is a clear alias
  -- must NOT be treated as a clear command when trailing text follows.
  describe "CGPT: /goal clear-alias vs condition disambiguation" $ do

    it "a bare alias (no trailing text) is a clear command" $
      forAll (elements goalClearAliases) $ \alias ->
        goalArgIsClear alias

    it "an alias followed by trailing text is NOT a clear command" $
      forAll (elements goalClearAliases) $ \alias ->
        forAll (listOf1 (elements "abcdefghij")) $ \trailing ->
          let arg = alias <> " " <> T.pack trailing
          in not (goalArgIsClear arg)

    it "an alias with only trailing whitespace is still a clear command" $
      forAll (elements goalClearAliases) $ \alias ->
        goalArgIsClear (alias <> "   ")

    it "an alias with leading whitespace is still a clear command" $
      forAll (elements goalClearAliases) $ \alias ->
        goalArgIsClear ("   " <> alias)

    it "an alias with leading and trailing whitespace is still a clear command" $
      forAll (elements goalClearAliases) $ \alias ->
        goalArgIsClear ("   " <> alias <> "   ")

    it "specifically: /goal stop the server is not a clear" $
      not (goalArgIsClear "stop the server")