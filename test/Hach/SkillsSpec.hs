{-# LANGUAGE OverloadedStrings #-}

module Hach.SkillsSpec (spec) where

import Hach.Skills
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "Hach.Skills" $ do
    describe "parseSkillFile" $ do
      it "parses valid frontmatter and markdown body" $ do
        let content =
              "---\n\
              \name: to-spec\n\
              \description: Generate a spec from conversation\n\
              \disable-model-invocation: true\n\
              \---\n\
              \# Process\n\
              \1. Synthesize context into spec."
        case parseSkillFile SkillGlobal "/path/to/SKILL.md" content of
          Left err -> expectationFailure ("Failed to parse: " ++ err)
          Right skill -> do
            skillName skill `shouldBe` "to-spec"
            skillDescription skill `shouldBe` "Generate a spec from conversation"
            skillContent skill `shouldBe` "# Process\n1. Synthesize context into spec."
            skillSource skill `shouldBe` SkillGlobal
            skillPath skill `shouldBe` "/path/to/SKILL.md"

      it "strips quotes from frontmatter values" $ do
        let content =
              "---\n\
              \name: \"diagnosing-bugs\"\n\
              \description: 'Systematic debugging workflow'\n\
              \---\n\
              \Follow Phase 1 through 5."
        case parseSkillFile SkillWorkspace "/ws/SKILL.md" content of
          Left err -> expectationFailure ("Failed to parse: " ++ err)
          Right skill -> do
            skillName skill `shouldBe` "diagnosing-bugs"
            skillDescription skill `shouldBe` "Systematic debugging workflow"
            skillSource skill `shouldBe` SkillWorkspace

      it "fails when frontmatter is missing name" $ do
        let content =
              "---\n\
              \description: No name here\n\
              \---\n\
              \Some content"
        parseSkillFile SkillGlobal "/path" content `shouldSatisfy` \case
          Left _  -> True
          Right _ -> False

      it "fails when frontmatter closing delimiter is missing" $ do
        let content =
              "---\n\
              \name: broken\n\
              \No closing delimiter"
        parseSkillFile SkillGlobal "/path" content `shouldSatisfy` \case
          Left _  -> True
          Right _ -> False

    describe "mergeSkills (Precedence)" $ do
      it "overrides global skill with workspace skill of the same name" $ do
        let globalSkill = mkSkill "review" "Global review" "Global instructions" "/home/.agents/skills/review/SKILL.md" SkillGlobal
            workspaceSkill = mkSkill "review" "Project review" "Project instructions" "/repo/.agents/skills/review/SKILL.md" SkillWorkspace
            catalog = mergeSkills [globalSkill] [workspaceSkill]
        Map.lookup "review" catalog `shouldBe` Just workspaceSkill

      it "includes distinct skills from both global and workspace" $ do
        let s1 = mkSkill "s1" "d1" "c1" "/p1" SkillGlobal
            s2 = mkSkill "s2" "d2" "c2" "/p2" SkillWorkspace
            catalog = mergeSkills [s1] [s2]
        Map.lookup "s1" catalog `shouldBe` Just s1
        Map.lookup "s2" catalog `shouldBe` Just s2

    describe "discoverSkillsFromDir" $ do
      let testDir = "dist-newstyle/test-sandbox-skills"
      around_ (\action -> do
        createDirectoryIfMissing True testDir
        action
        removeDirectoryRecursive testDir) $ do
        it "discovers skills in subdirectories with valid SKILL.md" $ do
          let skillDir = testDir </> "my-skill"
          createDirectoryIfMissing True skillDir
          TIO.writeFile (skillDir </> "SKILL.md")
            "---\nname: my-skill\ndescription: Discovered skill\n---\nBody here"
          skills <- discoverSkillsFromDir SkillWorkspace testDir
          case skills of
            [s] -> do
              skillName s `shouldBe` "my-skill"
              skillDescription s `shouldBe` "Discovered skill"
              skillContent s `shouldBe` "Body here"
              skillSource s `shouldBe` SkillWorkspace
            _ -> expectationFailure ("Expected 1 skill, got " ++ show (length skills))

    describe "parseSkillInvocations" $ do
      let skillA = mkSkill "to-spec" "Spec gen" "Instructions for spec" "/p" SkillGlobal
          catalog = Map.fromList [("to-spec", skillA)]

      it "extracts a leading skill invocation and returns cleaned prompt" $ do
        let input = "/to-spec create a spec for auth"
            (cleaned, skills) = parseSkillInvocations catalog input
        skills `shouldBe` [skillA]
        cleaned `shouldBe` "create a spec for auth"

      it "extracts skill invocation without extra prompt" $ do
        let input = "/to-spec"
            (cleaned, skills) = parseSkillInvocations catalog input
        skills `shouldBe` [skillA]
        cleaned `shouldBe` ""

      it "preserves multiline formatting when extracting skill invocation" $ do
        let input = "/to-spec Line 1\nLine 2\nLine 3"
            (cleaned, skills) = parseSkillInvocations catalog input
        skills `shouldBe` [skillA]
        cleaned `shouldBe` "Line 1\nLine 2\nLine 3"

      it "extracts skill invocation located anywhere in prompt" $ do
        let input = "Please use /to-spec to build this feature"
            (cleaned, skills) = parseSkillInvocations catalog input
        skills `shouldBe` [skillA]
        cleaned `shouldBe` "Please use to build this feature"

      it "deduplicates skill invocations when the same skill is mentioned multiple times" $ do
        let input = "Please run /to-spec and also /to-spec"
            (cleaned, skills) = parseSkillInvocations catalog input
        skills `shouldBe` [skillA]
        cleaned `shouldBe` "Please run and also"

      it "leaves input untouched if slash token is not in catalog" $ do
        let input = "/unknown do something"
            (cleaned, skills) = parseSkillInvocations catalog input
        skills `shouldBe` []
        cleaned `shouldBe` "/unknown do something"

    describe "injectSkillsIntoPrompt" $ do
      it "returns original prompt when skills list is empty" $ do
        injectSkillsIntoPrompt [] "Hello world" `shouldBe` "Hello world"

      it "wraps skill instructions into context tags prepended to prompt" $ do
        let skillA = mkSkill "to-spec" "desc" "Follow steps 1-3" "/p" SkillGlobal
            res = injectSkillsIntoPrompt [skillA] "Build auth"
        res `shouldBe` "<skill name=\"to-spec\">\nFollow steps 1-3\n</skill>\n\nBuild auth"

    describe "skillInvocationCompletion" $ do
      let goal  = mkSkill "goal"  "Goal skill"  "body" "/p" SkillGlobal
          goals = mkSkill "goals" "Goals skill" "body" "/p" SkillGlobal
          go    = mkSkill "go"    "Go skill"    "body" "/p" SkillGlobal
          toSpec = mkSkill "to-spec" "Spec skill" "body" "/p" SkillGlobal
          catalog = Map.fromList [("goal", goal), ("goals", goals), ("go", go), ("to-spec", toSpec)]

      it "completes a partial slash-command to the matching skill name" $ do
        skillInvocationCompletion catalog "/goa" `shouldBe` Just "l"

      it "completes when the partial uniquely extends to a longer skill" $ do
        skillInvocationCompletion catalog "/to-sp" `shouldBe` Just "ec"

      it "completes to the lexicographically smallest match when ambiguous" $ do
        skillInvocationCompletion (Map.fromList [("goal", goal), ("goals", goals)]) "/go"
          `shouldBe` Just "al"

      it "extends an exact match to a longer skill when one shares the prefix" $ do
        -- "/go" is itself a skill, but "goal" extends it, so the completion
        -- is still offered rather than suppressed.
        skillInvocationCompletion catalog "/go" `shouldBe` Just "al"
        skillInvocationCompletion catalog "/goal" `shouldBe` Just "s"

      it "offers no completion when the partial exactly matches the only matching skill" $ do
        let onlyGo = Map.fromList [("go", go)]
            onlyGoal = Map.fromList [("goal", goal)]
        skillInvocationCompletion onlyGo "/go" `shouldBe` Nothing
        skillInvocationCompletion onlyGoal "/goal" `shouldBe` Nothing

      it "offers no completion when the partial matches no skill" $ do
        skillInvocationCompletion catalog "/xyz" `shouldBe` Nothing

      it "offers no completion for input that is not a slash-command" $ do
        skillInvocationCompletion catalog "goa" `shouldBe` Nothing
        skillInvocationCompletion catalog "hello world" `shouldBe` Nothing

      it "offers no completion when the trailing word is finished (trailing space)" $ do
        skillInvocationCompletion catalog "/goa " `shouldBe` Nothing
        skillInvocationCompletion catalog "/goal " `shouldBe` Nothing

      it "offers no completion for a bare slash with no following text" $ do
        skillInvocationCompletion catalog "/" `shouldBe` Nothing
        skillInvocationCompletion catalog "" `shouldBe` Nothing

      it "completes only the trailing word, preserving leading prompt text" $ do
        skillInvocationCompletion catalog "please run /goa" `shouldBe` Just "l"

      it "offers no completion once the slash-command is followed by arguments" $ do
        -- the trailing word is now the argument, not the skill token
        skillInvocationCompletion catalog "/to-sp create a spec" `shouldBe` Nothing
        skillInvocationCompletion catalog "/goal build" `shouldBe` Nothing

    describe "extended frontmatter fields" $ do
      it "parses all extended frontmatter metadata" $ do
        let content =
              "---\n\
              \name: refactor\n\
              \description: Refactor code\n\
              \allowed-tools: bash, edit, grep\n\
              \user-invocable: false\n\
              \disable-model-invocation: true\n\
              \context: fork\n\
              \agent: refactoring-specialist\n\
              \paths: src/**/*.hs, test/**/*.hs\n\
              \---\n\
              \Refactor body here."
        case parseSkillFile SkillWorkspace "/path/to/SKILL.md" content of
          Left err -> expectationFailure ("Failed to parse: " ++ err)
          Right s -> do
            skillName s `shouldBe` "refactor"
            skillDescription s `shouldBe` "Refactor code"
            skillAllowedTools s `shouldBe` ["bash", "edit", "grep"]
            skillUserInvocable s `shouldBe` False
            skillDisableModelInvocation s `shouldBe` True
            skillContextFork s `shouldBe` True
            skillAgent s `shouldBe` Just "refactoring-specialist"
            skillPaths s `shouldBe` ["src/**/*.hs", "test/**/*.hs"]

    describe "substituteArguments" $ do
      it "replaces $ARGUMENTS with supplied argument text" $ do
        let template = "Review the following files: $ARGUMENTS please."
        substituteArguments "foo.hs bar.hs" template `shouldBe` "Review the following files: foo.hs bar.hs please."

      it "leaves content unchanged when $ARGUMENTS is absent" $ do
        substituteArguments "extra" "Static instructions" `shouldBe` "Static instructions"

    describe "injectDynamicContext" $ do
      let testDir = "dist-newstyle/test-sandbox-dynamic-context"
      around_ (\action -> do
        createDirectoryIfMissing True testDir
        action
        removeDirectoryRecursive testDir) $ do
        it "expands {{file:path}} placeholders with file contents" $ do
          TIO.writeFile (testDir </> "sample.txt") "sample content 123"
          let raw = "Intro: {{file:sample.txt}} - done."
          res <- injectDynamicContext testDir raw
          res `shouldBe` "Intro: sample content 123 - done.\n"

        it "executes !command lines and replaces with stdout" $ do
          let raw = "Command output:\n!echo hello from shell\nEnd."
          res <- injectDynamicContext testDir raw
          res `shouldBe` "Command output:\nhello from shell\nEnd.\n"

