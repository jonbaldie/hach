{-# LANGUAGE OverloadedStrings #-}

module Agent.SkillsSpec (spec) where

import Agent.Skills
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "Agent.Skills" $ do
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
        let globalSkill = Skill
              { skillName        = "review"
              , skillDescription = "Global review"
              , skillContent     = "Global instructions"
              , skillPath        = "/home/.agents/skills/review/SKILL.md"
              , skillSource      = SkillGlobal
              }
            workspaceSkill = Skill
              { skillName        = "review"
              , skillDescription = "Project review"
              , skillContent     = "Project instructions"
              , skillPath        = "/repo/.agents/skills/review/SKILL.md"
              , skillSource      = SkillWorkspace
              }
            catalog = mergeSkills [globalSkill] [workspaceSkill]
        Map.lookup "review" catalog `shouldBe` Just workspaceSkill

      it "includes distinct skills from both global and workspace" $ do
        let s1 = Skill "s1" "d1" "c1" "/p1" SkillGlobal
            s2 = Skill "s2" "d2" "c2" "/p2" SkillWorkspace
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
      let skillA = Skill "to-spec" "Spec gen" "Instructions for spec" "/p" SkillGlobal
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

      it "leaves input untouched if slash token is not in catalog" $ do
        let input = "/unknown do something"
            (cleaned, skills) = parseSkillInvocations catalog input
        skills `shouldBe` []
        cleaned `shouldBe` "/unknown do something"

    describe "injectSkillsIntoPrompt" $ do
      it "returns original prompt when skills list is empty" $ do
        injectSkillsIntoPrompt [] "Hello world" `shouldBe` "Hello world"

      it "wraps skill instructions into context tags prepended to prompt" $ do
        let skillA = Skill "to-spec" "desc" "Follow steps 1-3" "/p" SkillGlobal
            res = injectSkillsIntoPrompt [skillA] "Build auth"
        res `shouldBe` "<skill name=\"to-spec\">\nFollow steps 1-3\n</skill>\n\nBuild auth"
