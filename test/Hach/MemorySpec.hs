{-# LANGUAGE OverloadedStrings #-}

module Hach.MemorySpec (spec) where

import Hach.Memory
import Hach.Permissions (matchGlob)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removeDirectoryRecursive)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Test.Hspec

spec :: Spec
spec = describe "Hach.Memory" $ do
  let testDir = "dist-newstyle/test-memory"
  around_ (\action -> do
    createDirectoryIfMissing True testDir
    action
    removeDirectoryRecursive testDir) $ do

    it "resolves @import directives recursively up to depth 4" $ do
      TIO.writeFile (testDir </> "base.md") "Base instructions\n@import sub.md\n"
      TIO.writeFile (testDir </> "sub.md") "Sub instructions\n@import leaf.md\n"
      TIO.writeFile (testDir </> "leaf.md") "Leaf instructions\n"
      resolved <- resolveMemoryImports testDir 4 (testDir </> "base.md")
      T.unpack resolved `shouldContain` "Base instructions"
      T.unpack resolved `shouldContain` "Sub instructions"
      T.unpack resolved `shouldContain` "Leaf instructions"

    it "stops resolving @import when max depth is exceeded" $ do
      TIO.writeFile (testDir </> "loop1.md") "@import loop2.md\n"
      TIO.writeFile (testDir </> "loop2.md") "@import loop1.md\n"
      resolved <- resolveMemoryImports testDir 2 (testDir </> "loop1.md")
      -- Should terminate without crashing or hanging
      resolved `shouldSatisfy` (not . T.null)

  describe "Hierarchical memory" $ do
    around_ (\action -> do
      createDirectoryIfMissing True testDir
      action
      removeDirectoryRecursive testDir) $ do

      it "loads AGENTS.md when it is the only memory file" $ do
        TIO.writeFile (testDir </> "AGENTS.md") "Agents memory"
        mem <- loadHierarchicalMemory testDir testDir
        mem `shouldBe` ["Agents memory\n"]

      it "prefers AGENTS.md over AGENT.md and CLAUDE.md when all exist" $ do
        TIO.writeFile (testDir </> "AGENTS.md") "Agents memory"
        TIO.writeFile (testDir </> "AGENT.md") "Agent memory"
        TIO.writeFile (testDir </> "CLAUDE.md") "Claude memory"
        mem <- loadHierarchicalMemory testDir testDir
        mem `shouldBe` ["Agents memory\n"]

  describe "Global memory" $ do
    let homeDir = "dist-newstyle/test-memory-home"
    around_ (\action -> do
      origHome <- lookupEnv "HOME"
      createDirectoryIfMissing True homeDir
      setEnv "HOME" homeDir
      action
      case origHome of
        Just h  -> setEnv "HOME" h
        Nothing -> unsetEnv "HOME"
      removeDirectoryRecursive homeDir) $ do

      it "loads global memory from ~/.agents/AGENT.md" $ do
        createDirectoryIfMissing True (homeDir </> ".agents")
        TIO.writeFile (homeDir </> ".agents" </> "AGENT.md") "Global agent memory"
        mem <- loadFullMemory testDir []
        T.unpack mem `shouldContain` "Global agent memory"

      it "loads global memory from ~/.agents/AGENTS.md" $ do
        createDirectoryIfMissing True (homeDir </> ".agents")
        TIO.writeFile (homeDir </> ".agents" </> "AGENTS.md") "Global agents memory"
        mem <- loadFullMemory testDir []
        T.unpack mem `shouldContain` "Global agents memory"

      it "loads global memory from ~/.claude/CLAUDE.md" $ do
        createDirectoryIfMissing True (homeDir </> ".claude")
        TIO.writeFile (homeDir </> ".claude" </> "CLAUDE.md") "Global claude memory"
        mem <- loadFullMemory testDir []
        T.unpack mem `shouldContain` "Global claude memory"

      it "prefers ~/.claude/CLAUDE.md over ~/.agents/ memory files" $ do
        createDirectoryIfMissing True (homeDir </> ".claude")
        createDirectoryIfMissing True (homeDir </> ".agents")
        TIO.writeFile (homeDir </> ".claude" </> "CLAUDE.md") "Global claude memory"
        TIO.writeFile (homeDir </> ".agents" </> "AGENT.md") "Global agent memory"
        mem <- loadFullMemory testDir []
        T.unpack mem `shouldContain` "Global claude memory"
        T.unpack mem `shouldNotContain` "Global agent memory"

  describe "Rule matching" $ do
    it "matches rules by glob against active file paths" $ do
      let ruleContent = "---\npaths: src/**/*.hs, test/**/*.hs\n---\nUse GHC 2021\n"
      case parseRuleFile (testDir </> "ghc.md") ruleContent of
        Nothing -> expectationFailure "Failed to parse rule file"
        Just rule -> do
          ruleMatchesFiles rule ["src/Hach/Core.hs"] `shouldBe` True
          ruleMatchesFiles rule ["README.md"] `shouldBe` False

  describe "Workspace instructions file conventions" $ do
    it "discovers AGENTS.md, then AGENT.md, then CLAUDE.md" $
      instructionsFileNames `shouldBe` ["AGENTS.md", "AGENT.md", "CLAUDE.md"]

    it "scaffolds CLAUDE.md" $
      scaffoldInstructionsFileName `shouldBe` "CLAUDE.md"

    it "scaffolds a file that discovery finds" $
      instructionsFileNames `shouldContain` [scaffoldInstructionsFileName]

  describe "initializeWorkspaceInstructionsFile" $ do
    let initDir = "dist-newstyle/test-memory-init"
    around_ (\action -> do
      createDirectoryIfMissing True initDir
      action
      removeDirectoryRecursive initDir) $ do

      it "writes the starter template to CLAUDE.md" $ do
        result <- initializeWorkspaceInstructionsFile initDir
        result `shouldBe` ProjectInitialized
        TIO.readFile (initDir </> "CLAUDE.md") `shouldReturn` instructionsTemplate

      it "starts the template with a Markdown heading" $
        instructionsTemplate `shouldSatisfy` T.isPrefixOf "# "

      it "leaves an existing CLAUDE.md untouched" $ do
        let existing = "# Keep this file\n"
        TIO.writeFile (initDir </> "CLAUDE.md") existing
        result <- initializeWorkspaceInstructionsFile initDir
        result `shouldBe` ProjectAlreadyPresent
        TIO.readFile (initDir </> "CLAUDE.md") `shouldReturn` existing

      it "reports a failure when CLAUDE.md is a directory" $ do
        createDirectoryIfMissing True (initDir </> "CLAUDE.md")
        result <- initializeWorkspaceInstructionsFile initDir
        result `shouldBe` ProjectInitializationFailed
          ("Could not create " <> T.pack (initDir </> "CLAUDE.md") <> ": path is a directory.")

      it "reports an actionable error when the workspace is not a directory" $ do
        let blockedWorkspace = initDir </> "not-a-directory"
        writeFile blockedWorkspace "blocked"
        result <- initializeWorkspaceInstructionsFile blockedWorkspace
        result `shouldSatisfy` \case
          ProjectInitializationFailed err ->
            T.pack (blockedWorkspace </> "CLAUDE.md") `T.isInfixOf` err
          _ -> False

      it "creates a file that loadProjectInstructionsFile then discovers" $ do
        _ <- initializeWorkspaceInstructionsFile initDir
        loadProjectInstructionsFile initDir
          `shouldReturn` Just ("CLAUDE.md", instructionsTemplate)

  describe "loadProjectInstructions" $ do
    let testSandbox = "dist-newstyle/test-memory-instructions"
    around_ (\action -> do
      createDirectoryIfMissing True testSandbox
      action
      removeDirectoryRecursive testSandbox) $ do
      it "returns Nothing when neither AGENT.md nor CLAUDE.md exists" $ do
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Nothing

      it "loads AGENT.md when it exists" $ do
        TIO.writeFile (testSandbox </> "AGENT.md") "Agent rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Agent rules"

      it "loads CLAUDE.md when AGENT.md does not exist" $ do
        TIO.writeFile (testSandbox </> "CLAUDE.md") "Claude rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Claude rules"

      it "prefers AGENT.md over CLAUDE.md when both exist" $ do
        TIO.writeFile (testSandbox </> "AGENT.md") "Agent rules"
        TIO.writeFile (testSandbox </> "CLAUDE.md") "Claude rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Agent rules"

      it "loads AGENTS.md when it is the only instructions file" $ do
        TIO.writeFile (testSandbox </> "AGENTS.md") "Agents rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Agents rules"

      it "prefers AGENTS.md over AGENT.md and CLAUDE.md when all exist" $ do
        TIO.writeFile (testSandbox </> "AGENTS.md") "Agents rules"
        TIO.writeFile (testSandbox </> "AGENT.md") "Agent rules"
        TIO.writeFile (testSandbox </> "CLAUDE.md") "Claude rules"
        res <- loadProjectInstructions testSandbox
        res `shouldBe` Just "Agents rules"

      it "names the file the instructions came from" $ do
        TIO.writeFile (testSandbox </> "AGENT.md") "Agent rules"
        loadProjectInstructionsFile testSandbox `shouldReturn` Just ("AGENT.md", "Agent rules")
