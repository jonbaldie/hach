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
