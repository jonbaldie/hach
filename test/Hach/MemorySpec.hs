{-# LANGUAGE OverloadedStrings #-}

module Hach.MemorySpec (spec) where

import Hach.Memory
import Hach.Permissions (matchGlob)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
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

  describe "Rule matching" $ do
    it "matches rules by glob against active file paths" $ do
      let ruleContent = "---\npaths: src/**/*.hs, test/**/*.hs\n---\nUse GHC 2021\n"
      case parseRuleFile (testDir </> "ghc.md") ruleContent of
        Nothing -> expectationFailure "Failed to parse rule file"
        Just rule -> do
          ruleMatchesFiles rule ["src/Hach/Core.hs"] `shouldBe` True
          ruleMatchesFiles rule ["README.md"] `shouldBe` False
