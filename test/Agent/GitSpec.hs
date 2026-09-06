{-# LANGUAGE OverloadedStrings #-}

module Agent.GitSpec (spec) where

import Agent.Git
import Agent.Types
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "Agent.Git" $ do
  describe "Commit attribution" $ do
    it "appends Co-Authored-By trailer when not present" $ do
      let msg = "Fix parser bug"
          res = appendCoAuthor msg "Claude <noreply@anthropic.com>"
      res `shouldBe` "Fix parser bug\n\nCo-Authored-By: Claude <noreply@anthropic.com>"

    it "does not duplicate trailer if already present" $ do
      let msg = "Fix bug\n\nCo-Authored-By: Claude <noreply@anthropic.com>"
          res = appendCoAuthor msg "Claude <noreply@anthropic.com>"
      res `shouldBe` msg

  describe "Status parsing" $ do
    it "parses porcelain status output" $ do
      let raw = "## main...origin/main\n M src/Agent/Core.hs\n?? new-file.txt\n"
          status = parsePorcelainStatus "main" raw
      gsiBranch status `shouldBe` "main"
      gsiClean status `shouldBe` False
      gsiModified status `shouldBe` ["src/Agent/Core.hs"]
      gsiUntracked status `shouldBe` ["new-file.txt"]

    it "identifies clean working tree" $ do
      let raw = "## main\n"
          status = parsePorcelainStatus "main" raw
      gsiClean status `shouldBe` True

  describe "Worktree directory" $ do
    it "computes worktree directory under .agents/worktrees" $ do
      worktreePath "/repo" "feature-x" `shouldBe` "/repo/.agents/worktrees/feature-x"
