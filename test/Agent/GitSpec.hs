{-# LANGUAGE OverloadedStrings #-}

module Agent.GitSpec (spec) where

import Agent.Git
import Agent.Types
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
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

    it "preserves branch names containing dots and version numbers (BUG-8)" $ do
      let raw1 = "## release-1.0...origin/release-1.0\n"
          status1 = parsePorcelainStatus "main" raw1
      gsiBranch status1 `shouldBe` "release-1.0"

      let raw2 = "## v2.0\n"
          status2 = parsePorcelainStatus "main" raw2
      gsiBranch status2 `shouldBe` "v2.0"

      let raw3 = "## feature/fix.2.3...origin/feature/fix.2.3 [ahead 1]\n"
          status3 = parsePorcelainStatus "main" raw3
      gsiBranch status3 `shouldBe` "feature/fix.2.3"

  describe "Worktree directory" $ do
    it "computes worktree directory under .agents/worktrees" $ do
      worktreePath "/repo" "feature-x" `shouldBe` "/repo/.agents/worktrees/feature-x"

  describe "createWorktree security" $ do
    it "does not execute injected shell commands in branch names (BUG-2)" $ do
      let marker = "/tmp/agent-test-git-injection"
          maliciousBranch = "feat; touch " <> T.pack marker <> "; #"
      _ <- createWorktree "." maliciousBranch
      injected <- doesFileExist marker
      if injected then removeFile marker else pure ()
      injected `shouldBe` False
