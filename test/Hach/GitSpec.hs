{-# LANGUAGE OverloadedStrings #-}

module Hach.GitSpec (spec) where

import Hach.Git
import Hach.Types
import Control.Monad (when)
import qualified Data.Text as T
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  )
import System.Process (callProcess)
import Test.Hspec

spec :: Spec
spec = describe "Hach.Git" $ do
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
      let raw = "## main...origin/main\n M src/Hach/Core.hs\n?? new-file.txt\n"
          status = parsePorcelainStatus "main" raw
      gsiBranch status `shouldBe` "main"
      gsiClean status `shouldBe` False
      gsiModified status `shouldBe` ["src/Hach/Core.hs"]
      gsiUntracked status `shouldBe` ["new-file.txt"]

    it "correctly parses renamed and quoted paths (issue #68)" $ do
      let raw = "## main\nR  old.txt -> new.txt\n M \"file with spaces.txt\"\n"
          status = parsePorcelainStatus "main" raw
      gsiModified status `shouldBe` ["new.txt", "file with spaces.txt"]

    it "handles quoted renames, copies, and escape sequences" $ do
      let raw = T.unlines
            [ "## main"
            , "R  \"old with spaces.txt\" -> \"new with spaces.txt\""
            , "RM \"old -> arrow.txt\" -> \"new -> arrow.txt\""
            , "C  source.txt -> copy.txt"
            , "?? \"file\\\"quotes.txt\""
            , "?? \"file\\\\backslash.txt\""
            , "?? \"caf\\303\\251.txt\""
            , "?? \"literal -> arrow.txt\""
            ]
          status = parsePorcelainStatus "main" raw
      gsiModified status `shouldBe`
        [ "new with spaces.txt"
        , "new -> arrow.txt"
        , "copy.txt"
        ]
      gsiUntracked status `shouldBe`
        [ "file\"quotes.txt"
        , "file\\backslash.txt"
        , "café.txt"
        , "literal -> arrow.txt"
        ]


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

    it "rejects path traversal and flag injection in worktree names" $ do
      createWorktree "." "../../src" `shouldReturn` Left "Invalid worktree name: must be alphanumeric and cannot contain path separators or leading dashes."
      createWorktree "." "--orphan" `shouldReturn` Left "Invalid worktree name: must be alphanumeric and cannot contain path separators or leading dashes."
      removeWorktree "." "../escape" `shouldReturn` Left "Invalid worktree name: must be alphanumeric and cannot contain path separators or leading dashes."
      removeWorktree "." "-f" `shouldReturn` Left "Invalid worktree name: must be alphanumeric and cannot contain path separators or leading dashes."

  describe "isWorktreeDirectory" $ do
    it "returns False for non-worktree directory" $ do
      let testDir = "dist-newstyle/test-not-worktree"
      exists <- doesDirectoryExist testDir
      when exists (removeDirectoryRecursive testDir)
      createDirectoryIfMissing True testDir
      isWorktreeDirectory testDir `shouldReturn` False
      removeDirectoryRecursive testDir

  describe "createWorktree branch creation (-b)" $ do
    it "reuses an existing worktree instead of force-resetting its branch" $ do
      let tempDir = "dist-newstyle/test-git-branch-exists"
      exists <- doesDirectoryExist tempDir
      when exists (removeDirectoryRecursive tempDir)
      createDirectoryIfMissing True tempDir
      canonicalTempDir <- canonicalizePath tempDir
      callProcess "git" ["-C", canonicalTempDir, "init"]
      callProcess "git" ["-C", canonicalTempDir, "config", "user.name", "Test"]
      callProcess "git" ["-C", canonicalTempDir, "config", "user.email", "test@test.com"]
      callProcess "git" ["-C", canonicalTempDir, "commit", "--allow-empty", "-m", "init"]
      r1 <- createWorktree canonicalTempDir "feature-dup"
      r1 `shouldSatisfy` \case Right _ -> True; Left _ -> False
      r2 <- createWorktree canonicalTempDir "feature-dup"
      r2 `shouldBe` r1
      removeDirectoryRecursive tempDir

    it "attaches to an existing branch when the branch already exists in the repository" $ do
      let tempDir = "dist-newstyle/test-git-existing-branch"
      exists <- doesDirectoryExist tempDir
      when exists (removeDirectoryRecursive tempDir)
      createDirectoryIfMissing True tempDir
      canonicalTempDir <- canonicalizePath tempDir
      callProcess "git" ["-C", canonicalTempDir, "init"]
      callProcess "git" ["-C", canonicalTempDir, "config", "user.name", "Test"]
      callProcess "git" ["-C", canonicalTempDir, "config", "user.email", "test@test.com"]
      callProcess "git" ["-C", canonicalTempDir, "commit", "--allow-empty", "-m", "init"]
      callProcess "git" ["-C", canonicalTempDir, "branch", "feature-test"]
      res <- createWorktree canonicalTempDir "feature-test"
      res `shouldBe` Right (worktreePath canonicalTempDir "feature-test")
      status <- getGitStatus (worktreePath canonicalTempDir "feature-test")
      gsiBranch status `shouldBe` "feature-test"
      removeDirectoryRecursive tempDir

    it "creates a new branch when the branch does not exist in the repository" $ do
      let tempDir = "dist-newstyle/test-git-new-branch"
      exists <- doesDirectoryExist tempDir
      when exists (removeDirectoryRecursive tempDir)
      createDirectoryIfMissing True tempDir
      canonicalTempDir <- canonicalizePath tempDir
      callProcess "git" ["-C", canonicalTempDir, "init"]
      callProcess "git" ["-C", canonicalTempDir, "config", "user.name", "Test"]
      callProcess "git" ["-C", canonicalTempDir, "config", "user.email", "test@test.com"]
      callProcess "git" ["-C", canonicalTempDir, "commit", "--allow-empty", "-m", "init"]
      res <- createWorktree canonicalTempDir "new-feature"
      res `shouldBe` Right (worktreePath canonicalTempDir "new-feature")
      status <- getGitStatus (worktreePath canonicalTempDir "new-feature")
      gsiBranch status `shouldBe` "new-feature"
      removeDirectoryRecursive tempDir

    it "re-creates a worktree after removeWorktree when the branch remains in the repository" $ do
      let tempDir = "dist-newstyle/test-git-remove-recreate"
      exists <- doesDirectoryExist tempDir
      when exists (removeDirectoryRecursive tempDir)
      createDirectoryIfMissing True tempDir
      canonicalTempDir <- canonicalizePath tempDir
      callProcess "git" ["-C", canonicalTempDir, "init"]
      callProcess "git" ["-C", canonicalTempDir, "config", "user.name", "Test"]
      callProcess "git" ["-C", canonicalTempDir, "config", "user.email", "test@test.com"]
      callProcess "git" ["-C", canonicalTempDir, "commit", "--allow-empty", "-m", "init"]
      r1 <- createWorktree canonicalTempDir "ephemeral-feature"
      r1 `shouldBe` Right (worktreePath canonicalTempDir "ephemeral-feature")
      removeRes <- removeWorktree canonicalTempDir "ephemeral-feature"
      removeRes `shouldBe` Right ()
      r2 <- createWorktree canonicalTempDir "ephemeral-feature"
      r2 `shouldBe` Right (worktreePath canonicalTempDir "ephemeral-feature")
      status <- getGitStatus (worktreePath canonicalTempDir "ephemeral-feature")
      gsiBranch status `shouldBe` "ephemeral-feature"
      removeDirectoryRecursive tempDir

    it "rejects an existing non-worktree target directory" $ do
      let tempDir = "dist-newstyle/test-git-target-exists"
      exists <- doesDirectoryExist tempDir
      when exists (removeDirectoryRecursive tempDir)
      createDirectoryIfMissing True tempDir
      canonicalTempDir <- canonicalizePath tempDir
      let targetDir = worktreePath canonicalTempDir "feature-conflict"
      createDirectoryIfMissing True targetDir
      result <- createWorktree canonicalTempDir "feature-conflict"
      result `shouldBe`
        Left ("Failed to create worktree: target path already exists and is not a git worktree: " <> T.pack targetDir)
      removeDirectoryRecursive tempDir
