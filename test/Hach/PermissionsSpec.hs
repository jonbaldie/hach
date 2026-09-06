{-# LANGUAGE OverloadedStrings #-}

module Hach.PermissionsSpec (spec) where

import Hach.Permissions
import Hach.Types
import Data.Aeson (object, (.=))
import Test.Hspec

spec :: Spec
spec = describe "Hach.Permissions" $ do
  describe "Default mode" $ do
    it "allows read-only tools" $ do
      let args = object ["path" .= ("src/Hach.hs" :: String)]
      evalPermission ModeDefault [] "read_file" args `shouldBe` PermAllow
      evalPermission ModeDefault [] "list_dir" args `shouldBe` PermAllow
      evalPermission ModeDefault [] "find_files" args `shouldBe` PermAllow
      evalPermission ModeDefault [] "grep_search" args `shouldBe` PermAllow

    it "asks for file write tools" $ do
      let args = object ["path" .= ("src/Hach.hs" :: String), "content" .= ("" :: String)]
      case evalPermission ModeDefault [] "write_file" args of
        PermAsk _ -> pure ()
        other     -> expectationFailure ("Expected PermAsk, got " <> show other)

    it "asks for command execution" $ do
      let args = object ["command" .= ("cargo test" :: String)]
      case evalPermission ModeDefault [] "run_command" args of
        PermAsk _ -> pure ()
        other     -> expectationFailure ("Expected PermAsk, got " <> show other)

  describe "AcceptEdits mode" $ do
    it "auto-approves file writes and edits" $ do
      let writeArgs = object ["path" .= ("src/Hach.hs" :: String), "content" .= ("" :: String)]
          editArgs = object ["path" .= ("src/Hach.hs" :: String), "old_content" .= ("a" :: String), "new_content" .= ("b" :: String)]
      evalPermission ModeAcceptEdits [] "write_file" writeArgs `shouldBe` PermAllow
      evalPermission ModeAcceptEdits [] "replace_file_content" editArgs `shouldBe` PermAllow

    it "still asks for command execution" $ do
      let args = object ["command" .= ("rm -rf /" :: String)]
      case evalPermission ModeAcceptEdits [] "run_command" args of
        PermAsk _ -> pure ()
        other     -> expectationFailure ("Expected PermAsk, got " <> show other)

  describe "Plan mode" $ do
    it "allows read operations" $ do
      let args = object ["path" .= ("README.md" :: String)]
      evalPermission ModePlan [] "read_file" args `shouldBe` PermAllow

    it "denies write and command execution" $ do
      let writeArgs = object ["path" .= ("README.md" :: String), "content" .= ("" :: String)]
          cmdArgs = object ["command" .= ("ls" :: String)]
      case evalPermission ModePlan [] "write_file" writeArgs of
        PermDeny _ -> pure ()
        other      -> expectationFailure ("Expected PermDeny, got " <> show other)
      case evalPermission ModePlan [] "run_command" cmdArgs of
        PermDeny _ -> pure ()
        other      -> expectationFailure ("Expected PermDeny, got " <> show other)

  describe "DontAsk and BypassPermissions modes" $ do
    it "auto-approves all actions in DontAsk" $ do
      let cmdArgs = object ["command" .= ("make clean" :: String)]
      evalPermission ModeDontAsk [] "run_command" cmdArgs `shouldBe` PermAllow

    it "auto-approves all actions in BypassPermissions" $ do
      let cmdArgs = object ["command" .= ("make clean" :: String)]
      evalPermission ModeBypassPermissions [] "run_command" cmdArgs `shouldBe` PermAllow

  describe "Protected paths" $ do
    it "denies writes to .git directory" $ do
      let args = object ["path" .= (".git/config" :: String), "content" .= ("" :: String)]
      case evalPermission ModeAcceptEdits [] "write_file" args of
        PermDeny _ -> pure ()
        other      -> expectationFailure ("Expected PermDeny for .git write, got " <> show other)

    it "denies writes to .claude directory" $ do
      let args = object ["path" .= (".claude/settings.json" :: String), "content" .= ("" :: String)]
      case evalPermission ModeAcceptEdits [] "write_file" args of
        PermDeny _ -> pure ()
        other      -> expectationFailure ("Expected PermDeny for .claude write, got " <> show other)

    it "denies writes to .agents directory" $ do
      let args = object ["path" .= (".agents/worktrees/feat" :: String), "content" .= ("" :: String)]
      case evalPermission ModeAcceptEdits [] "write_file" args of
        PermDeny _ -> pure ()
        other      -> expectationFailure ("Expected PermDeny for .agents write, got " <> show other)

    it "allows writes to standard repository files like .gitignore, .gitattributes, and .github (BUG-3)" $ do
      isProtectedPath ".gitignore" `shouldBe` False
      isProtectedPath ".gitattributes" `shouldBe` False
      isProtectedPath ".gitmodules" `shouldBe` False
      isProtectedPath ".github/workflows/ci.yml" `shouldBe` False
      isProtectedPath ".gitlab-ci.yml" `shouldBe` False

      let gitignoreArgs = object ["path" .= (".gitignore" :: String), "content" .= ("dist-newstyle\n" :: String)]
      evalPermission ModeAcceptEdits [] "write_file" gitignoreArgs `shouldBe` PermAllow

      let ciArgs = object ["path" .= (".github/workflows/ci.yml" :: String), "content" .= ("name: CI\n" :: String)]
      evalPermission ModeAcceptEdits [] "write_file" ciArgs `shouldBe` PermAllow

  describe "Glob matching (BUG-4)" $ do
    it "matches zero or more directories with **/" $ do
      matchGlob "src/**/*.hs" "src/foo.hs" `shouldBe` True
      matchGlob "src/**/*.hs" "src/bar/foo.hs" `shouldBe` True
      matchGlob "src/**/*.hs" "src/a/b/c/foo.hs" `shouldBe` True
      matchGlob "**/*.hs" "foo.hs" `shouldBe` True
      matchGlob "**/*.hs" "a/b/foo.hs" `shouldBe` True
      matchGlob "src/*.hs" "src/foo.hs" `shouldBe` True
      matchGlob "src/*.hs" "src/sub/foo.hs" `shouldBe` False

  describe "Explicit rules" $ do
    it "allow rule overrides default ask" $ do
      let rule = PermissionRule
            { prAction   = RuleAllow
            , prTool     = Just "write_file"
            , prPathGlob = Just "src/**"
            }
          args = object ["path" .= ("src/Foo.hs" :: String), "content" .= ("" :: String)]
      evalPermission ModeDefault [rule] "write_file" args `shouldBe` PermAllow

    it "deny rule blocks tool" $ do
      let rule = PermissionRule
            { prAction   = RuleDeny
            , prTool     = Just "run_command"
            , prPathGlob = Nothing
            }
          args = object ["command" .= ("cabal test" :: String)]
      case evalPermission ModeDefault [rule] "run_command" args of
        PermDeny _ -> pure ()
        other      -> expectationFailure ("Expected PermDeny, got " <> show other)
