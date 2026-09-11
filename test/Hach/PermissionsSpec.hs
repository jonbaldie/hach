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
      evalPermission ModeAcceptEdits [] "Edit" editArgs `shouldBe` PermAllow
      evalPermission ModeAcceptEdits [] "edit" editArgs `shouldBe` PermAllow

    it "still asks for command execution including bash and Bash" $ do
      let args = object ["command" .= ("rm -rf /" :: String)]
      case evalPermission ModeAcceptEdits [] "run_command" args of
        PermAsk _ -> pure ()
        other     -> expectationFailure ("Expected PermAsk for run_command, got " <> show other)
      case evalPermission ModeAcceptEdits [] "bash" args of
        PermAsk _ -> pure ()
        other     -> expectationFailure ("Expected PermAsk for bash, got " <> show other)
      case evalPermission ModeAcceptEdits [] "Bash" args of
        PermAsk _ -> pure ()
        other     -> expectationFailure ("Expected PermAsk for Bash, got " <> show other)

    it "denies protected path writes when called via edit tool alias" $ do
      let editArgs = object ["path" .= (".git/config" :: String), "old_content" .= ("a" :: String), "new_content" .= ("b" :: String)]
      case evalPermission ModeAcceptEdits [] "edit" editArgs of
        PermDeny _ -> pure ()
        other      -> expectationFailure ("Expected PermDeny for edit on .git, got " <> show other)

    it "defaults to ask for unknown or unspecified tools in AcceptEdits mode" $ do
      let args = object []
      case evalPermission ModeAcceptEdits [] "unknown_tool" args of
        PermAsk _ -> pure ()
        other     -> expectationFailure ("Expected PermAsk for unknown tool in AcceptEdits, got " <> show other)


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

    it "allows ExitPlanMode, AskUserQuestion, and EndConversation" $ do
      let args = object []
      evalPermission ModePlan [] "ExitPlanMode" args `shouldBe` PermAllow
      evalPermission ModePlan [] "exit_plan_mode" args `shouldBe` PermAllow
      evalPermission ModePlan [] "AskUserQuestion" args `shouldBe` PermAllow
      evalPermission ModePlan [] "ask_user_question" args `shouldBe` PermAllow
      evalPermission ModePlan [] "EndConversation" args `shouldBe` PermAllow
      evalPermission ModePlan [] "end_conversation" args `shouldBe` PermAllow
      evalPermission ModePlan [] "Skill" args `shouldBe` PermAllow
      evalPermission ModePlan [] "skill" args `shouldBe` PermAllow
      evalPermission ModePlan [] "EnterPlanMode" args `shouldBe` PermAllow
      evalPermission ModePlan [] "enter_plan_mode" args `shouldBe` PermAllow

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

  describe "TaskCreate with a command (issue #110)" $ do
    let cmdArgs = object ["name" .= ("t" :: String), "command" .= ("rm -rf /tmp/x" :: String)]
        noCmdArgs = object ["name" .= ("t" :: String)]
        blankCmdArgs = object ["name" .= ("t" :: String), "command" .= ("   " :: String)]
        tools = ["TaskCreate", "task_create"]

    it "asks for command approval in acceptEdits mode" $
      mapM_ (\t -> evalPermission ModeAcceptEdits [] t cmdArgs
                     `shouldBe` PermAsk ("Command execution requires approval: " <> t)) tools

    it "asks for approval in auto mode" $
      mapM_ (\t -> evalPermission ModeAuto [] t cmdArgs
                     `shouldBe` PermAsk ("Auto mode requires approval for: " <> t)) tools

    it "asks for approval in default mode" $
      mapM_ (\t -> evalPermission ModeDefault [] t cmdArgs
                     `shouldBe` PermAsk ("Tool execution requires approval: " <> t)) tools

    it "is denied in plan mode with or without a command" $
      mapM_ (\t -> mapM_ (\a -> evalPermission ModePlan [] t a
                                  `shouldBe` PermDeny "Plan mode is read-only. Tool execution denied.")
                         [cmdArgs, noCmdArgs, blankCmdArgs]) tools

    it "is allowed in dontAsk and bypassPermissions modes" $
      mapM_ (\t -> mapM_ (\m -> evalPermission m [] t cmdArgs `shouldBe` PermAllow)
                         [ModeDontAsk, ModeBypassPermissions]) tools

    it "stays a write tool without a non-blank command" $
      mapM_ (\t -> mapM_ (\(m, a) -> evalPermission m [] t a `shouldBe` PermAllow)
                         [ (m, a) | m <- [ModeAcceptEdits, ModeAuto], a <- [noCmdArgs, blankCmdArgs] ]) tools
