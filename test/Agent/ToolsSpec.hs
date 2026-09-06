{-# LANGUAGE OverloadedStrings #-}

module Agent.ToolsSpec (spec) where

import Agent.Tools
import Agent.Types
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "Tool argument parsing" $ do
    it "parses valid read_file arguments" $ do
      let call = ToolCall "c1" "read_file" "{\"path\":\"src/Main.hs\"}"
      parseReadFileArgs call `shouldBe` Right (ReadFileArgs "src/Main.hs")

    it "rejects read_file arguments missing 'path'" $ do
      let call = ToolCall "c1" "read_file" "{\"wrong\":\"value\"}"
      case parseReadFileArgs call of
        Left _  -> pure ()
        Right _ -> expectationFailure "Expected parse failure when 'path' is missing"

    it "parses valid write_file arguments" $ do
      let call = ToolCall "c2" "write_file" "{\"path\":\"test.txt\",\"content\":\"hello\"}"
      parseWriteFileArgs call `shouldBe` Right (WriteFileArgs "test.txt" "hello")

    it "parses valid replace_file_content arguments" $ do
      let call = ToolCall "c_rep" "replace_file_content" "{\"path\":\"foo.hs\",\"old_content\":\"old\",\"new_content\":\"new\"}"
      parseReplaceFileContentArgs call `shouldBe` Right (ReplaceFileContentArgs "foo.hs" "old" "new")

    it "rejects replace_file_content when missing old_content" $ do
      let call = ToolCall "c_rep" "replace_file_content" "{\"path\":\"foo.hs\",\"new_content\":\"new\"}"
      case parseReplaceFileContentArgs call of
        Left _  -> pure ()
        Right _ -> expectationFailure "Expected parse failure when old_content missing"

    it "parses valid find_files arguments" $ do
      let call1 = ToolCall "c_f1" "find_files" "{\"pattern\":\"*.hs\"}"
          call2 = ToolCall "c_f2" "find_files" "{\"pattern\":\"*.hs\",\"path\":\"src\"}"
      parseFindFilesArgs call1 `shouldBe` Right (FindFilesArgs "*.hs" ".")
      parseFindFilesArgs call2 `shouldBe` Right (FindFilesArgs "*.hs" "src")

    it "parses valid grep_search arguments" $ do
      let call1 = ToolCall "c_g1" "grep_search" "{\"query\":\"import\"}"
          call2 = ToolCall "c_g2" "grep_search" "{\"query\":\"import\",\"path\":\"src\",\"case_sensitive\":false}"
      parseGrepSearchArgs call1 `shouldBe` Right (GrepSearchArgs "import" "." True)
      parseGrepSearchArgs call2 `shouldBe` Right (GrepSearchArgs "import" "src" False)

    it "parses valid run_command arguments" $ do
      let call = ToolCall "c3" "run_command" "{\"command\":\"echo 42\"}"
      parseRunCommandArgs call `shouldBe` Right (RunCommandArgs "echo 42")

    it "parses list_dir with explicit path" $ do
      let call = ToolCall "c4" "list_dir" "{\"path\":\"src\"}"
      parseListDirArgs call `shouldBe` Right (ListDirArgs "src")

    it "parses list_dir defaulting to '.'" $ do
      let call = ToolCall "c4" "list_dir" "{}"
      parseListDirArgs call `shouldBe` Right (ListDirArgs ".")

    it "parses Edit tool arguments" $ do
      let call = ToolCall "c_edit" "Edit" "{\"path\":\"file.txt\",\"old_content\":\"a\",\"new_content\":\"b\"}"
      parseEditArgs call `shouldBe` Right (EditArgs "file.txt" "a" "b")

    it "parses Bash tool arguments with optional timeout" $ do
      let call1 = ToolCall "c_b1" "Bash" "{\"command\":\"ls -la\"}"
          call2 = ToolCall "c_b2" "Bash" "{\"command\":\"sleep 10\",\"timeout\":15}"
      parseBashArgs call1 `shouldBe` Right (BashArgs "ls -la" Nothing)
      parseBashArgs call2 `shouldBe` Right (BashArgs "sleep 10" (Just 15))

    it "parses Glob tool arguments" $ do
      let call = ToolCall "c_glob" "Glob" "{\"pattern\":\"*.hs\",\"path\":\"src\"}"
      parseGlobArgs call `shouldBe` Right (GlobArgs "*.hs" "src")

    it "parses Grep tool arguments" $ do
      let call = ToolCall "c_grep" "Grep" "{\"query\":\"data \",\"path\":\"src\",\"case_sensitive\":true}"
      parseGrepArgs call `shouldBe` Right (GrepArgs "data " "src" True)

    it "parses WebFetch arguments" $ do
      let call = ToolCall "c_fetch" "WebFetch" "{\"url\":\"https://example.com\"}"
      parseWebFetchArgs call `shouldBe` Right (WebFetchArgs "https://example.com")

    it "parses WebSearch arguments" $ do
      let call = ToolCall "c_search" "WebSearch" "{\"query\":\"haskell free monads\"}"
      parseWebSearchArgs call `shouldBe` Right (WebSearchArgs "haskell free monads")

    it "parses Agent arguments" $ do
      let call = ToolCall "c_agent" "Agent" "{\"name\":\"explore\",\"prompt\":\"Find files\"}"
      parseAgentArgs call `shouldBe` Right (AgentArgs "explore" "Find files")

    it "parses TodoWrite arguments" $ do
      let call = ToolCall "c_todo" "TodoWrite" "{\"tasks\":[\"task 1\",\"task 2\"]}"
      parseTodoWriteArgs call `shouldBe` Right (TodoWriteArgs ["task 1", "task 2"])

    it "parses Skill arguments" $ do
      let call = ToolCall "c_skill" "Skill" "{\"name\":\"review\",\"args\":\"file.hs\"}"
      parseSkillToolArgs call `shouldBe` Right (SkillToolArgs "review" (Just "file.hs"))

    it "parses PushNotification arguments" $ do
      let call = ToolCall "c_notify" "PushNotification" "{\"title\":\"Build\",\"message\":\"Done!\"}"
      parsePushNotificationArgs call `shouldBe` Right (PushNotificationArgs "Build" "Done!")

    it "parses Task arguments (Create, Get, Update, Stop)" $ do
      let callCreate = ToolCall "c_tc" "TaskCreate" "{\"name\":\"compile\",\"command\":\"cabal build\"}"
          callGet = ToolCall "c_tg" "TaskGet" "{\"task_id\":\"task-1\"}"
          callUpdate = ToolCall "c_tu" "TaskUpdate" "{\"task_id\":\"task-1\",\"status\":\"completed\"}"
          callStop = ToolCall "c_ts" "TaskStop" "{\"task_id\":\"task-1\"}"
      parseTaskCreateArgs callCreate `shouldBe` Right (TaskCreateArgs "compile" (Just "cabal build"))
      parseTaskGetArgs callGet `shouldBe` Right (TaskGetArgs (TaskId "task-1"))
      parseTaskUpdateArgs callUpdate `shouldBe` Right (TaskUpdateArgs (TaskId "task-1") "completed")
      parseTaskStopArgs callStop `shouldBe` Right (TaskStopArgs (TaskId "task-1"))

    it "parses AskUserQuestion arguments" $ do
      let call = ToolCall "c_ask" "AskUserQuestion" "{\"question\":\"Proceed?\",\"options\":[\"yes\",\"no\"]}"
      parseAskUserQuestionArgs call `shouldBe` Right (AskUserQuestionArgs "Proceed?" ["yes", "no"])

  describe "allToolDefs" $ do
    it "contains all seven coding tools" $ do
      let names = map toolName allToolDefs
      names `shouldContain`
        [ "read_file"
        , "write_file"
        , "replace_file_content"
        , "run_command"
        , "list_dir"
        , "find_files"
        , "grep_search"
        ]

  describe "truncateToolOutput" $ do
    it "leaves short output intact" $ do
      let out = "Line 1\nLine 2"
      truncateToolOutput out `shouldBe` out

    it "truncates output exceeding max lines" $ do
      let linesList = [ "Line " <> T.pack (show (i :: Int)) | i <- [1..1200] ]
          longOutput = T.unlines linesList
          res = truncateToolOutput longOutput
      res `shouldSatisfy` \t -> "[Output truncated" `T.isInfixOf` t
      length (T.lines res) `shouldSatisfy` (<= 1005)

  describe "Tool Execution" $ do
    let testSandbox = "dist-newstyle/test-sandbox-tools"

    around_ (\action -> do
      createDirectoryIfMissing True testSandbox
      action
      removeDirectoryRecursive testSandbox) $ do

      it "replaces unique text in a file" $ do
        let targetFile = testSandbox </> "sample.txt"
        _ <- executeWriteFile "." (WriteFileArgs targetFile "foo bar baz")
        res <- executeReplaceFileContent "." (ReplaceFileContentArgs targetFile "bar" "qux")
        case res of
          ToolSuccess _ -> do
            readRes <- executeReadFile "." (ReadFileArgs targetFile)
            readRes `shouldBe` ToolSuccess "foo qux baz"
          ToolError err -> expectationFailure ("Unexpected error: " ++ T.unpack err)

      it "fails to replace text when target content is not found" $ do
        let targetFile = testSandbox </> "sample.txt"
        _ <- executeWriteFile "." (WriteFileArgs targetFile "foo bar baz")
        res <- executeReplaceFileContent "." (ReplaceFileContentArgs targetFile "missing" "qux")
        case res of
          ToolError err -> err `shouldSatisfy` ("not found" `T.isInfixOf`)
          ToolSuccess _ -> expectationFailure "Expected error when target content missing"

      it "fails to replace text when target content occurs multiple times" $ do
        let targetFile = testSandbox </> "sample.txt"
        _ <- executeWriteFile "." (WriteFileArgs targetFile "repeat repeat repeat")
        res <- executeReplaceFileContent "." (ReplaceFileContentArgs targetFile "repeat" "single")
        case res of
          ToolError err -> err `shouldSatisfy` ("multiple" `T.isInfixOf`)
          ToolSuccess _ -> expectationFailure "Expected error when target content occurs multiple times"

      it "returns ToolError and does not crash when old_content is empty" $ do
        let targetFile = testSandbox </> "sample.txt"
        _ <- executeWriteFile "." (WriteFileArgs targetFile "foo bar baz")
        res <- executeReplaceFileContent "." (ReplaceFileContentArgs targetFile "" "qux")
        case res of
          ToolError err -> err `shouldSatisfy` ("cannot be empty" `T.isInfixOf`)
          ToolSuccess _ -> expectationFailure "Expected error when old_content is empty"

      it "finds files matching glob/extension pattern" $ do
        _ <- executeWriteFile "." (WriteFileArgs (testSandbox </> "A.hs") "module A where")
        _ <- executeWriteFile "." (WriteFileArgs (testSandbox </> "B.txt") "notes")
        res <- executeFindFiles "." (FindFilesArgs "*.hs" testSandbox)
        case res of
          ToolSuccess out -> do
            out `shouldSatisfy` ("A.hs" `T.isInfixOf`)
            out `shouldSatisfy` (not . ("B.txt" `T.isInfixOf`))
          ToolError err -> expectationFailure (T.unpack err)

      it "does not match *.hs against files with .hs elsewhere in the name" $ do
        _ <- executeWriteFile "." (WriteFileArgs (testSandbox </> "NotMatch.hsx") "x")
        _ <- executeWriteFile "." (WriteFileArgs (testSandbox </> "Archive.hs.zip") "x")
        _ <- executeWriteFile "." (WriteFileArgs (testSandbox </> "Real.hs") "module Real where")
        res <- executeFindFiles "." (FindFilesArgs "*.hs" testSandbox)
        case res of
          ToolSuccess out -> do
            out `shouldSatisfy` ("Real.hs" `T.isInfixOf`)
            out `shouldSatisfy` (not . ("NotMatch.hsx" `T.isInfixOf`))
            out `shouldSatisfy` (not . ("Archive.hs.zip" `T.isInfixOf`))
          ToolError err -> expectationFailure (T.unpack err)

      it "greps files for matching pattern and reports line number" $ do
        let targetFile = testSandbox </> "Code.hs"
        _ <- executeWriteFile "." (WriteFileArgs targetFile "line 1\nsearchTarget here\nline 3")
        res <- executeGrepSearch "." (GrepSearchArgs "searchTarget" testSandbox True)
        case res of
          ToolSuccess out -> do
            out `shouldSatisfy` ("Code.hs:2: searchTarget here" `T.isInfixOf`)
          ToolError err -> expectationFailure (T.unpack err)

      it "executes Edit tool via executeCodingTool" $ do
        let targetFile = testSandbox </> "edit_target.txt"
        _ <- executeWriteFile "." (WriteFileArgs targetFile "apple banana cherry")
        let call = ToolCall "c_e" "Edit" ("{\"path\":\"" <> T.pack targetFile <> "\",\"old_content\":\"banana\",\"new_content\":\"orange\"}")
        res <- executeCodingTool "." call
        case res of
          ToolSuccess _ -> do
            readRes <- executeReadFile "." (ReadFileArgs targetFile)
            readRes `shouldBe` ToolSuccess "apple orange cherry"
          ToolError err -> expectationFailure ("Edit execution failed: " ++ T.unpack err)

      it "executes Bash command via executeCodingTool" $ do
        let call = ToolCall "c_b" "Bash" "{\"command\":\"echo hello-bash\"}"
        res <- executeCodingTool "." call
        case res of
          ToolSuccess out -> out `shouldSatisfy` ("hello-bash" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("Bash failed: " ++ T.unpack err)

      it "executes Glob tool via executeCodingTool" $ do
        _ <- executeWriteFile "." (WriteFileArgs (testSandbox </> "sub" </> "foo.txt") "content")
        let call = ToolCall "c_g" "Glob" ("{\"pattern\":\"*.txt\",\"path\":\"" <> T.pack testSandbox <> "\"}")
        res <- executeCodingTool "." call
        case res of
          ToolSuccess out -> out `shouldSatisfy` ("foo.txt" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("Glob failed: " ++ T.unpack err)

      it "executes EnterPlanMode and ExitPlanMode" $ do
        r1 <- executeCodingTool "." (ToolCall "p1" "EnterPlanMode" "{}")
        r1 `shouldSatisfy` \case ToolSuccess out -> "plan mode" `T.isInfixOf` out; _ -> False
        r2 <- executeCodingTool "." (ToolCall "p2" "ExitPlanMode" "{}")
        r2 `shouldSatisfy` \case ToolSuccess out -> "Exited" `T.isInfixOf` out; _ -> False

      it "manages tasks via TaskCreate and TaskList" $ do
        r1 <- executeCodingTool "." (ToolCall "t1" "TaskCreate" "{\"name\":\"Build feature\"}")
        r1 `shouldSatisfy` \case ToolSuccess out -> "Build feature" `T.isInfixOf` out; _ -> False
        r2 <- executeCodingTool "." (ToolCall "t2" "TaskList" "{}")
        r2 `shouldSatisfy` \case ToolSuccess out -> "Build feature" `T.isInfixOf` out; _ -> False

      it "writes todos via TodoWrite" $ do
        r <- executeCodingTool testSandbox (ToolCall "tw" "TodoWrite" "{\"tasks\":[\"Step 1\",\"Step 2\"]}")
        r `shouldSatisfy` \case ToolSuccess out -> "2 todo items" `T.isInfixOf` out; _ -> False

      it "manages background tasks with consistent IDs and error handling (BUG-6)" $ do
        r1 <- executeCodingTool "." (ToolCall "t1" "TaskCreate" "{\"name\":\"compile-bg\",\"command\":\"sleep 0.1\"}")
        case r1 of
          ToolSuccess out -> out `shouldSatisfy` ("Created background task bg-" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("TaskCreate failed: " ++ T.unpack err)

        rList <- executeCodingTool "." (ToolCall "tl" "TaskList" "{}")
        case rList of
          ToolSuccess out -> out `shouldSatisfy` ("compile-bg" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("TaskList failed: " ++ T.unpack err)

        rGet <- executeCodingTool "." (ToolCall "tg" "TaskGet" "{\"task_id\":\"bg-1\"}")
        case rGet of
          ToolSuccess out -> out `shouldSatisfy` ("compile-bg" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("TaskGet failed: " ++ T.unpack err)

        rUpdate <- executeCodingTool "." (ToolCall "tu" "TaskUpdate" "{\"task_id\":\"bg-1\",\"status\":\"completed\"}")
        case rUpdate of
          ToolSuccess out -> out `shouldSatisfy` ("Updated task bg-1 status to completed" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("TaskUpdate failed: " ++ T.unpack err)

        rUpdateMissing <- executeCodingTool "." (ToolCall "tu2" "TaskUpdate" "{\"task_id\":\"nonexistent-id\",\"status\":\"completed\"}")
        case rUpdateMissing of
          ToolError err   -> err `shouldSatisfy` ("Task not found: nonexistent-id" `T.isInfixOf`)
          ToolSuccess out -> expectationFailure ("Expected error on nonexistent task update, got: " ++ T.unpack out)

        rStop <- executeCodingTool "." (ToolCall "ts" "TaskStop" "{\"task_id\":\"bg-1\"}")
        case rStop of
          ToolSuccess out -> out `shouldSatisfy` ("Stopped task bg-1" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("TaskStop failed: " ++ T.unpack err)


