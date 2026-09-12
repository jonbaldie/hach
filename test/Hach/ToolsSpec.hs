{-# LANGUAGE OverloadedStrings #-}

module Hach.ToolsSpec (spec) where

import Hach.Tools
import Hach.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (Permissions(..), canonicalizePath, createDirectoryIfMissing, createDirectoryLink, doesFileExist, getPermissions, removeDirectoryRecursive, removeFile, removePathForcibly, setPermissions)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import System.Timeout (timeout)
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
      parseRunCommandArgs call `shouldBe` Right (RunCommandArgs "echo 42" Nothing)
      let callWithTimeout = ToolCall "c3_to" "run_command" "{\"command\":\"echo 42\",\"timeout\":10}"
      parseRunCommandArgs callWithTimeout `shouldBe` Right (RunCommandArgs "echo 42" (Just 10))

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

  describe "read workspace capability registry" $ do
    it "resolves every accepted name to its canonical model, authority, and target" $ do
      let cases =
            [ ("read_file", "{\"path\":\"README.md\"}", "read_file", "README.md")
            , ("list_dir", "{}", "list_dir", ".")
            , ("ListDir", "{\"path\":\"src\"}", "list_dir", "src")
            , ("find_files", "{\"pattern\":\"*.hs\"}", "find_files", "*.hs")
            , ("Glob", "{\"pattern\":\"**/*.json\"}", "find_files", "**/*.json")
            , ("glob", "{\"pattern\":\"*.md\"}", "find_files", "*.md")
            , ("grep_search", "{\"query\":\"TODO\"}", "grep_search", "TODO")
            , ("Grep", "{\"query\":\"TODO\"}", "grep_search", "TODO")
            , ("grep", "{\"pattern\":\"FIXME\"}", "grep_search", "FIXME")
            ]
          showResult = \case
            Nothing -> "unknown name"
            Just (Left err) -> T.unpack err
            Just (Right _) -> "resolved unexpectedly"
      mapM_ (\(name, args, canonical, target) ->
        case resolveReadWorkspaceTool (ToolCall "call" name args) of
          Just (Right resolved) -> do
            resolvedReadCanonicalName resolved `shouldBe` canonical
            resolvedReadAuthority resolved `shouldBe` AuthorityRead
            resolvedReadTarget resolved `shouldBe` target
            readWorkspaceToolTarget name args `shouldBe` Just target
          other -> expectationFailure ("Did not resolve " <> T.unpack name <> ": " <> showResult other)
        ) cases

    it "rejects invalid read arguments and unknown read names before execution" $ do
      case resolveReadWorkspaceTool (ToolCall "bad" "Glob" "{}") of
        Just (Left _) -> pure ()
        _ -> expectationFailure "Invalid Glob arguments resolved unexpectedly"
      executeCodingTool "." (ToolCall "bad" "Glob" "{}") `shouldReturn` ToolError "Failed to parse find_files args: Error in $: key \"pattern\" not found"
      executeCodingTool "." (ToolCall "unknown" "unknown_read" "{}") `shouldReturn` ToolError "Unknown tool function: unknown_read"

  describe "command and web capability registry" $ do
    it "resolves every accepted name to its canonical model, authority, and target" $ do
      let cases =
            [ ("run_command", "{\"command\":\"cabal test\",\"timeout\":60}", "run_command", AuthorityCommand, "cabal test")
            , ("Bash", "{\"command\":\"make check\"}", "run_command", AuthorityCommand, "make check")
            , ("bash", "{\"command\":\"stack test\"}", "run_command", AuthorityCommand, "stack test")
            , ("WebFetch", "{\"url\":\"https://example.com\"}", "WebFetch", AuthorityRead, "https://example.com")
            , ("web_fetch", "{\"url\":\"https://example.org\"}", "WebFetch", AuthorityRead, "https://example.org")
            , ("webfetch", "{\"url\":\"https://example.net\"}", "WebFetch", AuthorityRead, "https://example.net")
            , ("WebSearch", "{\"query\":\"Haskell\"}", "WebSearch", AuthorityRead, "Haskell")
            , ("web_search", "{\"query\":\"tool registry\"}", "WebSearch", AuthorityRead, "tool registry")
            , ("websearch", "{\"query\":\"capability\"}", "WebSearch", AuthorityRead, "capability")
            ]
          showResult = \case
            Nothing -> "unknown name"
            Just (Left err) -> T.unpack err
            Just (Right _) -> "resolved unexpectedly"
      mapM_ (\(name, args, canonical, authority, target) ->
        case resolveCommandWebTool (ToolCall "call" name args) of
          Just (Right resolved) -> do
            resolvedCommandWebCanonicalName resolved `shouldBe` canonical
            resolvedCommandWebAuthority resolved `shouldBe` authority
            resolvedCommandWebTarget resolved `shouldBe` target
            commandWebToolTarget name args `shouldBe` Just target
          other -> expectationFailure ("Did not resolve " <> T.unpack name <> ": " <> showResult other)
        ) cases

    it "rejects invalid command and web arguments before execution" $ do
      case resolveCommandWebTool (ToolCall "bad" "Bash" "{}") of
        Just (Left _) -> pure ()
        _ -> expectationFailure "Invalid Bash arguments resolved unexpectedly"
      executeCodingTool "." (ToolCall "bad" "Bash" "{}") `shouldReturn` ToolError "Failed to parse run_command args: Error in $: key \"command\" not found"

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

  describe "matchPattern (find_files)" $ do
    it "evaluates pathological wildcard pattern in linear time without backtracking" $ do
      let pat = "*a*a*a*a*a*a*a*a*a*a*b"
          target = replicate 30 'a'
      matchPattern pat target `shouldBe` False

    it "preserves wildcard semantics matching across directory separators" $ do
      matchPattern "*foo*" "src/foo.hs" `shouldBe` True
      matchPattern "*.hs" "src/foo.hs" `shouldBe` True
      matchPattern "*test*" "test/Hach/ToolsSpec.hs" `shouldBe` True
      matchPattern "*a*b*" "dir/a/sub/b/file.txt" `shouldBe` True
      matchPattern "*.hs" "src/foo.hsx" `shouldBe` False

    it "performs non-wildcard substring searches against name and full path" $ do
      matchPattern "foo" "src/foo.hs" `shouldBe` True
      matchPattern "src" "src/foo.hs" `shouldBe` True
      matchPattern "missing" "src/foo.hs" `shouldBe` False

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

      it "refuses write_file through symlinks into protected directories" $ do
        let protectedTargets =
              [ (".git", ".safe-git-write-link")
              , (".claude", ".safe-claude-write-link")
              , (".agents", ".safe-agents-write-link")
              , (".agent", ".safe-agent-write-link")
              ]
        mapM_ (\(protectedName, linkName) -> do
          let protectedDir = testSandbox </> protectedName
              protectedFile = protectedDir </> "config"
              link = testSandbox </> linkName
              rawPath = linkName <> "/config"
          createDirectoryIfMissing True protectedDir
          writeFile protectedFile "ORIGINAL"
          removePathForcibly link
          protectedDirCanon <- canonicalizePath protectedDir
          createDirectoryLink protectedDirCanon link

          let call = ToolCall "c_protected_write" "write_file"
                ("{\"path\":\"" <> T.pack rawPath <> "\",\"content\":\"HIJACKED\"}")
          res <- executeCodingTool testSandbox call
          res `shouldBe` ToolError ("Protected path: write denied to " <> T.pack rawPath)
          readFile protectedFile `shouldReturn` "ORIGINAL"
          ) protectedTargets

      it "refuses Edit through a symlink into a protected directory" $ do
        let protectedFile = testSandbox </> ".git" </> "config"
            link = testSandbox </> "safe-edit-link"
        createDirectoryIfMissing True (testSandbox </> ".git")
        writeFile protectedFile "ORIGINAL"
        removePathForcibly link
        protectedDir <- canonicalizePath (testSandbox </> ".git")
        createDirectoryLink protectedDir link

        let call = ToolCall "c_protected_edit" "Edit"
              "{\"path\":\"safe-edit-link/config\",\"old_content\":\"ORIGINAL\",\"new_content\":\"HIJACKED\"}"
        res <- executeCodingTool testSandbox call
        res `shouldBe` ToolError "Protected path: edit denied to safe-edit-link/config"
        readFile protectedFile `shouldReturn` "ORIGINAL"

      it "refuses writes through chained symlinks and dot-dot segments" $ do
        let protectedDir = testSandbox </> ".git"
            protectedFile = protectedDir </> "config"
            firstLink = testSandbox </> "safe-chain-one"
            secondLink = testSandbox </> "safe-chain-two"
            rawPath = "prefix/../safe-chain-one/config"
        createDirectoryIfMissing True protectedDir
        writeFile protectedFile "ORIGINAL"
        removePathForcibly firstLink
        removePathForcibly secondLink
        protectedDirCanon <- canonicalizePath protectedDir
        createDirectoryLink protectedDirCanon secondLink
        createDirectoryLink "safe-chain-two" firstLink

        let call = ToolCall "c_protected_chain" "write_file"
              ("{\"path\":\"" <> T.pack rawPath <> "\",\"content\":\"HIJACKED\"}")
        res <- executeCodingTool testSandbox call
        res `shouldBe` ToolError ("Protected path: write denied to " <> T.pack rawPath)
        readFile protectedFile `shouldReturn` "ORIGINAL"

      it "preserves the workspace escape error for writes outside the root" $ do
        let rawPath = "../hach-99-outside.txt"
        res <- executeWriteFile testSandbox (WriteFileArgs rawPath "outside")
        res `shouldBe` ToolError ("Access denied: path '" <> T.pack rawPath <> "' escapes the workspace root.")

      it "allows ordinary writes when the workspace root is under a protected directory name" $ do
        let workspace = testSandbox </> ".claude" </> "workspace"
            targetFile = workspace </> "ordinary.txt"
        createDirectoryIfMissing True workspace
        relativeRes <- executeWriteFile workspace (WriteFileArgs "ordinary.txt" "safe")
        relativeRes `shouldBe` ToolSuccess "Successfully wrote 4 characters to ordinary.txt"
        absoluteTargetFile <- canonicalizePath targetFile
        absoluteRes <- executeWriteFile workspace (WriteFileArgs absoluteTargetFile "safe-again")
        absoluteRes `shouldBe` ToolSuccess ("Successfully wrote 10 characters to " <> T.pack absoluteTargetFile)
        readFile targetFile `shouldReturn` "safe-again"

      it "does not expand commands supplied to the Skill tool" $ do
        sandboxRoot <- canonicalizePath testSandbox
        let skillDir = testSandbox </> ".claude" </> "skills" </> "issue-98"
            marker = sandboxRoot </> "skill-argument-command-ran"
            supplied = "first\n  !touch " <> T.pack marker
        markerExists <- doesFileExist marker
        if markerExists then removeFile marker else pure ()
        createDirectoryIfMissing True skillDir
        TIO.writeFile (skillDir </> "SKILL.md")
          "---\nname: issue-98\ndescription: Issue 98 regression\n---\nTrusted output:\n!printf trusted\nFirst: $ARGUMENTS\nSecond: $ARGUMENTS"
        result <- executeSkill testSandbox (SkillToolArgs "issue-98" (Just supplied))
        doesFileExist marker `shouldReturn` False
        result `shouldSatisfy` \case
          ToolSuccess out ->
            "Trusted output:\ntrusted\nFirst: first\n  !touch " `T.isInfixOf` out
              && "Second: first\n  !touch " `T.isInfixOf` out
          ToolError _ -> False

      it "fails to replace text when target content is not found" $ do
        let targetFile = testSandbox </> "sample.txt"
        _ <- executeWriteFile "." (WriteFileArgs targetFile "foo bar baz")
        res <- executeReplaceFileContent "." (ReplaceFileContentArgs targetFile "missing" "qux")
        case res of
          ToolError err -> err `shouldSatisfy` ("not found" `T.isInfixOf`)
          ToolSuccess _ -> expectationFailure "Expected error when target content missing"

      it "fails to replace text when target content occurs multiple times" $ do
        let targetFile = testSandbox </> "sample.txt"
        _ <- executeWriteFile "." (WriteFileArgs targetFile "repeat repeat")
        res <- executeReplaceFileContent "." (ReplaceFileContentArgs targetFile "repeat" "single")
        res `shouldBe` ToolError
          ("Target content found multiple (2) times in '" <> T.pack targetFile <> "'; replacement requires a unique match.")

      it "refuses overlapping target matches and leaves the file unchanged" $ do
        let targetFile = "overlap.txt"
        _ <- executeWriteFile testSandbox (WriteFileArgs targetFile "aaa")
        let call = ToolCall "c_overlap" "Edit"
              "{\"path\":\"overlap.txt\",\"old_content\":\"aa\",\"new_content\":\"X\"}"
        res <- executeCodingTool testSandbox call
        res `shouldBe` ToolError "Target content found multiple (2) times in 'overlap.txt'; replacement requires a unique match."
        readRes <- executeReadFile testSandbox (ReadFileArgs targetFile)
        readRes `shouldBe` ToolSuccess "aaa"

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

      it "evaluates pathological wildcard patterns without exponential backtracking in executeFindFiles" $ do
        let pathologicalFile = testSandbox </> (replicate 30 'a' ++ ".txt")
        _ <- executeWriteFile "." (WriteFileArgs pathologicalFile "target")
        let pat = "*a*a*a*a*a*a*a*a*a*a*b"
        mRes <- timeout 2000000 (executeFindFiles "." (FindFilesArgs pat testSandbox))
        case mRes of
          Nothing -> expectationFailure "executeFindFiles timed out on pathological pattern (exponential backtracking)"
          Just (ToolSuccess out) -> out `shouldBe` "No matching files found."
          Just (ToolError err)   -> expectationFailure ("Unexpected error: " ++ T.unpack err)

      it "preserves find_files wildcard semantics across path separators in executeFindFiles" $ do
        let nestedFile = testSandbox </> "sub" </> "foo.txt"
        _ <- executeWriteFile "." (WriteFileArgs nestedFile "content")
        r1 <- executeFindFiles "." (FindFilesArgs "*sub*foo*" testSandbox)
        case r1 of
          ToolSuccess out -> out `shouldSatisfy` ("foo.txt" `T.isInfixOf`)
          ToolError err   -> expectationFailure (T.unpack err)
        r2 <- executeFindFiles "." (FindFilesArgs "*.txt" testSandbox)
        case r2 of
          ToolSuccess out -> out `shouldSatisfy` ("foo.txt" `T.isInfixOf`)
          ToolError err   -> expectationFailure (T.unpack err)

      it "performs non-wildcard substring searches in executeFindFiles" $ do
        let nestedFile = testSandbox </> "sub" </> "searchme.txt"
        _ <- executeWriteFile "." (WriteFileArgs nestedFile "content")
        r1 <- executeFindFiles "." (FindFilesArgs "searchme" testSandbox)
        case r1 of
          ToolSuccess out -> out `shouldSatisfy` ("searchme.txt" `T.isInfixOf`)
          ToolError err   -> expectationFailure (T.unpack err)

      it "does not follow directory symlinks in find_files or grep_search" $ do
        sandboxRoot <- canonicalizePath testSandbox
        let workspace = sandboxRoot </> "issue-67-external-workspace"
            outside = sandboxRoot </> "issue-67-external-target"
            secretFile = outside </> "secret.txt"
            externalLink = workspace </> "external"
        removePathForcibly workspace
        removePathForcibly outside
        createDirectoryIfMissing True workspace
        createDirectoryIfMissing True outside
        writeFile secretFile "TOP SECRET CONTENT\n"
        createDirectoryLink outside externalLink

        findRes <- timeout 2000000 (executeFindFiles workspace (FindFilesArgs "*.txt" "."))
        grepRes <- timeout 2000000 (executeGrepSearch workspace (GrepSearchArgs "TOP SECRET" "." True))
        case (findRes, grepRes) of
          (Just (ToolSuccess findOut), Just (ToolSuccess grepOut)) ->
            [ "secret.txt" `T.isInfixOf` findOut
            , "TOP SECRET" `T.isInfixOf` grepOut
            ] `shouldBe` [False, False]
          (Nothing, _) -> expectationFailure "find_files timed out while traversing a directory symlink"
          (_, Nothing) -> expectationFailure "grep_search timed out while traversing a directory symlink"
          (Just (ToolError err), _) -> expectationFailure (T.unpack err)
          (_, Just (ToolError err)) -> expectationFailure (T.unpack err)

      it "terminates when a directory symlink points back to the workspace" $ do
        sandboxRoot <- canonicalizePath testSandbox
        let workspace = sandboxRoot </> "issue-67-cycle-workspace"
            cycleLink = workspace </> "loop"
        removePathForcibly workspace
        createDirectoryIfMissing True workspace
        createDirectoryLink workspace cycleLink

        findRes <- timeout 2000000 (executeFindFiles workspace (FindFilesArgs "*" "."))
        grepRes <- timeout 2000000 (executeGrepSearch workspace (GrepSearchArgs "anything" "." True))
        case (findRes, grepRes) of
          (Just (ToolSuccess findOut), Just (ToolSuccess grepOut)) ->
            [findOut, grepOut] `shouldBe` ["No matching files found.", "No matches found."]
          (Nothing, _) -> expectationFailure "find_files did not terminate on a cyclic directory symlink"
          (_, Nothing) -> expectationFailure "grep_search did not terminate on a cyclic directory symlink"
          (Just (ToolError err), _) -> expectationFailure (T.unpack err)
          (_, Just (ToolError err)) -> expectationFailure (T.unpack err)

      it "executes find_files via executeCodingTool and reports accurate error on parse failure" $ do
        let nestedFile = testSandbox </> "sub" </> "alpha.txt"
        _ <- executeWriteFile "." (WriteFileArgs nestedFile "alpha")
        let call = ToolCall "c_ff" "find_files" ("{\"pattern\":\"*.txt\",\"path\":\"" <> T.pack testSandbox <> "\"}")
        rSuccess <- executeCodingTool "." call
        case rSuccess of
          ToolSuccess out -> out `shouldSatisfy` ("alpha.txt" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("find_files failed: " ++ T.unpack err)
        let badCall = ToolCall "c_bad" "find_files" "{}"
        rBad <- executeCodingTool "." badCall
        case rBad of
          ToolError err   -> err `shouldSatisfy` ("Failed to parse find_files args" `T.isInfixOf`)
          ToolSuccess out -> expectationFailure ("Expected parse error, got: " ++ T.unpack out)

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

      it "runExecCommand runs the command in the workspace and returns its status" $ do
        (code, out, err) <- runExecCommand testSandbox "echo hello-from-exec"
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("hello-from-exec" `T.isInfixOf`)
        err `shouldBe` ""

      it "runExecCommand propagates a non-zero exit status" $ do
        (code, _, _) <- runExecCommand testSandbox "sh -c 'exit 7'"
        code `shouldBe` ExitFailure 7

      it "executes Bash command via executeCodingTool" $ do
        let call = ToolCall "c_b" "Bash" "{\"command\":\"echo hello-bash\"}"
        res <- executeCodingTool "." call
        case res of
          ToolSuccess out -> out `shouldSatisfy` ("hello-bash" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("Bash failed: " ++ T.unpack err)

      it "respects timeout in Bash tool calls via executeCodingTool" $ do
        let call = ToolCall "c_b_to" "Bash" "{\"command\":\"sleep 2\",\"timeout\":1}"
        res <- executeCodingTool "." call
        case res of
          ToolError err   -> err `shouldSatisfy` ("Command timed out after 1 seconds" `T.isInfixOf`)
          ToolSuccess out -> expectationFailure ("Expected timeout error, but succeeded: " ++ T.unpack out)

      it "respects timeout in run_command tool calls via executeCodingTool" $ do
        let call = ToolCall "c_rc_to" "run_command" "{\"command\":\"sleep 2\",\"timeout\":1}"
        res <- executeCodingTool "." call
        case res of
          ToolError err   -> err `shouldSatisfy` ("Command timed out after 1 seconds" `T.isInfixOf`)
          ToolSuccess out -> expectationFailure ("Expected timeout error, but succeeded: " ++ T.unpack out)

      it "terminates the shell and its background children when a command times out" $ do
        root <- canonicalizePath testSandbox
        let cmd = "echo $$ > shell.pid; sleep 30 & echo $! > child.pid; wait"
        res <- executeRunCommand root (RunCommandArgs cmd (Just 1))
        case res of
          ToolError err   -> err `shouldSatisfy` ("Command timed out after 1 seconds" `T.isInfixOf`)
          ToolSuccess out -> expectationFailure ("Expected timeout error, but succeeded: " ++ T.unpack out)
        threadDelay 200000
        let alive pidFile = do
              pid <- T.unpack . T.strip <$> TIO.readFile (root </> pidFile)
              (code, _, _) <- readProcessWithExitCode "kill" ["-0", pid] ""
              pure (code == ExitSuccess)
        alive "shell.pid" `shouldReturn` False
        alive "child.pid" `shouldReturn` False

      it "executes Glob tool via executeCodingTool" $ do
        _ <- executeWriteFile "." (WriteFileArgs (testSandbox </> "sub" </> "foo.txt") "content")
        let call = ToolCall "c_g" "Glob" ("{\"pattern\":\"*.txt\",\"path\":\"" <> T.pack testSandbox <> "\"}")
        res <- executeCodingTool "." call
        case res of
          ToolSuccess out -> out `shouldSatisfy` ("foo.txt" `T.isInfixOf`)
          ToolError err   -> expectationFailure ("Glob failed: " ++ T.unpack err)

      it "executes EnterPlanMode and enter_plan_mode" $ do
        r1 <- executeCodingTool "." (ToolCall "p1" "EnterPlanMode" "{}")
        r1 `shouldBe` ToolSuccess "Entered plan mode. The agent is now in read-only planning mode."
        r2 <- executeCodingTool "." (ToolCall "p2" "enter_plan_mode" "{}")
        r2 `shouldBe` ToolSuccess "Entered plan mode. The agent is now in read-only planning mode."

      it "executes ExitPlanMode and exit_plan_mode" $ do
        r1 <- executeCodingTool "." (ToolCall "p1" "ExitPlanMode" "{}")
        r1 `shouldBe` ToolSuccess "Exited plan mode. The agent is now in standard execution mode."
        r2 <- executeCodingTool "." (ToolCall "p2" "exit_plan_mode" "{}")
        r2 `shouldBe` ToolSuccess "Exited plan mode. The agent is now in standard execution mode."

      it "returns ToolError for ExitWorktree and exit_worktree when not in a worktree" $ do
        r1 <- executeCodingTool testSandbox (ToolCall "w1" "ExitWorktree" "{}")
        r1 `shouldBe` ToolError "Not currently inside a worktree."
        r2 <- executeCodingTool testSandbox (ToolCall "w2" "exit_worktree" "{}")
        r2 `shouldBe` ToolError "Not currently inside a worktree."

      it "executes ListAgents and list_agents" $ do
        r1 <- executeCodingTool "." (ToolCall "a1" "ListAgents" "{}")
        r1 `shouldBe` ToolSuccess "Available subagents: explore, plan."
        r2 <- executeCodingTool "." (ToolCall "a2" "list_agents" "{}")
        r2 `shouldBe` ToolSuccess "Available subagents: explore, plan."

      it "executes EndConversation and end_conversation" $ do
        r1 <- executeCodingTool "." (ToolCall "e1" "EndConversation" "{}")
        r1 `shouldBe` ToolSuccess "Conversation completed by agent."
        r2 <- executeCodingTool "." (ToolCall "e2" "end_conversation" "{}")
        r2 `shouldBe` ToolSuccess "Conversation completed by agent."

      it "manages tasks via TaskCreate and TaskList" $ do
        r1 <- executeCodingTool "." (ToolCall "t1" "TaskCreate" "{\"name\":\"Build feature\"}")
        r1 `shouldSatisfy` \case ToolSuccess out -> "Build feature" `T.isInfixOf` out; _ -> False
        r2 <- executeCodingTool "." (ToolCall "t2" "TaskList" "{}")
        r2 `shouldSatisfy` \case ToolSuccess out -> "Build feature" `T.isInfixOf` out; _ -> False

      it "writes todos via TodoWrite" $ do
        r <- executeCodingTool testSandbox (ToolCall "tw" "TodoWrite" "{\"tasks\":[\"Step 1\",\"Step 2\"]}")
        r `shouldSatisfy` \case ToolSuccess out -> "2 todo items" `T.isInfixOf` out; _ -> False
        TIO.readFile (testSandbox </> ".claude" </> "todos.json") `shouldReturn` "[\"Step 1\",\"Step 2\"]"

      it "returns ToolError when TodoWrite cannot write the workspace" $ do
        originalPermissions <- getPermissions testSandbox
        let readOnlyPermissions = originalPermissions { writable = False }
        setPermissions testSandbox readOnlyPermissions
        (do
          result <- executeCodingTool testSandbox (ToolCall "tw-read-only" "TodoWrite" "{\"tasks\":[]}")
          result `shouldSatisfy` \case
            ToolError err -> "TodoWrite error: " `T.isPrefixOf` err
            ToolSuccess _ -> False
          ) `finally` setPermissions testSandbox originalPermissions

      it "returns ToolError when TodoWrite cannot write its todo file" $ do
        let todoDir = testSandbox </> ".claude"
        createDirectoryIfMissing True todoDir
        originalPermissions <- getPermissions todoDir
        let readOnlyPermissions = originalPermissions { writable = False }
        setPermissions todoDir readOnlyPermissions
        (do
          result <- executeCodingTool testSandbox (ToolCall "tw-file-read-only" "TodoWrite" "{\"tasks\":[]}")
          result `shouldSatisfy` \case
            ToolError err -> "TodoWrite error: " `T.isPrefixOf` err
            ToolSuccess _ -> False
          ) `finally` setPermissions todoDir originalPermissions

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
