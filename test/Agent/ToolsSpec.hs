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

      it "finds files matching glob/extension pattern" $ do
        _ <- executeWriteFile "." (WriteFileArgs (testSandbox </> "A.hs") "module A where")
        _ <- executeWriteFile "." (WriteFileArgs (testSandbox </> "B.txt") "notes")
        res <- executeFindFiles "." (FindFilesArgs "*.hs" testSandbox)
        case res of
          ToolSuccess out -> do
            out `shouldSatisfy` ("A.hs" `T.isInfixOf`)
            out `shouldSatisfy` (not . ("B.txt" `T.isInfixOf`))
          ToolError err -> expectationFailure (T.unpack err)

      it "greps files for matching pattern and reports line number" $ do
        let targetFile = testSandbox </> "Code.hs"
        _ <- executeWriteFile "." (WriteFileArgs targetFile "line 1\nsearchTarget here\nline 3")
        res <- executeGrepSearch "." (GrepSearchArgs "searchTarget" testSandbox True)
        case res of
          ToolSuccess out -> do
            out `shouldSatisfy` ("Code.hs:2: searchTarget here" `T.isInfixOf`)
          ToolError err -> expectationFailure (T.unpack err)

