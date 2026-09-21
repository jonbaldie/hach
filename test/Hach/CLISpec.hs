module Hach.CLISpec (spec) where

import Control.Exception (finally)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isRight)
import Data.List (intercalate, isPrefixOf)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.FilePath ((</>))
import System.Process
  ( CreateProcess (..)
  , callProcess
  , proc
  , readCreateProcessWithExitCode
  , readProcess
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

spec :: Spec
spec = describe "headless CLI prompt acquisition" $ do
  it "turns closed stdin into the intentional empty-prompt exit" $ do
    executable <- hachExecutable
    withTemporaryWorkspace $ \workspace -> do
      testEnvironment <- isolatedEnvironment workspace
      let command =
            (proc executable ["--no-tui", "--model", "test-model"])
              { cwd = Just workspace
              , env = Just testEnvironment
              }
      (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode command ""

      exitCode `shouldBe` ExitFailure 1
      stdoutText `shouldContain` "Empty task prompt provided. Exiting."
      stderrText `shouldNotContain` "Uncaught exception"
      stderrText `shouldNotContain` "hGetLine: end of file"
      stderrText `shouldNotContain` "HasCallStack backtrace"
      listDirectory workspace `shouldReturn` []

  it "creates and enters a requested worktree before headless startup" $ do
    executable <- hachExecutable
    withTemporaryGitWorkspace $ \workspace -> do
      let worktree = workspace </> ".agents" </> "worktrees" </> "feat-test"
          runCli worktreeArgs = do
            testEnvironment <- isolatedEnvironment workspace
            let command =
                  (proc executable worktreeArgs)
                    { cwd = Just workspace
                    , env = Just testEnvironment
                    }
            readCreateProcessWithExitCode command ""

      (firstExit, firstOutput, firstError) <- runCli ["--no-tui", "--worktree", "feat-test"]
      firstExit `shouldBe` ExitFailure 1
      firstError `shouldBe` ""
      firstOutput `shouldContain` ("Workspace: " <> worktree)
      doesDirectoryExist worktree `shouldReturn` True

      (secondExit, secondOutput, secondError) <- runCli ["--no-tui", "-w", "feat-test"]
      secondExit `shouldBe` ExitFailure 1
      secondError `shouldBe` ""
      secondOutput `shouldContain` ("Workspace: " <> worktree)

  it "attaches to an existing branch when requested via -w before startup" $ do
    executable <- hachExecutable
    withTemporaryGitWorkspace $ \workspace -> do
      let worktree = workspace </> ".agents" </> "worktrees" </> "feat-existing"
          runCli worktreeArgs = do
            testEnvironment <- isolatedEnvironment workspace
            let command =
                  (proc executable worktreeArgs)
                    { cwd = Just workspace
                    , env = Just testEnvironment
                    }
            readCreateProcessWithExitCode command ""

      callProcess "git" ["-C", workspace, "branch", "feat-existing"]
      (exitCode, output, err) <- runCli ["--no-tui", "-w", "feat-existing"]
      exitCode `shouldBe` ExitFailure 1
      err `shouldBe` ""
      output `shouldContain` ("Workspace: " <> worktree)
      doesDirectoryExist worktree `shouldReturn` True

  it "reports invalid worktree names before loading configuration" $ do
    executable <- hachExecutable
    withTemporaryWorkspace $ \workspace -> do
      let command =
            (proc executable ["--no-tui", "--worktree", "../escape"])
              { cwd = Just workspace
              }
      (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode command ""

      exitCode `shouldBe` ExitFailure 1
      stdoutText `shouldContain` "Worktree error: Invalid worktree name:"
      stderrText `shouldBe` ""

  it "reports Git-invalid dot-pattern worktree names before creating a worktree" $ do
    executable <- hachExecutable
    forM_ [".", ".hidden", "feat.", "feat.lock"] $ \name ->
      withTemporaryGitWorkspace $ \workspace -> do
        let command =
              (proc executable ["--no-tui", "--worktree", name])
                { cwd = Just workspace
                }
        (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode command ""

        exitCode `shouldBe` ExitFailure 1
        stdoutText `shouldBe`
          "Worktree error: Invalid worktree name: must be alphanumeric and cannot contain path separators, leading dashes, or invalid git ref patterns.\n"
        stderrText `shouldBe` ""

  it "reports when a requested worktree is outside a Git repository" $ do
    executable <- hachExecutable
    withTemporaryWorkspace $ \workspace -> do
      let command =
            (proc executable ["--no-tui", "--worktree", "feat-outside"])
              { cwd = Just workspace
              }
      (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode command ""

      exitCode `shouldBe` ExitFailure 1
      stdoutText `shouldContain` "Worktree error: Failed to create worktree:"
      stderrText `shouldBe` ""

  it "runs --exec in the requested worktree" $ do
    executable <- hachExecutable
    withTemporaryGitWorkspace $ \workspace -> do
      let worktree = workspace </> ".agents" </> "worktrees" </> "feat-exec"
          command =
            (proc executable ["--worktree", "feat-exec", "--exec", "pwd"])
              { cwd = Just workspace
              }
      (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode command ""

      exitCode `shouldBe` ExitSuccess
      stdoutText `shouldBe` worktree <> "\n"
      stderrText `shouldBe` ""

  it "rejects worktree case collisions on CLI startup" $ do
    executable <- hachExecutable
    withTemporaryGitWorkspace $ \workspace -> do
      let cmd1 =
            (proc executable ["--worktree", "CaseName", "--exec", "git branch --show-current"])
              { cwd = Just workspace
              }
      (exitCode1, stdoutText1, stderrText1) <- readCreateProcessWithExitCode cmd1 ""
      exitCode1 `shouldBe` ExitSuccess
      stdoutText1 `shouldContain` "CaseName"
      stderrText1 `shouldBe` ""

      let cmd2 =
            (proc executable ["--worktree", "casename", "--exec", "git branch --show-current"])
              { cwd = Just workspace
              }
      (exitCode2, stdoutText2, _stderrText2) <- readCreateProcessWithExitCode cmd2 ""
      exitCode2 `shouldBe` ExitFailure 1
      stdoutText2 `shouldContain` "Worktree error: Worktree case collision:"

  it "leaves the primary repository git status clean after -w (issue #156)" $ do
    executable <- hachExecutable
    withTemporaryGitWorkspace $ \workspace -> do
      let worktree = workspace </> ".agents" </> "worktrees" </> "feat-clean"
          command =
            (proc executable ["--worktree", "feat-clean", "--exec", "true"])
              { cwd = Just workspace
              }
      (exitCode, _stdoutText, stderrText) <- readCreateProcessWithExitCode command ""
      exitCode `shouldBe` ExitSuccess
      stderrText `shouldBe` ""
      doesDirectoryExist worktree `shouldReturn` True
      (statusCode, statusOut, _) <-
        readCreateProcessWithExitCode (proc "git" ["-C", workspace, "status", "--porcelain"]) ""
      statusCode `shouldBe` ExitSuccess
      statusOut `shouldBe` ""
      (addCode, _, addErr) <-
        readCreateProcessWithExitCode (proc "git" ["-C", workspace, "add", "-A"]) ""
      addCode `shouldBe` ExitSuccess
      addErr `shouldNotContain` "embedded git repository"

  describe "--print stdout contract (Issue #116)" $ do
    -- An invalid key makes the agent fail on its first request, which drives
    -- agent events through the real renderer without a live model.
    let runPrint executable workspace args = do
          testEnvironment <- isolatedEnvironment workspace
          let command =
                (proc executable (args <> ["--model", "test-model", "What is 2+2?"]))
                  { cwd = Just workspace
                  , env = Just testEnvironment
                  }
          readCreateProcessWithExitCode command ""

    it "prints only the JSON result with --output-format json" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, stderrText) <-
          runPrint executable workspace ["-p", "--output-format", "json"]

        exitCode `shouldBe` ExitFailure 1
        (Aeson.eitherDecode (LBS.pack stdoutText) :: Either String Aeson.Value)
          `shouldSatisfy` isRight
        stderrText `shouldContain` "[Agent Error]"

    it "prints only the formatted result in text mode" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, _) <- runPrint executable workspace ["--print"]

        exitCode `shouldBe` ExitFailure 1
        stdoutText `shouldNotContain` "[Agent Error]"
        stdoutText `shouldNotContain` "Final Answer"
        stdoutText `shouldSatisfy` (not . isPrefixOf "\n")

    it "returns a failure status for a failed --no-tui agent" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        testEnvironment <- isolatedEnvironment workspace
        let command =
              (proc executable ["--no-tui", "--model", "test-model", "Answer in one sentence."])
                { cwd = Just workspace
                , env = Just testEnvironment
                }
        (exitCode, stdoutText, stderrText) <-
          readCreateProcessWithExitCode command ""

        exitCode `shouldBe` ExitFailure 1
        stdoutText `shouldContain` "Agent failed with error:"
        stderrText `shouldBe` ""

    it "returns a failure status for a failed headless goal loop" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        testEnvironment <- isolatedEnvironment workspace
        let command =
              (proc executable
                [ "--print"
                , "--output-format"
                , "json"
                , "--max-turns"
                , "1"
                , "--model"
                , "test-model"
                , "/goal answer in one sentence"
                ])
                { cwd = Just workspace
                , env = Just testEnvironment
                }
        (exitCode, stdoutText, stderrText) <-
          readCreateProcessWithExitCode command ""

        exitCode `shouldBe` ExitFailure 1
        (Aeson.eitherDecode (LBS.pack stdoutText) :: Either String Aeson.Value)
          `shouldSatisfy` isRight
        stderrText `shouldContain` "[Goal] Failed"

  describe "--init (Issue #118)" $ do
    let initEnvironment = do
          environment <- getEnvironment
          pure (filter ((/= "OPENROUTER_API_KEY") . fst) environment)
        runInit executable workspace args = do
          environment <- initEnvironment
          let command =
                (proc executable args)
                  { cwd = Just workspace
                  , env = Just environment
                  }
          readCreateProcessWithExitCode command ""

    it "creates CLAUDE.md and exits cleanly without credentials" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, stderrText) <-
          runInit executable workspace ["--init", "--no-tui"]

        exitCode `shouldBe` ExitSuccess
        stderrText `shouldBe` ""
        stdoutText `shouldContain` "CLAUDE.md"
        doesFileExist (workspace </> "CLAUDE.md") `shouldReturn` True
        TIO.readFile (workspace </> "CLAUDE.md") >>= \content ->
          T.strip content `shouldNotBe` ""

    it "creates CLAUDE.md without --no-tui" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, _stdoutText, stderrText) <-
          runInit executable workspace ["--init"]

        exitCode `shouldBe` ExitSuccess
        stderrText `shouldBe` ""
        doesFileExist (workspace </> "CLAUDE.md") `shouldReturn` True

    it "leaves an existing CLAUDE.md untouched" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        let existing = "# Keep this file\n"
        writeFile (workspace </> "CLAUDE.md") existing
        (exitCode, stdoutText, stderrText) <-
          runInit executable workspace ["--init", "--no-tui"]

        exitCode `shouldBe` ExitSuccess
        stderrText `shouldBe` ""
        stdoutText `shouldContain` "already exists"
        readFile (workspace </> "CLAUDE.md") `shouldReturn` existing

  describe "--help (Issue #153)" $ do
    let runHelp executable workspace args = do
          environment <- getEnvironment
          let command =
                (proc executable args)
                  { cwd = Just workspace
                  , env = Just (filter ((/= "OPENROUTER_API_KEY") . fst) environment)
                  }
          readCreateProcessWithExitCode command ""

    forM_ ["--help", "-h"] $ \flag ->
      it ("prints every option and exits 0 for " <> flag) $ do
        executable <- hachExecutable
        withTemporaryWorkspace $ \workspace -> do
          (exitCode, stdoutText, stderrText) <- runHelp executable workspace [flag]

          exitCode `shouldBe` ExitSuccess
          stderrText `shouldBe` ""
          forM_ ["--model", "--print", "--max-budget-usd", "--add-dir", "--session-id", "--permission-mode"] $
            \option -> stdoutText `shouldContain` option
          listDirectory workspace `shouldReturn` []

    it "points at --help after an argument error" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, _) <- runHelp executable workspace ["--bogus"]

        exitCode `shouldBe` ExitFailure 1
        stdoutText `shouldContain` "Unknown flag: --bogus"
        stdoutText `shouldContain` "hach --help"

  describe "--max-budget-usd enforcement (Issue #150)" $ do
    let runBudget executable workspace args = do
          testEnvironment <- isolatedEnvironment workspace
          let command =
                (proc executable args)
                  { cwd = Just workspace
                  , env = Just testEnvironment
                  }
          readCreateProcessWithExitCode command ""
        namesBudget combined =
          any (`T.isInfixOf` T.toLower (T.pack combined))
            ["budget", "spending limit"]

    it "stops --print --max-budget-usd 0 before any billable request" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, stderrText) <-
          runBudget executable workspace
            ["--print", "--max-budget-usd", "0", "--model", "test-model", "Reply with exactly the word pong"]

        exitCode `shouldBe` ExitFailure 1
        namesBudget (stdoutText <> stderrText) `shouldBe` True
        stdoutText `shouldNotContain` "OpenRouter API error"
        stderrText `shouldNotContain` "OpenRouter API error"
        stdoutText `shouldNotContain` "pong"

    it "reports the budget abort as JSON under --output-format json" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, stderrText) <-
          runBudget executable workspace
            [ "--print"
            , "--output-format"
            , "json"
            , "--max-budget-usd"
            , "0"
            , "--model"
            , "test-model"
            , "Reply with exactly the word pong"
            ]

        exitCode `shouldBe` ExitFailure 1
        stderrText `shouldNotContain` "OpenRouter API error"
        (Aeson.eitherDecode (LBS.pack stdoutText) :: Either String Aeson.Value)
          `shouldSatisfy` isRight
        stdoutText `shouldContain` "max_budget"

    it "honours max_budget_usd from settings.json the same way as the flag" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        createDirectoryIfMissing True (workspace </> ".claude")
        writeFile (workspace </> ".claude" </> "settings.json") "{\"max_budget_usd\": 0}\n"
        (exitCode, stdoutText, stderrText) <-
          runBudget executable workspace
            ["--print", "--model", "test-model", "Reply with exactly the word pong"]

        exitCode `shouldBe` ExitFailure 1
        namesBudget (stdoutText <> stderrText) `shouldBe` True
        stdoutText `shouldNotContain` "OpenRouter API error"

    it "keeps --max-turns aborts distinct when no budget is set" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, stderrText) <-
          runBudget executable workspace
            ["--print", "--max-turns", "1", "--model", "test-model", "Reply with exactly the word pong"]

        exitCode `shouldBe` ExitFailure 1
        namesBudget (stdoutText <> stderrText) `shouldBe` False
        (stdoutText <> stderrText) `shouldContain` "OpenRouter API error"

  describe "unparseable settings files (Issue #165)" $ do
    let runWithSettings executable workspace settings args = do
          createDirectoryIfMissing True (workspace </> ".claude")
          writeFile (workspace </> ".claude" </> "settings.json") settings
          testEnvironment <- isolatedEnvironment workspace
          let command =
                (proc executable args)
                  { cwd = Just workspace
                  , env = Just testEnvironment
                  }
          readCreateProcessWithExitCode command ""
        denyRule =
          "\"permission_rules\":[{\"action\":\"deny\",\"tool\":\"write_file\",\"path\":\"**/secret.txt\"}]"

    forM_
      [ ("a trailing comma", "{" <> denyRule <> ",}\n")
      , ("a stray closing brace", "{" <> denyRule <> "}}\n")
      , ("a field of the wrong shape", "{\"permission_rules\":7}\n")
      ] $ \(label, settings) ->
      it ("refuses to start when settings.json has " <> label) $ do
        executable <- hachExecutable
        withTemporaryWorkspace $ \workspace -> do
          (exitCode, stdoutText, stderrText) <-
            runWithSettings executable workspace settings
              ["--no-tui", "--permission-mode", "acceptEdits", "Write CHANGED to secret.txt"]
          let combined = stdoutText <> stderrText

          exitCode `shouldBe` ExitFailure 1
          combined `shouldContain` (".claude" </> "settings.json")
          combined `shouldNotContain` "Starting agent loop"
          combined `shouldNotContain` "OpenRouter API error"

    it "still starts when the settings file parses" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, stderrText) <-
          runWithSettings executable workspace ("{" <> denyRule <> "}\n")
            ["--no-tui", "--max-turns", "1", "--model", "test-model", "Write CHANGED to secret.txt"]
        let combined = stdoutText <> stderrText

        exitCode `shouldBe` ExitFailure 1
        combined `shouldNotContain` "settings.json"
        combined `shouldContain` "Starting agent loop"

  describe "session persistence and continuation (Issue #151)" $ do
    let runWithEnv executable workspace args = do
          testEnvironment <- isolatedEnvironment workspace
          let command =
                (proc executable args)
                  { cwd = Just workspace
                  , env = Just testEnvironment
                  }
          readCreateProcessWithExitCode command ""

    it "fails with an explicit error when --continue is used without stored sessions" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, _stderrText) <-
          runWithEnv executable workspace ["--no-tui", "-c", "continue previous task"]
        exitCode `shouldBe` ExitFailure 1
        stdoutText `shouldContain` "No stored session found"
        stdoutText `shouldNotContain` "Prompting LLM"

    it "fails with an explicit error when --session-id is used with nonexistent id" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        (exitCode, stdoutText, _stderrText) <-
          runWithEnv executable workspace ["--no-tui", "--session-id", "nonexistent-sess-id", "do task"]
        exitCode `shouldBe` ExitFailure 1
        stdoutText `shouldContain` "No stored session found"
        stdoutText `shouldNotContain` "Prompting LLM"

    it "restores prior history when continuing a session via -c" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        let sessionsDir = workspace </> ".agents" </> "sessions"
        createDirectoryIfMissing True sessionsDir
        writeFile (sessionsDir </> "sess-1.meta.json")
          "{\"id\":\"sess-1\",\"created_at\":\"2026-09-18T09:00:00Z\",\"model\":\"test-model\",\"turns\":1,\"cost_usd\":0.0}"
        writeFile (sessionsDir </> "sess-1.jsonl")
          "{\"role\":\"user\",\"content\":\"Codeword PLATYPUS\"}\n{\"role\":\"assistant\",\"content\":\"Acknowledged.\"}\n"

        (_exitCode, stdoutText, _) <-
          runWithEnv executable workspace ["--no-tui", "-c", "What was the codeword?"]
        stdoutText `shouldContain` "Prompting LLM with 4 messages in context"

    it "restores specific prior history when resumed via --session-id" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        let sessionsDir = workspace </> ".agents" </> "sessions"
        createDirectoryIfMissing True sessionsDir
        writeFile (sessionsDir </> "sess-specific.meta.json")
          "{\"id\":\"sess-specific\",\"created_at\":\"2026-09-18T09:00:00Z\",\"model\":\"test-model\",\"turns\":1,\"cost_usd\":0.0}"
        writeFile (sessionsDir </> "sess-specific.jsonl")
          "{\"role\":\"user\",\"content\":\"Codeword PLATYPUS\"}\n{\"role\":\"assistant\",\"content\":\"Acknowledged.\"}\n"

        (_exitCode, stdoutText, _) <-
          runWithEnv executable workspace ["--no-tui", "--session-id", "sess-specific", "What was the codeword?"]
        stdoutText `shouldContain` "Prompting LLM with 4 messages in context"

  -- The stub credentials make every model request fail, so these runs also
  -- show that the lifecycle hooks fire on a run that ends in failure.
  describe "lifecycle hooks (Issue #168)" $ do
    let recordingHook event =
          "{\"handler\":{\"type\":\"command\",\"command\":\"{ printf '"
            <> event <> " '; cat; echo; } >> hooks.log\"}}"
        hooksSettings entries =
          "{\"hooks\":[" <> intercalate "," entries <> "]}\n"
        entry event handlers = "[\"" <> event <> "\",[" <> intercalate "," handlers <> "]]"
        runWithHooks executable workspace settings args = do
          createDirectoryIfMissing True (workspace </> ".claude")
          writeFile (workspace </> ".claude" </> "settings.json") settings
          testEnvironment <- isolatedEnvironment workspace
          let command =
                (proc executable args)
                  { cwd = Just workspace
                  , env = Just testEnvironment
                  }
          readCreateProcessWithExitCode command ""
        hookLog workspace = lines <$> readFile (workspace </> "hooks.log")
        eventOf = takeWhile (/= ' ')
        payloadOf = drop 1 . dropWhile (/= ' ')

    it "runs session_start, user_prompt_submit and stop hooks in order" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        let settings = hooksSettings
              [ entry "session_start" [recordingHook "session_start"]
              , entry "user_prompt_submit" [recordingHook "user_prompt_submit"]
              , entry "stop" [recordingHook "stop"]
              ]
        _ <- runWithHooks executable workspace settings
          ["--no-tui", "--max-turns", "1", "--model", "test-model", "Say hello world"]
        entries <- hookLog workspace

        map eventOf entries `shouldBe` ["session_start", "user_prompt_submit", "stop"]
        let payloads = map (Aeson.decode . LBS.pack . payloadOf) entries :: [Maybe Aeson.Value]
        payloads `shouldSatisfy` all (/= Nothing)
        (entries !! 1) `shouldContain` "\"prompt\":\"Say hello world\""

    it "does not reach the model when a user_prompt_submit hook blocks the prompt" $ do
      executable <- hachExecutable
      withTemporaryWorkspace $ \workspace -> do
        let blocking =
              "{\"handler\":{\"type\":\"command\",\"command\":\"echo prompt rejected by policy; exit 2\"}}"
            settings = hooksSettings [entry "user_prompt_submit" [blocking]]
        (exitCode, stdoutText, stderrText) <- runWithHooks executable workspace settings
          ["--no-tui", "--model", "test-model", "Say hello world"]
        let combined = stdoutText <> stderrText

        exitCode `shouldBe` ExitFailure 1
        combined `shouldContain` "prompt rejected by policy"
        combined `shouldNotContain` "OpenRouter API error"

-- | Process environment for a spawned hach: stub credentials, and a user
-- settings layer pointed at a directory that does not exist, so the
-- developer's own ~/.claude/settings.json cannot reach the run.
isolatedEnvironment :: FilePath -> IO [(String, String)]
isolatedEnvironment workspace = do
  environment <- getEnvironment
  pure $
    ("OPENROUTER_API_KEY", "test")
      : ("OPENROUTER_MODEL", "test-model")
      : ("CLAUDE_CONFIG_DIR", workspace <> "-isolated-config")
      : filter ((`notElem` overridden) . fst) environment
  where
    overridden = ["OPENROUTER_API_KEY", "OPENROUTER_MODEL", "CLAUDE_CONFIG_DIR"]

hachExecutable :: IO FilePath
hachExecutable = do
  output <- readProcess "cabal" ["list-bin", "exe:hach"] ""
  pure (T.unpack (T.strip (T.pack output)))

withTemporaryWorkspace :: (FilePath -> IO a) -> IO a
withTemporaryWorkspace action = do
  temporaryDirectory <- getTemporaryDirectory
  (workspace, handle) <- openTempFile temporaryDirectory "hach-cli-test"
  hClose handle
  removeFile workspace
  createDirectory workspace
  action workspace `finally` removeDirectoryRecursive workspace

withTemporaryGitWorkspace :: (FilePath -> IO a) -> IO a
withTemporaryGitWorkspace action =
  withTemporaryWorkspace $ \workspace -> do
    canonicalWorkspace <- canonicalizePath workspace
    callProcess "git" ["-C", canonicalWorkspace, "init", "-q"]
    callProcess "git" ["-C", canonicalWorkspace, "config", "user.name", "Hach Test"]
    callProcess "git" ["-C", canonicalWorkspace, "config", "user.email", "hach-test@example.invalid"]
    callProcess "git" ["-C", canonicalWorkspace, "commit", "--allow-empty", "-q", "-m", "init"]
    action canonicalWorkspace
