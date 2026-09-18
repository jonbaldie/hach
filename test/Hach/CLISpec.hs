module Hach.CLISpec (spec) where

import Control.Exception (finally)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isRight)
import Data.List (isPrefixOf)
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
      environment <- getEnvironment
      let testEnvironment =
            ("OPENROUTER_API_KEY", "test")
              : filter ((/= "OPENROUTER_API_KEY") . fst) environment
          command =
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
            environment <- getEnvironment
            let testEnvironment =
                  ("OPENROUTER_API_KEY", "test")
                    : ("OPENROUTER_MODEL", "test-model")
                    : filter ((/= "OPENROUTER_API_KEY") . fst)
                        (filter ((/= "OPENROUTER_MODEL") . fst) environment)
                command =
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
            environment <- getEnvironment
            let testEnvironment =
                  ("OPENROUTER_API_KEY", "test")
                    : ("OPENROUTER_MODEL", "test-model")
                    : filter ((/= "OPENROUTER_API_KEY") . fst)
                        (filter ((/= "OPENROUTER_MODEL") . fst) environment)
                command =
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


  describe "--print stdout contract (Issue #116)" $ do
    -- An invalid key makes the agent fail on its first request, which drives
    -- agent events through the real renderer without a live model.
    let runPrint executable workspace args = do
          environment <- getEnvironment
          let testEnvironment =
                ("OPENROUTER_API_KEY", "test")
                  : filter ((/= "OPENROUTER_API_KEY") . fst) environment
              command =
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
        environment <- getEnvironment
        let testEnvironment =
              ("OPENROUTER_API_KEY", "test")
                : filter ((/= "OPENROUTER_API_KEY") . fst) environment
            command =
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
        environment <- getEnvironment
        let testEnvironment =
              ("OPENROUTER_API_KEY", "test")
                : filter ((/= "OPENROUTER_API_KEY") . fst) environment
            command =
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

  describe "session persistence and continuation (Issue #151)" $ do
    let runWithEnv executable workspace args = do
          environment <- getEnvironment
          let testEnvironment =
                ("OPENROUTER_API_KEY", "test")
                  : ("OPENROUTER_MODEL", "test-model")
                  : filter ((`notElem` ["OPENROUTER_API_KEY", "OPENROUTER_MODEL"]) . fst) environment
              command =
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
