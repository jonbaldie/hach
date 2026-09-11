module Hach.CLISpec (spec) where

import Control.Exception (finally)
import System.Directory
  ( createDirectory
  , canonicalizePath
  , doesDirectoryExist
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
