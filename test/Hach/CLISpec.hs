module Hach.CLISpec (spec) where

import Control.Exception (finally)
import System.Directory
  ( createDirectory
  , getTemporaryDirectory
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process
  ( CreateProcess (..)
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
