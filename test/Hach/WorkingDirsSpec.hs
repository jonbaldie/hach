{-# LANGUAGE OverloadedStrings #-}

-- | Additional working directories (@--add-dir@ / @working_directories@).
--
-- Covers the whole chain the CLI walks: flag parsing, resolution against
-- layered settings, and the tool runtime that has to treat the extra roots
-- as inside the workspace boundary.
module Hach.WorkingDirsSpec (spec) where

import Hach.Core (AgentAlgebra (..))
import Hach.CLI (CliOptions (..), parseCliArgs)
import Hach.Env (resolveWorkingDirs)
import Hach.Interpreter.IO
  ( IOEnv (..)
  , IOEnvPermissions (..)
  , currentIOWorkspaceScope
  , defaultIOEnvPermissions
  , ioAlgebraWithLog
  , newIOEnvWithPermissions
  )
import Hach.Paths (Workspace (..), workspaceAt)
import Hach.Settings (Settings (..), defaultSettings)
import Hach.Tools
  ( ReadFileArgs (..)
  , WriteFileArgs (..)
  , executeReadFile
  , executeWriteFile
  )
import Hach.Types
import Control.Exception (SomeException, bracket, try)
import qualified Data.Text as T
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import Test.Hspec

-- | A primary workspace root, an unrelated sibling directory, and a third
-- directory that is never added. Both extras sit outside the primary root.
data Sandbox = Sandbox
  { sbRoot    :: FilePath
  , sbExtra   :: FilePath
  , sbOutside :: FilePath
  }

withSandbox :: (Sandbox -> IO a) -> IO a
withSandbox action = do
  tmp <- getTemporaryDirectory
  let base = tmp </> "hach-working-dirs-spec"
  bracket (setup base) (const (discard base)) action
  where
    setup base = do
      discard base
      let sandbox = Sandbox (base </> "primary") (base </> "extra") (base </> "outside")
      mapM_ (createDirectoryIfMissing True)
        [sbRoot sandbox, sbExtra sandbox, sbOutside sandbox, sbExtra sandbox </> ".git"]
      writeFile (sbRoot sandbox </> "inside.txt") "INSIDE"
      writeFile (sbExtra sandbox </> "note.txt") "SECRET-OUTSIDE-FILE"
      writeFile (sbExtra sandbox </> ".git" </> "config") "[core]"
      writeFile (sbOutside sandbox </> "note.txt") "UNREACHABLE"
      pure sandbox
    discard base = do
      res <- try (removeDirectoryRecursive base) :: IO (Either SomeException ())
      either (const (pure ())) pure res

spec :: Spec
spec = do
  describe "resolveWorkingDirs" $ do
    it "accumulates --add-dir flags and the working_directories setting" $ do
      let settings = defaultSettings { setWorkingDirs = ["/srv/shared"] }
      resolveWorkingDirs ["/tmp/extra"] settings
        `shouldBe` ["/tmp/extra", "/srv/shared"]

    it "treats working_directories on its own like a repeated --add-dir" $ do
      let settings = defaultSettings { setWorkingDirs = ["/tmp/extra", "/srv/shared"] }
      resolveWorkingDirs [] settings
        `shouldBe` resolveWorkingDirs ["/tmp/extra", "/srv/shared"] defaultSettings

    it "keeps a directory named by both the flag and the setting only once" $ do
      let settings = defaultSettings { setWorkingDirs = ["/tmp/extra"] }
      resolveWorkingDirs ["/tmp/extra"] settings `shouldBe` ["/tmp/extra"]

    it "is empty when neither the flag nor the setting names a directory" $
      resolveWorkingDirs [] defaultSettings `shouldBe` []

  describe "--add-dir parsing" $
    it "collects every repetition of the flag" $ do
      let parsed = parseCliArgs ["--add-dir", "/tmp/a", "--add-dir=/tmp/b", "do", "it"]
      fmap optAddDir parsed `shouldBe` Right ["/tmp/a", "/tmp/b"]

  describe "workspace containment" $ do
    it "reads a file under an added directory" $ withSandbox $ \Sandbox{..} -> do
      res <- executeReadFile (Workspace sbRoot [sbExtra]) (ReadFileArgs (sbExtra </> "note.txt"))
      res `shouldBe` ToolSuccess "SECRET-OUTSIDE-FILE"

    it "still rejects a file outside every configured root" $ withSandbox $ \Sandbox{..} -> do
      let target = sbOutside </> "note.txt"
      res <- executeReadFile (Workspace sbRoot [sbExtra]) (ReadFileArgs target)
      res `shouldBe` ToolError
        ("Access denied: path '" <> T.pack target <> "' escapes the workspace root.")

    it "rejects the same file when no directory was added" $ withSandbox $ \Sandbox{..} -> do
      let target = sbExtra </> "note.txt"
      res <- executeReadFile (workspaceAt sbRoot) (ReadFileArgs target)
      res `shouldBe` ToolError
        ("Access denied: path '" <> T.pack target <> "' escapes the workspace root.")

    it "resolves relative paths against the primary root, not an added one" $
      withSandbox $ \Sandbox{..} -> do
        res <- executeReadFile (Workspace sbRoot [sbExtra]) (ReadFileArgs "inside.txt")
        res `shouldBe` ToolSuccess "INSIDE"

    it "keeps protected paths protected inside an added directory" $
      withSandbox $ \Sandbox{..} -> do
        let target = sbExtra </> ".git" </> "pwned"
        res <- executeWriteFile (Workspace sbRoot [sbExtra]) (WriteFileArgs target "pwned")
        res `shouldBe` ToolError ("Protected path: write denied to " <> T.pack target)
        doesFileExist target `shouldReturn` False

    it "writes to a non-protected path inside an added directory" $
      withSandbox $ \Sandbox{..} -> do
        let target = sbExtra </> "written.txt"
        res <- executeWriteFile (Workspace sbRoot [sbExtra]) (WriteFileArgs target "ok")
        res `shouldBe` ToolSuccess ("Successfully wrote 2 characters to " <> T.pack target)
        readFile target `shouldReturn` "ok"

  describe "IO interpreter wiring" $ do
    it "hands the session's working directories to the tool runtime" $
      withSandbox $ \Sandbox{..} -> do
        env <- workingDirsEnv sbRoot [sbExtra]
        scope <- currentIOWorkspaceScope env
        canonRoot <- canonicalizePath sbRoot
        canonScope <- canonicalizePath (wsRoot scope)
        canonScope `shouldBe` canonRoot
        wsExtraRoots scope `shouldBe` [sbExtra]

    it "executes read_file against an added directory through interpTool" $
      withSandbox $ \Sandbox{..} -> do
        env <- workingDirsEnv sbRoot [sbExtra]
        let algebra = ioAlgebraWithLog (const (pure ())) env
            call = ToolCall "call-1" "read_file"
              (T.pack ("{\"path\":\"" <> sbExtra </> "note.txt" <> "\"}"))
        res <- interpTool algebra call
        res `shouldBe` ToolSuccess "SECRET-OUTSIDE-FILE"

    it "denies a directory that was never added, through interpTool" $
      withSandbox $ \Sandbox{..} -> do
        env <- workingDirsEnv sbRoot []
        let algebra = ioAlgebraWithLog (const (pure ())) env
            target = sbExtra </> "note.txt"
            call = ToolCall "call-2" "read_file"
              (T.pack ("{\"path\":\"" <> target <> "\"}"))
        res <- interpTool algebra call
        res `shouldBe` ToolError
          ("Access denied: path '" <> T.pack target <> "' escapes the workspace root.")

workingDirsEnv :: FilePath -> [FilePath] -> IO IOEnv
workingDirsEnv root extras = do
  let perms = defaultIOEnvPermissions { iopInitialMode = ModeBypassPermissions }
  env <- newIOEnvWithPermissions perms "test-key" "test-model" root False
  pure env { ioWorkingDirs = extras }
