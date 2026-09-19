{-# LANGUAGE OverloadedStrings #-}

module Hach.Clipboard
  ( copyToClipboard
  , clipboardCommands
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.IO (IOMode(..), hClose, hSetBinaryMode, withFile)
import System.Process
  ( CreateProcess(..)
  , StdStream(..)
  , proc
  , waitForProcess
  , withCreateProcess
  )

-- | Clipboard writers for this platform, in order of preference.
clipboardCommands :: [(FilePath, [String])]
clipboardCommands
  | os == "darwin" = [("pbcopy", [])]
  | otherwise =
      [ ("wl-copy", [])
      , ("xclip", ["-selection", "clipboard"])
      , ("xsel", ["--clipboard", "--input"])
      ]

-- | Write text to the system clipboard with the first installed clipboard
-- writer, feeding it UTF-8 bytes on stdin.
copyToClipboard :: Text -> IO (Either Text ())
copyToClipboard text = do
  installed <- traverse locate clipboardCommands
  case listToMaybe [ (exe, args) | Just (exe, args) <- installed ] of
    Nothing ->
      pure (Left ("no clipboard tool found (tried " <> T.intercalate ", " (map (T.pack . fst) clipboardCommands) <> ")"))
    Just (exe, args) -> do
      res <- try (writeTo exe args) :: IO (Either SomeException ExitCode)
      pure $ case res of
        Right ExitSuccess     -> Right ()
        Right (ExitFailure n) -> Left (T.pack exe <> " exited with status " <> T.pack (show n))
        Left ex               -> Left (T.pack (show ex))
  where
    locate (name, args) = fmap (\exe -> (exe, args)) <$> findExecutable name

    -- Writers such as xclip fork to keep serving the selection, so their
    -- output goes to the null device: reading it would wait on the forked
    -- child, and inheriting it would draw over the TUI.
    writeTo exe args =
      withFile "/dev/null" WriteMode $ \devNull ->
        withCreateProcess (proc exe args) { std_in = CreatePipe, std_out = UseHandle devNull, std_err = UseHandle devNull } $
          \mIn _ _ ph -> do
            mapM_ (\h -> hSetBinaryMode h True >> BS.hPut h (TE.encodeUtf8 text) >> hClose h) mIn
            waitForProcess ph
