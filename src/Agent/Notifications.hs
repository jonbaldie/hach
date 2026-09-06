{-# LANGUAGE OverloadedStrings #-}

module Agent.Notifications
  ( sendOsNotification
  , sendDesktopNotification
  ) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode)
import System.Info (os)
import System.Process (readCreateProcessWithExitCode, shell)

-- | Dispatch an OS desktop notification.
sendOsNotification :: Text -> IO ()
sendOsNotification msg = do
  _ <- sendDesktopNotification "Agent" msg
  pure ()

-- | Dispatch an OS desktop notification with title and message.
sendDesktopNotification :: Text -> Text -> IO (Maybe Text)
sendDesktopNotification title msg = do
  let cleanTitle = T.replace "\"" "\\\"" (T.take 100 title)
      cleanMsg = T.replace "\"" "\\\"" (T.take 200 msg)
      cmd = if os == "darwin"
        then "osascript -e 'display notification \"" ++ T.unpack cleanMsg ++ "\" with title \"" ++ T.unpack cleanTitle ++ "\"'"
        else "notify-send \"" ++ T.unpack cleanTitle ++ "\" \"" ++ T.unpack cleanMsg ++ "\""
  res <- try (readCreateProcessWithExitCode (shell cmd) "") :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Left ex -> pure (Just (T.pack (show ex)))
    Right _ -> pure Nothing

