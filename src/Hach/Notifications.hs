{-# LANGUAGE OverloadedStrings #-}

module Hach.Notifications
  ( sendOsNotification
  , sendDesktopNotification
  , notificationCreateProcess
  ) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Info (os)
import System.Process (CreateProcess, proc, readCreateProcessWithExitCode)

-- | Construct the process specification for an OS desktop notification.
-- Uses direct executable dispatch (proc) rather than shell execution to prevent
-- command injection.
notificationCreateProcess :: Text -> Text -> CreateProcess
notificationCreateProcess title msg =
  let cleanTitle = escapeAppleScript (T.take 100 title)
      cleanMsg   = escapeAppleScript (T.take 200 msg)
  in if os == "darwin"
       then
         let script = "display notification \"" ++ T.unpack cleanMsg ++ "\" with title \"" ++ T.unpack cleanTitle ++ "\""
         in proc "osascript" ["-e", script]
       else
         proc "notify-send" [T.unpack (T.take 100 title), T.unpack (T.take 200 msg)]
  where
    -- In AppleScript string literals: backslash and double quotes are escaped with backslash.
    escapeAppleScript t =
      T.replace "\\" "\\\\" (T.replace "\"" "\\\"" t)

-- | Dispatch an OS desktop notification.
sendOsNotification :: Text -> IO ()
sendOsNotification msg = do
  _ <- sendDesktopNotification "Agent" msg
  pure ()

-- | Dispatch an OS desktop notification with title and message.
sendDesktopNotification :: Text -> Text -> IO (Maybe Text)
sendDesktopNotification title msg = do
  let procSpec = notificationCreateProcess title msg
  res <- try (readCreateProcessWithExitCode procSpec "") :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Left ex -> pure (Just (T.pack (show ex)))
    Right (ExitSuccess, _, _) -> pure Nothing
    Right (ExitFailure _, _, err) -> pure (Just (if null err then "Notification command failed" else T.pack err))
