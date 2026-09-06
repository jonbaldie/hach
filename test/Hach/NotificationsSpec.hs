{-# LANGUAGE OverloadedStrings #-}

module Hach.NotificationsSpec (spec) where

import Hach.Notifications
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.Info (os)
import System.Process (CmdSpec(..), CreateProcess(..))
import Test.Hspec

spec :: Spec
spec = describe "Hach.Notifications" $ do
  describe "sendDesktopNotification security & escaping" $ do
    it "uses direct process execution (RawCommand) rather than shell to prevent injection" $ do
      let procSpec = notificationCreateProcess "Hach" "Notification body"
      case cmdspec procSpec of
        RawCommand _ _ -> pure ()
        ShellCommand _ -> expectationFailure "Expected RawCommand (proc) but got ShellCommand"

    it "safely handles apostrophes in notification message without shell failure or injection" $ do
      let marker = "/tmp/agent-test-notif-injection"
          payload = "Don't panic; touch " <> T.pack marker <> "; echo 'ok"
      _ <- sendDesktopNotification "Hach" payload
      injected <- doesFileExist marker
      if injected then removeFile marker else pure ()
      injected `shouldBe` False

    it "correctly escapes double quotes without doubling backslashes in AppleScript" $ do
      escapeAppleScript "He said \"hello\"" `shouldBe` "He said \\\"hello\\\""
      escapeAppleScript "C:\\path\\file" `shouldBe` "C:\\\\path\\\\file"
      escapeAppleScript "Backslash \\ and \"quote\"" `shouldBe` "Backslash \\\\ and \\\"quote\\\""
      let procSpec = notificationCreateProcess "Hach" "He said \"hello\""
      case cmdspec procSpec of
        RawCommand prog args ->
          if os == "darwin"
            then do
              prog `shouldBe` "osascript"
              args `shouldContain` ["display notification \"He said \\\"hello\\\"\" with title \"Hach\""]
            else do
              prog `shouldBe` "notify-send"
              args `shouldBe` ["Hach", "He said \"hello\""]
        ShellCommand _ -> expectationFailure "Expected RawCommand (proc) but got ShellCommand"


