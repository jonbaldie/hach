{-# LANGUAGE OverloadedStrings #-}

module Agent.NotificationsSpec (spec) where

import Agent.Notifications
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.Process (CmdSpec(..), CreateProcess(..))
import Test.Hspec

spec :: Spec
spec = describe "Agent.Notifications" $ do
  describe "sendDesktopNotification security & escaping" $ do
    it "uses direct process execution (RawCommand) rather than shell to prevent injection" $ do
      let procSpec = notificationCreateProcess "Agent" "Notification body"
      case cmdspec procSpec of
        RawCommand _ _ -> pure ()
        ShellCommand _ -> expectationFailure "Expected RawCommand (proc) but got ShellCommand"

    it "safely handles apostrophes in notification message without shell failure or injection" $ do
      let marker = "/tmp/agent-test-notif-injection"
          payload = "Don't panic; touch " <> T.pack marker <> "; echo 'ok"
      _ <- sendDesktopNotification "Agent" payload
      injected <- doesFileExist marker
      if injected then removeFile marker else pure ()
      injected `shouldBe` False
