{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Hach.CoreSpec as CoreSpec
import qualified Hach.EnvSpec as EnvSpec
import qualified Hach.OpenRouterSpec as OpenRouterSpec
import qualified Hach.ToolsSpec as ToolsSpec
import qualified Hach.TUISpec as TUISpec
import qualified Hach.TUILayoutSpec as TUILayoutSpec
import qualified Hach.PropertySpec as PropertySpec
import qualified Hach.GoalFuzzSpec as GoalFuzzSpec
import qualified Hach.CampaignSpec as CampaignSpec
import qualified Hach.Campaign2Spec as Campaign2Spec
import qualified Hach.Campaign3Spec as Campaign3Spec
import qualified Hach.PerfFuzzSpec as PerfFuzzSpec
import qualified Hach.SkillsSpec as SkillsSpec
import qualified Hach.PermissionsSpec as PermissionsSpec
import qualified Hach.SettingsSpec as SettingsSpec
import qualified Hach.SessionsSpec as SessionsSpec
import qualified Hach.HooksSpec as HooksSpec
import qualified Hach.SubagentsSpec as SubagentsSpec
import qualified Hach.MCPSpec as MCPSpec
import qualified Hach.MemorySpec as MemorySpec
import qualified Hach.GitSpec as GitSpec
import qualified Hach.TasksSpec as TasksSpec
import qualified Hach.NotificationsSpec as NotificationsSpec
import qualified Hach.InterpreterIOSpec as InterpreterIOSpec
import qualified Hach.CLISpec as CLISpec

main :: IO ()
main = hspec $ do
  describe "Hach.Env" EnvSpec.spec
  describe "Hach.Core" CoreSpec.spec
  describe "Hach.Tools" ToolsSpec.spec
  describe "Hach.OpenRouter" OpenRouterSpec.spec
  describe "Hach.TUI (Pure Reducer Seam)" TUISpec.spec
  describe "Hach.TUI (Layout / Border Alignment)" TUILayoutSpec.spec
  describe "Hach.Property (CGPT & Fuzzing)" PropertySpec.spec
  describe "Hach.GoalFuzz (CGPT /goal campaign)" GoalFuzzSpec.spec
  describe "Hach.Campaign (CGPT coverage-guided campaign)" CampaignSpec.spec
  describe "Hach.Campaign2 (CGPT wave-2 containment & identity)" Campaign2Spec.spec
  describe "Hach.Campaign3 (CGPT wave-3 cost, completion, TUI)" Campaign3Spec.spec
  describe "Hach.PerfFuzz (performance-feedback campaign)" PerfFuzzSpec.spec
  describe "Hach.Skills" SkillsSpec.spec
  describe "Hach.Permissions" PermissionsSpec.spec
  describe "Hach.Settings" SettingsSpec.spec
  describe "Hach.Sessions" SessionsSpec.spec
  describe "Hach.Hooks" HooksSpec.spec
  describe "Hach.Subagents" SubagentsSpec.spec
  describe "Hach.MCP" MCPSpec.spec
  describe "Hach.Memory" MemorySpec.spec
  describe "Hach.Git" GitSpec.spec
  describe "Hach.Tasks" TasksSpec.spec
  describe "Hach.Notifications" NotificationsSpec.spec
  describe "Hach.Interpreter.IO" InterpreterIOSpec.spec
  describe "Hach CLI" CLISpec.spec
