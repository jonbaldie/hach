{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Agent.CoreSpec as CoreSpec
import qualified Agent.EnvSpec as EnvSpec
import qualified Agent.OpenRouterSpec as OpenRouterSpec
import qualified Agent.ToolsSpec as ToolsSpec
import qualified Agent.TUISpec as TUISpec
import qualified Agent.PropertySpec as PropertySpec
import qualified Agent.SkillsSpec as SkillsSpec
import qualified Agent.PermissionsSpec as PermissionsSpec
import qualified Agent.SettingsSpec as SettingsSpec
import qualified Agent.SessionsSpec as SessionsSpec
import qualified Agent.HooksSpec as HooksSpec
import qualified Agent.SubagentsSpec as SubagentsSpec
import qualified Agent.MCPSpec as MCPSpec
import qualified Agent.MemorySpec as MemorySpec
import qualified Agent.GitSpec as GitSpec
import qualified Agent.TasksSpec as TasksSpec

main :: IO ()
main = hspec $ do
  describe "Agent.Env" EnvSpec.spec
  describe "Agent.Core (Functional Pearl)" CoreSpec.spec
  describe "Agent.Tools" ToolsSpec.spec
  describe "Agent.OpenRouter" OpenRouterSpec.spec
  describe "Agent.TUI (Pure Reducer Seam)" TUISpec.spec
  describe "Agent.Property (CGPT & Fuzzing)" PropertySpec.spec
  describe "Agent.Skills" SkillsSpec.spec
  describe "Agent.Permissions" PermissionsSpec.spec
  describe "Agent.Settings" SettingsSpec.spec
  describe "Agent.Sessions" SessionsSpec.spec
  describe "Agent.Hooks" HooksSpec.spec
  describe "Agent.Subagents" SubagentsSpec.spec
  describe "Agent.MCP" MCPSpec.spec
  describe "Agent.Memory" MemorySpec.spec
  describe "Agent.Git" GitSpec.spec
  describe "Agent.Tasks" TasksSpec.spec
