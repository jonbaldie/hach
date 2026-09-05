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

main :: IO ()
main = hspec $ do
  describe "Agent.Env" EnvSpec.spec
  describe "Agent.Core (Functional Pearl)" CoreSpec.spec
  describe "Agent.Tools" ToolsSpec.spec
  describe "Agent.OpenRouter" OpenRouterSpec.spec
  describe "Agent.TUI (Pure Reducer Seam)" TUISpec.spec
  describe "Agent.Property (CGPT & Fuzzing)" PropertySpec.spec
  describe "Agent.Skills" SkillsSpec.spec
