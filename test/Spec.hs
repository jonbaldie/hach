{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Agent.CoreSpec as CoreSpec
import qualified Agent.EnvSpec as EnvSpec
import qualified Agent.OpenRouterSpec as OpenRouterSpec
import qualified Agent.ToolsSpec as ToolsSpec

main :: IO ()
main = hspec $ do
  describe "Agent.Env" EnvSpec.spec
  describe "Agent.Core (Functional Pearl)" CoreSpec.spec
  describe "Agent.Tools" ToolsSpec.spec
  describe "Agent.OpenRouter" OpenRouterSpec.spec
