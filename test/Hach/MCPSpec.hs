{-# LANGUAGE OverloadedStrings #-}

module Hach.MCPSpec (spec) where

import Hach.MCP
import Hach.Types
import Data.Aeson (decode, object, (.=))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = describe "Hach.MCP" $ do
  describe "Tool naming conventions" $ do
    it "formats MCP tool name with double underscore prefix" $ do
      formatMcpToolName "sqlite" "query" `shouldBe` "mcp__sqlite__query"

    it "parses qualified MCP tool name back into server and tool" $ do
      parseMcpToolName "mcp__sqlite__query" `shouldBe` Just ("sqlite", "query")

    it "rejects non-MCP tool names" $ do
      parseMcpToolName "read_file" `shouldBe` Nothing
      parseMcpToolName "mcp_single_underscore" `shouldBe` Nothing

  describe ".mcp.json parsing" $ do
    it "parses stdio server configuration" $ do
      let raw = "{\n\
        \  \"mcpServers\": {\n\
        \    \"filesystem\": {\n\
        \      \"command\": \"npx\",\n\
        \      \"args\": [\"-y\", \"@modelcontextprotocol/server-filesystem\", \"/tmp\"]\n\
        \    }\n\
        \  }\n\
        \}"
      case decode (BSL.fromStrict raw) of
        Nothing -> expectationFailure "Failed to parse .mcp.json"
        Just cfg -> do
          case Map.lookup "filesystem" (mcfServers cfg) of
            Nothing -> expectationFailure "Expected 'filesystem' server in config"
            Just srv -> do
              mscName srv `shouldBe` "filesystem"
              case mscTransport srv of
                McpStdio cmd args _env -> do
                  cmd `shouldBe` "npx"
                  args `shouldBe` ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
                other -> expectationFailure ("Expected McpStdio, got " <> show other)

    it "parses HTTP/SSE server configuration" $ do
      let raw = "{\n\
        \  \"mcpServers\": {\n\
        \    \"remote\": {\n\
        \      \"url\": \"https://mcp.example.com/sse\"\n\
        \    }\n\
        \  }\n\
        \}"
      case decode (BSL.fromStrict raw) of
        Nothing -> expectationFailure "Failed to parse SSE .mcp.json"
        Just cfg -> do
          case Map.lookup "remote" (mcfServers cfg) of
            Nothing -> expectationFailure "Expected 'remote' server"
            Just srv -> mscTransport srv `shouldBe` McpSse "https://mcp.example.com/sse"
