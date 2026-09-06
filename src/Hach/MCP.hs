{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Hach.MCP
  ( McpTransport(..)
  , McpServerConfig(..)
  , McpConfigFile(..)
  , formatMcpToolName
  , parseMcpToolName
  , loadMcpConfig
  , searchMcpTools
  ) where

import Hach.Types
import Control.Monad (guard)
import Control.Exception (SomeException, try)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.:?), (.!=), object, (.=), withObject
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Supported transport mechanisms for talking to MCP servers.
data McpTransport
  = McpStdio !FilePath ![Text] !(Map Text Text)
  | McpSse !Text
  | McpHttp !Text
  | McpWs !Text
  deriving (Show, Eq, Generic)

instance ToJSON McpTransport where
  toJSON = \case
    McpStdio cmd args env -> object
      [ "command" .= cmd
      , "args"    .= args
      , "env"     .= env
      ]
    McpSse url  -> object ["url" .= url, "type" .= ("sse" :: Text)]
    McpHttp url -> object ["url" .= url, "type" .= ("http" :: Text)]
    McpWs url   -> object ["url" .= url, "type" .= ("ws" :: Text)]

instance FromJSON McpTransport where
  parseJSON = withObject "McpTransport" $ \o -> do
    mCmd <- o .:? "command"
    mUrl <- o .:? "url"
    mType <- o .:? "type"
    case (mCmd, mUrl) of
      (Just cmd, _) -> do
        args <- o .:? "args" .!= []
        env  <- o .:? "env"  .!= Map.empty
        pure (McpStdio cmd args env)
      (_, Just url) ->
        case (mType :: Maybe Text) of
          Just "ws"   -> pure (McpWs url)
          Just "http" -> pure (McpHttp url)
          _           -> pure (McpSse url)
      _ -> fail "McpTransport requires either 'command' or 'url'"

data McpServerConfig = McpServerConfig
  { mscName      :: !Text
  , mscTransport :: !McpTransport
  } deriving (Show, Eq, Generic)

instance ToJSON McpServerConfig where
  toJSON McpServerConfig{..} = toJSON mscTransport

newtype McpConfigFile = McpConfigFile
  { mcfServers :: Map Text McpServerConfig
  } deriving (Show, Eq, Generic)

instance ToJSON McpConfigFile where
  toJSON McpConfigFile{..} = object ["mcpServers" .= mcfServers]

instance FromJSON McpConfigFile where
  parseJSON = withObject "McpConfigFile" $ \o -> do
    mServersObj <- o .:? "mcpServers" .!= KM.empty
    let pairs = [ (Key.toText k, McpServerConfig (Key.toText k) transport)
                | (k, v) <- KM.toList mServersObj
                , Aeson.Success transport <- [Aeson.fromJSON v]
                ]
    pure (McpConfigFile (Map.fromList pairs))

-- | Format tool name using MCP convention: mcp__<server>__<tool>.
formatMcpToolName :: Text -> Text -> Text
formatMcpToolName server tool = "mcp__" <> server <> "__" <> tool

-- | Parse a qualified MCP tool name back into server name and tool name.
--
-- Splits on the first @__@ delimiter. A server name ending in @_@ or a
-- tool name starting with @_@ fuses with the delimiter into @___@ and is
-- rejected, as are extra @__@ segments in the tool name.
parseMcpToolName :: Text -> Maybe (Text, Text)
parseMcpToolName t = do
  rest <- T.stripPrefix "mcp__" t
  let (srv, after) = T.breakOn "__" rest
  tool <- T.stripPrefix "__" after
  guard (not (T.null srv) && not (T.null tool))
  guard (not (T.isSuffixOf "_" srv) && not (T.isPrefixOf "_" tool))
  guard (not ("__" `T.isInfixOf` tool))
  pure (srv, tool)

-- | Search tool definitions by substring matching.
searchMcpTools :: Text -> [ToolDef] -> [ToolDef]
searchMcpTools query defs =
  let q = T.toLower query
  in [ td | td <- defs, q `T.isInfixOf` T.toLower (toolName td) || q `T.isInfixOf` T.toLower (toolDescription td) ]

-- | Load .mcp.json from the workspace.
loadMcpConfig :: FilePath -> IO (Maybe McpConfigFile)
loadMcpConfig workspace = do
  let path = workspace </> ".mcp.json"
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      res <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _      -> pure Nothing
        Right bytes -> pure (Aeson.decodeStrict bytes)
