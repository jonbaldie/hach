{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Hooks
  ( filterMatchingHandlers
  , parseHookOutput
  , runHookHandler
  , executeHooks
  ) where

import Hach.Types
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), readCreateProcessWithExitCode, shell)

-- | Filter hook handlers by checking whether their matcher matches the target tool.
filterMatchingHandlers :: Maybe Text -> [HookHandler] -> [HookHandler]
filterMatchingHandlers mTool handlers =
  [ h | h <- handlers, matches (hhMatcher h) mTool ]
  where
    matches Nothing _ = True
    matches (Just "*") _ = True
    matches (Just m) (Just t) = T.toLower m == T.toLower t
    matches (Just _) Nothing = False

-- | Parse the output of a command hook according to the exit code protocol.
-- Exit code 0: Pass (no effect).
-- Exit code 2: Block or modify (JSON payload parsed for permissionDecision, additionalContext, modifiedToolInput).
-- Other code: Non-blocking error notice.
parseHookOutput :: Int -> Text -> HookResult
parseHookOutput 0 _ = defaultHookResult
parseHookOutput 2 raw =
  case Aeson.decodeStrict (TE.encodeUtf8 raw) of
    Just res -> res
    Nothing  -> defaultHookResult { hrDecision = Just (PermDeny (if T.null (T.strip raw) then "Hook blocked execution" else raw)) }
parseHookOutput _code err =
  defaultHookResult { hrError = Just err }

-- | Execute a single hook handler synchronously.
runHookHandler :: FilePath -> Aeson.Value -> HookHandler -> IO HookResult
runHookHandler root payload HookHandler{..} = case hhType of
  HookCommand cmd -> do
    let inputStr = T.unpack (TE.decodeUtf8 (BSL.toStrict (Aeson.encode payload)))
        procSpec = (shell (T.unpack cmd)) { cwd = Just root }
    res <- try (readCreateProcessWithExitCode procSpec inputStr) :: IO (Either SomeException (ExitCode, String, String))
    case res of
      Left ex -> pure defaultHookResult { hrError = Just ("Hook execution failed: " <> T.pack (show ex)) }
      Right (ExitSuccess, out, _) -> pure (parseHookOutput 0 (T.pack out))
      Right (ExitFailure 2, out, err) ->
        let combined = if null out then err else out
        in pure (parseHookOutput 2 (T.pack combined))
      Right (ExitFailure code, out, err) ->
        let errMsg = if T.null (T.strip (T.pack err)) then out else err
        in pure (parseHookOutput code (T.pack errMsg))

  HookHttp _url -> do
    -- HTTP hooks can be dispatched over HTTP client
    pure defaultHookResult

  HookMcp _srv _tool -> do
    -- MCP hooks proxy to MCP server
    pure defaultHookResult

-- | Execute all configured handlers for an event sequentially, merging results.
-- Async handlers are spawned in the background.
executeHooks
  :: FilePath
  -> Map HookEvent [HookHandler]
  -> HookEvent
  -> Maybe Text
  -> Aeson.Value
  -> IO HookResult
executeHooks root allHooks event mTool payload = do
  let handlers = fromMaybe [] (Map.lookup event allHooks)
      matching = filterMatchingHandlers mTool handlers
  foldHandlers defaultHookResult matching
  where
    foldHandlers acc [] = pure acc
    foldHandlers acc (h : hs)
      | hhAsync h = do
          _ <- async (runHookHandler root payload h)
          foldHandlers acc hs
      | otherwise = do
          res <- runHookHandler root payload h
          let merged = mergeHookResult acc res
          case hrDecision merged of
            Just (PermDeny _) -> pure merged -- Early stop on block
            _                 -> foldHandlers merged hs

    mergeHookResult r1 r2 = HookResult
      { hrDecision          = hrDecision r2 <|> hrDecision r1
      , hrAdditionalContext = case (hrAdditionalContext r1, hrAdditionalContext r2) of
          (Just c1, Just c2) -> Just (c1 <> "\n" <> c2)
          (Just c1, Nothing) -> Just c1
          (Nothing, Just c2) -> Just c2
          (Nothing, Nothing) -> Nothing
      , hrModifiedInput     = hrModifiedInput r2 <|> hrModifiedInput r1
      , hrError             = case (hrError r1, hrError r2) of
          (Just e1, Just e2) -> Just (e1 <> "; " <> e2)
          (Just e1, Nothing) -> Just e1
          (Nothing, Just e2) -> Just e2
          (Nothing, Nothing) -> Nothing
      }
    (<|>) ma mb = case ma of Just a -> Just a; Nothing -> mb
