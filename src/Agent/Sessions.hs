{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.Sessions
  ( saveSession
  , loadSession
  , listSessions
  , getLatestSessionId
  , makeCompactedHistory
  , formatHistoryForCompaction
  , compactionSystemPrompt
  , estimateCostUsd
  , generateSessionId
  ) where

import Agent.Types
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Numeric (showHex)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>), (<.>))

-- | Save session metadata and message transcript to JSONL under the given sessions directory.
saveSession :: FilePath -> SessionInfo -> [Message] -> IO ()
saveSession dir info msgs = do
  createDirectoryIfMissing True dir
  let sid = T.unpack (siId info)
      metaFile = dir </> (sid <.> "meta.json")
      jsonlFile = dir </> (sid <.> "jsonl")

  -- Write metadata
  BSL.writeFile metaFile (Aeson.encode info)

  -- Write JSONL messages
  let encodedLines = [ BSL.toStrict (Aeson.encode m) | m <- msgs ]
      joined = BS.intercalate "\n" encodedLines <> "\n"
  BS.writeFile jsonlFile joined

-- | Load session metadata and transcript messages from the sessions directory.
loadSession :: FilePath -> Text -> IO (Maybe (SessionInfo, [Message]))
loadSession dir sid = do
  let sidStr = T.unpack sid
      metaFile = dir </> (sidStr <.> "meta.json")
      jsonlFile = dir </> (sidStr <.> "jsonl")
  metaOk <- doesFileExist metaFile
  jsonlOk <- doesFileExist jsonlFile
  if not (metaOk && jsonlOk)
    then pure Nothing
    else do
      metaBytes <- BS.readFile metaFile
      jsonlBytes <- BS.readFile jsonlFile
      case Aeson.decodeStrict metaBytes of
        Nothing -> pure Nothing
        Just info -> do
          let rawLines = BS.split 10 jsonlBytes -- newline '\n'
              parsedMsgs = catMaybes [ Aeson.decodeStrict l | l <- rawLines, not (BS.null l) ]
          pure (Just (info, parsedMsgs))

-- | List all sessions in the directory, sorted by creation date descending.
listSessions :: FilePath -> IO [SessionInfo]
listSessions dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entriesRes <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
      case entriesRes of
        Left _ -> pure []
        Right entries -> do
          let metaFiles = [ dir </> e | e <- entries, ".meta.json" `T.isSuffixOf` T.pack e ]
          infos <- mapM readMeta metaFiles
          pure (sortBy (comparing (Down . siCreatedAt)) (catMaybes infos))
  where
    readMeta fp = do
      res <- try (BS.readFile fp) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _      -> pure Nothing
        Right bytes -> pure (Aeson.decodeStrict bytes)

-- | Get the ID of the most recently created session.
getLatestSessionId :: FilePath -> IO (Maybe Text)
getLatestSessionId dir = do
  sessions <- listSessions dir
  pure $ case sessions of
    (latest : _) -> Just (siId latest)
    []           -> Nothing

-- | Format history into human-readable text suitable for LLM compaction prompt.
formatHistoryForCompaction :: [Message] -> Text
formatHistoryForCompaction = T.unlines . map msgText
  where
    msgText = \case
      SystemMsg c        -> "[System Instructions]: " <> c
      UserMsg c          -> "[User]: " <> c
      AssistantMsg mc tc ->
        let mainTxt = case mc of
              Just c  -> "[Assistant]: " <> c
              Nothing -> "[Assistant]"
            toolTxt = if null tc then "" else " [Called tools: " <> T.intercalate ", " (map functionName tc) <> "]"
        in mainTxt <> toolTxt
      ToolMsg cid name c -> "[Tool Result " <> name <> " (" <> cid <> ")]: " <> T.take 300 c

-- | System prompt for LLM-based context compaction.
compactionSystemPrompt :: Text
compactionSystemPrompt =
  "You are an expert context summarizer. Provide a concise, dense, and structured summary " <>
  "of the conversation history below. Retain key facts, user requests, files read or edited, " <>
  "important tool outputs, architectural decisions, and open tasks. " <>
  "Do not include conversational filler."

-- | Build compacted conversation history replacing old turns with a structured summary message.
makeCompactedHistory :: Maybe Text -> Text -> [Message]
makeCompactedHistory mSysPrompt summary =
  let sysMsg = maybe [] (\s -> [SystemMsg s]) mSysPrompt
      summaryMsg = UserMsg ("[Context summary of earlier turns]:\n" <> summary)
  in sysMsg ++ [summaryMsg]

-- | Estimate token cost in USD based on model family and token usage.
estimateCostUsd :: Text -> Int -> Int -> Double
estimateCostUsd model promptTokens completionTokens =
  let (promptRate, completionRate) = lookupRates (T.toLower model)
      promptCost = (fromIntegral promptTokens / 1000000.0) * promptRate
      completionCost = (fromIntegral completionTokens / 1000000.0) * completionRate
  in promptCost + completionCost
  where
    -- Rates in USD per million tokens
    lookupRates m
      | "opus" `T.isInfixOf` m        = (15.0, 75.0)
      | "sonnet" `T.isInfixOf` m      = (3.0, 15.0)
      | "haiku" `T.isInfixOf` m       = (0.25, 1.25)
      | "gpt-4o-mini" `T.isInfixOf` m = (0.15, 0.6)
      | "gpt-4o" `T.isInfixOf` m      = (5.0, 15.0)
      | otherwise                     = (1.0, 3.0)

-- | Generate a unique session identifier.
generateSessionId :: IO Text
generateSessionId = do
  posix <- getPOSIXTime
  let nanos = round (posix * 1000000) :: Integer
      hex = showHex nanos ""
  pure ("sess-" <> T.pack hex)
