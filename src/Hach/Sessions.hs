{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Sessions
  ( saveSession
  , loadSession
  , listSessions
  , getLatestSessionId
  , makeCompactedHistory
  , formatHistoryForCompaction
  , compactionSystemPrompt
  , estimateCostUsd
  , generateSessionId
  , defaultSessionsDir
  , legacySessionsDir
  , saveWorkspaceSession
  , loadWorkspaceSession
  , getLatestWorkspaceSessionId
  , currentTimestampIso8601
  , SessionTarget(..)
  , resolveSessionTarget
  , resolveSessionLoad
  , buildSessionHistory
  , saveRunSession
  ) where

import Hach.Types
import Control.Exception (SomeException, try)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Numeric (showHex)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, stderr)

-- | Save session metadata and message transcript to JSONL under the given sessions directory.
saveSession :: FilePath -> SessionInfo -> [Message] -> IO ()
saveSession dir info msgs = do
  result <- try $ do
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
  case result of
    Left (ex :: SomeException) ->
      hPutStrLn stderr ("Session save error: " <> show ex)
    Right () -> pure ()

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
    -- First matching needle wins; keep more specific families first so
    -- "claude-3-opus" is not billed as generic Claude.
    lookupRates m =
      fromMaybe (1.0, 3.0) $
        listToMaybe
          [ (p, c)
          | (needle, p, c) <- familyRates
          , needle `T.isInfixOf` m
          ]

    familyRates :: [(Text, Double, Double)]
    familyRates =
      [ ("opus",        15.0, 75.0)
      , ("sonnet",       3.0, 15.0)
      , ("haiku",        0.25, 1.25)
      , ("gpt-4o-mini",  0.15, 0.6)
      , ("gpt-4o",       5.0, 15.0)
      , ("gpt-4",        2.5, 10.0)
      , ("claude",       3.0, 15.0)
      , ("deepseek",     0.27, 1.1)
      ]

-- | Generate a unique session identifier.
generateSessionId :: IO Text
generateSessionId = do
  posix <- getPOSIXTime
  let nanos = round (posix * 1000000) :: Integer
      hex = showHex nanos ""
  pure ("sess-" <> T.pack hex)

-- | Default sessions directory inside a workspace root.
defaultSessionsDir :: FilePath -> FilePath
defaultSessionsDir ws = ws </> ".agents" </> "sessions"

-- | Legacy sessions directory used by earlier versions.
legacySessionsDir :: FilePath -> FilePath
legacySessionsDir ws = ws </> ".agent" </> "sessions"

-- | Save session to workspace default sessions directory.
saveWorkspaceSession :: FilePath -> SessionInfo -> [Message] -> IO ()
saveWorkspaceSession ws sinfo msgs =
  saveSession (defaultSessionsDir ws) sinfo msgs

-- | Load session from workspace, checking canonical .agents and falling back to legacy .agent.
loadWorkspaceSession :: FilePath -> Text -> IO (Maybe (SessionInfo, [Message]))
loadWorkspaceSession ws sid = do
  mRes <- loadSession (defaultSessionsDir ws) sid
  case mRes of
    Just _  -> pure mRes
    Nothing -> loadSession (legacySessionsDir ws) sid

-- | Get ID of the most recently created session in the workspace.
getLatestWorkspaceSessionId :: FilePath -> IO (Maybe Text)
getLatestWorkspaceSessionId ws = do
  mLatest <- getLatestSessionId (defaultSessionsDir ws)
  case mLatest of
    Just sid -> pure (Just sid)
    Nothing  -> getLatestSessionId (legacySessionsDir ws)

-- | ISO-8601 formatted current timestamp.
currentTimestampIso8601 :: IO Text
currentTimestampIso8601 = do
  t <- getCurrentTime
  pure (T.pack (iso8601Show t))

-- | Session target parsed from CLI flags.
data SessionTarget
  = SessionNone
  | SessionContinue
  | SessionSpecific !Text
  deriving (Show, Eq)

-- | Determine session target from CLI continuation/resume flags.
resolveSessionTarget :: Bool -> Bool -> Maybe Text -> SessionTarget
resolveSessionTarget optContinue optResume optSessionId =
  case optSessionId of
    Just sid -> SessionSpecific sid
    Nothing
      | optContinue || optResume -> SessionContinue
      | otherwise                -> SessionNone

-- | Resolve and load session from workspace according to session target.
resolveSessionLoad
  :: FilePath
  -> SessionTarget
  -> IO (Either String (Maybe (SessionInfo, [Message])))
resolveSessionLoad workspace = \case
  SessionNone -> pure (Right Nothing)
  SessionContinue -> do
    mLatestId <- getLatestWorkspaceSessionId workspace
    case mLatestId of
      Nothing -> pure (Left "No stored session found in workspace.")
      Just sid -> do
        mSession <- loadWorkspaceSession workspace sid
        case mSession of
          Just sess -> pure (Right (Just sess))
          Nothing   -> pure (Left ("No stored session found in workspace (failed to load " <> T.unpack sid <> ")."))
  SessionSpecific sid -> do
    mSession <- loadWorkspaceSession workspace sid
    case mSession of
      Just sess -> pure (Right (Just sess))
      Nothing   -> pure (Left ("No stored session found for session ID: " <> T.unpack sid))

isSystemMsg :: Message -> Bool
isSystemMsg (SystemMsg _) = True
isSystemMsg _             = False

-- | Drop trailing unanswered user prompts so history ends on an assistant
-- or tool turn. Matching tool results are kept.
dropTrailingUnansweredUser :: [Message] -> [Message]
dropTrailingUnansweredUser = reverse . dropWhile isUserMsg . reverse
  where
    isUserMsg (UserMsg _) = True
    isUserMsg _           = False

isToolMsg :: Message -> Bool
isToolMsg ToolMsg{} = True
isToolMsg _         = False

-- | Strip assistant tool_calls that have no matching tool results, and drop
-- orphan tool messages, so the sequence is OpenAI-compatible.
closeUnresolvedToolCalls :: [Message] -> [Message]
closeUnresolvedToolCalls = go
  where
    go [] = []
    go (AssistantMsg content calls : rest)
      | null calls = AssistantMsg content [] : go rest
      | otherwise =
          let (tools, afterTools) = span isToolMsg rest
              gotIds = [cid | ToolMsg cid _ _ <- tools]
              wantIds = map callId calls
          in if gotIds == wantIds
               then AssistantMsg content calls : tools ++ go afterTools
               else AssistantMsg content [] : go rest
    go (ToolMsg{} : rest) = go rest
    go (m : rest) = m : go rest

cleanHistoryForSession :: [Message] -> [Message]
cleanHistoryForSession = closeUnresolvedToolCalls . dropTrailingUnansweredUser

-- | Build initial conversation history for a new or resumed session.
-- Preserves prior dialogue messages while ensuring the current system prompt is at the head.
buildSessionHistory :: Text -> Maybe [Message] -> Text -> [Message]
buildSessionHistory sysPrompt mPriorHistory prompt =
  let priorDialogue = case mPriorHistory of
        Nothing   -> []
        Just msgs -> cleanHistoryForSession (filter (not . isSystemMsg) msgs)
  in SystemMsg sysPrompt : priorDialogue ++ [UserMsg prompt]

-- | Persist completed agent dialogue to the workspace session directory.
saveRunSession :: FilePath -> Text -> Text -> Maybe SessionInfo -> [Message] -> IO ()
saveRunSession workspace activeSid model mPrevInfo finalHistory = do
  let cleanHistory = cleanHistoryForSession finalHistory
      totalTurns = length [() | AssistantMsg _ _ <- cleanHistory]
  when (totalTurns > 0) $ do
    timestamp <- currentTimestampIso8601
    let prevCost = maybe 0.0 siCostUsd mPrevInfo
        sessionInfo = SessionInfo
          { siId        = activeSid
          , siCreatedAt = timestamp
          , siModel     = model
          , siTurns     = totalTurns
          , siCostUsd   = prevCost
          }
    saveWorkspaceSession workspace sessionInfo cleanHistory

