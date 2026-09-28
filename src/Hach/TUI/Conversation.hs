{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Hach.TUI.Conversation
  ( messagesToTranscriptItems
  , transcriptItemsToMessages
  , compactTranscriptHistory
  , cancelledToolCallPlaceholder
  ) where

import Hach.TUI.Types
import Hach.Types
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Render model messages as transcript items. A tool result is paired with
-- its call by ID, so each call has one card with its original args and result.
messagesToTranscriptItems :: [Message] -> [TranscriptItem]
messagesToTranscriptItems = go
  where
    go [] = []
    go (SystemMsg systemPrompt : rest) = TiSystem systemPrompt : go rest
    go (UserMsg userText : rest) = TiUser userText : go rest
    go (AssistantMsg response calls : rest) =
      let (toolResults, following) = span isToolMessage rest
          (callCards, unmatchedResults) = pairCallResults calls toolResults
          textItems = maybe [] (\text -> [TiAssistant text]) response
          orphanCards = map toolMessageToCard unmatchedResults
      in textItems ++ callCards ++ orphanCards ++ go following
    go (toolMessage@ToolMsg{} : rest) =
      toolMessageToCard toolMessage : go rest

    isToolMessage ToolMsg{} = True
    isToolMessage _ = False

    pairCallResults calls results =
      let (cardsRev, unmatched) = foldl pairOne ([], results) calls
      in (reverse cardsRev, unmatched)

    pairOne (cards, remaining) call =
      let (before, after) = break (matchesCall (callId call)) remaining
          (lifecycle, rest) = case after of
            ToolMsg _ _ content : later -> (Finished (ToolSuccess content), before ++ later)
            _ -> (Finished (ToolSuccess ""), remaining)
          card = ToolCard (callId call) (functionName call) (callArgsRaw call) lifecycle False
      in (TiToolCard card : cards, rest)

    matchesCall callIdentifier = \case
      ToolMsg resultIdentifier _ _ -> resultIdentifier == callIdentifier
      _ -> False

    toolMessageToCard = \case
      ToolMsg callIdentifier name content ->
        TiToolCard (ToolCard callIdentifier name "" (Finished (ToolSuccess content)) False)
      _ -> error "toolMessageToCard called with a non-tool message"

-- | Convert transcript items back to messages for actions that deliberately
-- compact visible history. Notices do not enter model context.
transcriptItemsToMessages :: [TranscriptItem] -> [Message]
transcriptItemsToMessages = go . collapseAdjacentTextItems . filter (not . isNotice)
  where
    isNotice (TiNotice _) = True
    isNotice _ = False

    collapseAdjacentTextItems =
      collapseAdjacent (\case TiAssistant text -> Just text; _ -> Nothing) TiAssistant
      . collapseAdjacent (\case TiUser text -> Just text; _ -> Nothing) TiUser

    extractCards (TiToolCard card : rest) =
      let (cards, remaining) = extractCards rest
      in (card : cards, remaining)
    extractCards rest = ([], rest)

    go [] = []
    go (TiAssistant text : rest) =
      let (cards, remaining) = extractCards rest
          content = if T.null text then Nothing else Just text
      in if null cards
           then AssistantMsg content [] : go remaining
           else AssistantMsg content (map cardToToolCall cards)
                  : map cardToToolMessage cards ++ go remaining
    go (TiToolCard card : rest) =
      let (cards, remaining) = extractCards rest
          allCards = card : cards
      in AssistantMsg Nothing (map cardToToolCall allCards)
           : map cardToToolMessage allCards ++ go remaining
    go (TiUser text : rest) = UserMsg text : go rest
    go (TiSystem text : rest) = SystemMsg text : go rest
    go (TiNotice _ : rest) = go rest

-- | Keep the latest visible transcript entries and the active system prompt
-- when the user explicitly compacts the conversation.
compactTranscriptHistory :: [Message] -> [TranscriptItem] -> [Message]
compactTranscriptHistory currentHistory retainedTranscript =
  let systemMessages = [message | message@SystemMsg{} <- currentHistory]
      activeSystem = maybe [] pure (listToMaybe (reverse systemMessages))
      visibleDialogue = filter (\case TiSystem _ -> False; _ -> True) retainedTranscript
  in activeSystem ++ transcriptItemsToMessages visibleDialogue

cardToToolCall :: ToolCard -> ToolCall
cardToToolCall card = ToolCall
  { callId = tcId card
  , functionName = tcName card
  , callArgsRaw = tcArgs card
  }

cardToToolMessage :: ToolCard -> Message
cardToToolMessage card =
  ToolMsg (tcId card) (tcName card) (toolCardContent (tcLifecycle card))

toolCardContent :: ToolLifecycle -> Text
toolCardContent = \case
  Finished result -> toolResultToText result
  Denied reason -> reason
  Pending -> cancelledToolCallPlaceholder
  Running -> cancelledToolCallPlaceholder
  Cancelled -> cancelledToolCallPlaceholder

cancelledToolCallPlaceholder :: Text
cancelledToolCallPlaceholder = "Tool call was cancelled before completion."

collapseAdjacent :: (a -> Maybe Text) -> (Text -> a) -> [a] -> [a]
collapseAdjacent view wrap = go
  where
    go [] = []
    go (item : rest) = case view item of
      Just text ->
        let (more, remaining) = spanView [] rest
        in wrap (T.intercalate "\n\n" (text : more)) : go remaining
      Nothing -> item : go rest

    spanView acc [] = (reverse acc, [])
    spanView acc (item : rest) = case view item of
      Just text -> spanView (text : acc) rest
      Nothing -> (reverse acc, item : rest)
