{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.TUI.UI
  ( drawUI
  , tuiAttrMap
  , formatTokens
  , formatCompactLimit
  , formatToolTarget
  , renderTokensDisplay
  , tokenWarnAttr
  , tokenCritAttr
  , renderMaxTurns
  , Name(..)
  , wideGlyphs
  , installWideGlyphWidths
  ) where

import Hach.Skills (inputSlashCompletion)
import Hach.TUI.Types
import Hach.Tools (toolTarget)
import Hach.Types (SessionTokenUsage(..), ToolCall(..), ToolResult(..), contextSaturationPercent)
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Exception (handle)
import Data.Char (ord)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Lens.Micro ((^.))
import Text.Printf (printf)
import Graphics.Vty.UnicodeWidthTable.Install (TableInstallException, installUnicodeWidthTable)
import Graphics.Vty.UnicodeWidthTable.Types (UnicodeWidthTable(..), WidthTableRange(..))

data Name
  = VpTranscript
  | VpInput
  | VpApproval !Int
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Glyph Widths
--------------------------------------------------------------------------------

-- | Glyphs used below that terminals draw two columns wide.
--
-- Vty 6.6 ships a character-width table predating Unicode 9, so it measures
-- each of these as one column. Every widget vty places after one of them on the
-- same row therefore lands one column left of where the terminal actually draws
-- it, which knocks the panel borders out of alignment on exactly those rows.
--
-- Keep this list in sync with the glyphs used in this module: any character
-- with East_Asian_Width W/F or Emoji_Presentation=Yes belongs here.
wideGlyphs :: [Char]
wideGlyphs =
  [ '\x1F4AC'  -- SPEECH BALLOON, in the Transcript border label
  , '\x23FA'   -- BLACK CIRCLE FOR RECORD, the tool card bullet
  ]

-- | Teach vty the real terminal width of 'wideGlyphs'. Must run before the
-- first render. Characters absent from the override keep their built-in width.
--
-- Vty's width table is process-global and installable only once, so a repeat
-- call throws; the first install has already taken effect, and a failure here
-- costs alignment rather than correctness, so it must not take down the TUI.
installWideGlyphWidths :: IO ()
installWideGlyphWidths =
  handle ignoreInstallFailure $
    installUnicodeWidthTable $ UnicodeWidthTable
      [ WidthTableRange (fromIntegral (ord c)) 1 2 | c <- wideGlyphs ]
  where
    ignoreInstallFailure :: TableInstallException -> IO ()
    ignoreInstallFailure _ = pure ()

--------------------------------------------------------------------------------
-- Theme Attributes (Claude Code / AGY CLI Style)
--------------------------------------------------------------------------------

brandAttr, modelAttr, turnAttr, tokenAttr, tokenWarnAttr, tokenCritAttr, dimAttr :: AttrName
brandAttr     = attrName "brand"
modelAttr     = attrName "model"
turnAttr      = attrName "turn"
tokenAttr     = attrName "token"
tokenWarnAttr = attrName "tokenWarn"
tokenCritAttr = attrName "tokenCrit"
dimAttr       = attrName "dim"

statusIdleAttr, statusThinkingAttr, statusRunningAttr, statusErrorAttr :: AttrName
statusIdleAttr     = attrName "statusIdle"
statusThinkingAttr = attrName "statusThinking"
statusRunningAttr  = attrName "statusRunning"
statusErrorAttr    = attrName "statusError"

userAttr, userPromptAttr, userTextAttr :: AttrName
userAttr       = attrName "user"
userPromptAttr = attrName "userPrompt"
userTextAttr   = attrName "userText"

asstAttr, asstTextAttr, codeBlockAttr :: AttrName
asstAttr     = attrName "assistant"
asstTextAttr = attrName "assistantText"
codeBlockAttr = attrName "codeBlock"

sysAttr, noticeAttr :: AttrName
sysAttr    = attrName "system"
noticeAttr = attrName "notice"

toolIconAttr, toolNameAttr, toolTargetAttr, toolSuccessAttr, toolErrorAttr :: AttrName
toolIconAttr    = attrName "toolIcon"
toolNameAttr    = attrName "toolName"
toolTargetAttr  = attrName "toolTarget"
toolSuccessAttr = attrName "toolSuccess"
toolErrorAttr   = attrName "toolError"

shortcutKeyAttr, activeBorderAttr, inactiveBorderAttr :: AttrName
shortcutKeyAttr    = attrName "shortcutKey"
activeBorderAttr   = attrName "activeBorder"
inactiveBorderAttr = attrName "inactiveBorder"

tuiAttrMap :: AttrMap
tuiAttrMap = attrMap Vty.defAttr
  [ (brandAttr,          Vty.withStyle (fg (Vty.rgbColor (249 :: Int) (115 :: Int) (22 :: Int))) Vty.bold)  -- Claude Warm Amber/Orange
  , (modelAttr,          Vty.withStyle (fg (Vty.rgbColor (192 :: Int) (132 :: Int) (252 :: Int))) Vty.bold)  -- Lavender / Purple
  , (turnAttr,           fg (Vty.rgbColor (251 :: Int) (191 :: Int) (36 :: Int)))                            -- Gold
  , (tokenAttr,          Vty.withStyle (fg (Vty.rgbColor (56 :: Int) (189 :: Int) (248 :: Int))) Vty.bold)  -- Electric Sky Blue
  , (tokenWarnAttr,      Vty.withStyle (fg (Vty.rgbColor (251 :: Int) (191 :: Int) (36 :: Int))) Vty.bold)  -- Amber Warning (>80%)
  , (tokenCritAttr,      Vty.withStyle (fg (Vty.rgbColor (248 :: Int) (113 :: Int) (113 :: Int))) Vty.bold)  -- Coral/Red Critical (>90%)
  , (dimAttr,            fg (Vty.rgbColor (100 :: Int) (116 :: Int) (139 :: Int)))                           -- Slate Gray
  , (statusIdleAttr,     Vty.withStyle (fg (Vty.rgbColor (52 :: Int) (211 :: Int) (153 :: Int))) Vty.bold)  -- Mint Green
  , (statusThinkingAttr, Vty.withStyle (fg (Vty.rgbColor (251 :: Int) (191 :: Int) (36 :: Int))) Vty.bold)  -- Amber
  , (statusRunningAttr,  Vty.withStyle (fg (Vty.rgbColor (56 :: Int) (189 :: Int) (248 :: Int))) Vty.bold)  -- Sky Blue
  , (statusErrorAttr,    Vty.withStyle (fg (Vty.rgbColor (248 :: Int) (113 :: Int) (113 :: Int))) Vty.bold)  -- Coral Red
  , (userPromptAttr,     Vty.withStyle (fg (Vty.rgbColor (56 :: Int) (189 :: Int) (248 :: Int))) Vty.bold)  -- Vibrant Cyan
  , (userAttr,           Vty.withStyle (fg (Vty.rgbColor (241 :: Int) (245 :: Int) (249 :: Int))) Vty.bold)  -- Crisp White
  , (userTextAttr,       fg (Vty.rgbColor (248 :: Int) (250 :: Int) (252 :: Int)))                            -- High Contrast White
  , (asstAttr,           Vty.withStyle (fg (Vty.rgbColor (249 :: Int) (115 :: Int) (22 :: Int))) Vty.bold)  -- Warm Claude Accent
  , (asstTextAttr,       fg (Vty.rgbColor (226 :: Int) (232 :: Int) (240 :: Int)))                            -- Soft Off-White
  , (codeBlockAttr,      fg (Vty.rgbColor (148 :: Int) (163 :: Int) (184 :: Int)))                           -- Code Slate
  , (sysAttr,            fg (Vty.rgbColor (251 :: Int) (191 :: Int) (36 :: Int)))                            -- Amber Notice
  , (noticeAttr,         Vty.withStyle (fg (Vty.rgbColor (248 :: Int) (113 :: Int) (113 :: Int))) Vty.bold)  -- Alert Red
  , (toolIconAttr,       Vty.withStyle (fg (Vty.rgbColor (129 :: Int) (140 :: Int) (248 :: Int))) Vty.bold)  -- Electric Violet
  , (toolNameAttr,       Vty.withStyle (fg (Vty.rgbColor (199 :: Int) (210 :: Int) (254 :: Int))) Vty.bold)  -- Soft Indigo
  , (toolTargetAttr,     fg (Vty.rgbColor (253 :: Int) (224 :: Int) (71 :: Int)))                            -- Soft Yellow
  , (toolSuccessAttr,    Vty.withStyle (fg (Vty.rgbColor (52 :: Int) (211 :: Int) (153 :: Int))) Vty.bold)  -- Emerald
  , (toolErrorAttr,      Vty.withStyle (fg (Vty.rgbColor (248 :: Int) (113 :: Int) (113 :: Int))) Vty.bold)  -- Coral
  , (shortcutKeyAttr,    Vty.withStyle (fg (Vty.rgbColor (241 :: Int) (245 :: Int) (249 :: Int))) Vty.bold)  -- White
  , (activeBorderAttr,   Vty.withStyle (fg (Vty.rgbColor (56 :: Int) (189 :: Int) (248 :: Int))) Vty.bold)  -- Glowing Cyan
  , (inactiveBorderAttr, fg (Vty.rgbColor (51 :: Int) (65 :: Int) (85 :: Int)))                            -- Muted Slate
  ]

--------------------------------------------------------------------------------
-- Main Layout
--------------------------------------------------------------------------------

-- | Draw the full TUI layout.
drawUI :: TuiState -> [Widget Name]
drawUI state@TuiState{..} =
  case tsPendingAsk of
    Just prompt -> [permissionOverlay prompt, baseLayout state]
    Nothing | tsShowHelp -> [helpOverlay, baseLayout state]
    Nothing -> [baseLayout state]

-- | The core dashboard layout with Claude Code / AGY CLI proportions.
--
-- The transcript panel is the only greedy widget: Brick's 'vBox'
-- allocates 'Fixed' widgets first (header, input, footer) and gives
-- the remainder to greedy ones, so the input line keeps its height
-- even on short terminals. (A 'vLimitPercent' here would be fixed
-- height computed from the rows left after the header, crowding the
-- input and footer out on terminals under ~15 rows.)
baseLayout :: TuiState -> Widget Name
baseLayout state =
  vBox
    [ renderHeader state
    , renderTranscriptPanel state
    , renderInputPanel state
    , renderFooter
    ]

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Format an integer token count with comma thousands separators.
-- Uses Integer internally to avoid overflow on minBound (abs minBound == minBound).
formatTokens :: Int -> Text
formatTokens n
  | n < 0     = "-" <> go (negate (fromIntegral n))
  | otherwise = go (fromIntegral n)
  where
    go :: Integer -> Text
    go m
      | m < 1000  = T.pack (show m)
      | otherwise = go (m `div` 1000) <> "," <> padThree (m `mod` 1000)

    padThree x =
      let s = show x
      in T.pack (replicate (3 - length s) '0' ++ s)

-- | Render the max-turns display: the infinity sign when unlimited.
renderMaxTurns :: Maybe Int -> Text
renderMaxTurns = maybe "∞" (T.pack . show)

-- | Format a token limit or count compactly (e.g., 0, 8.4k, 128k, 1M, 1.5M).
formatCompactLimit :: Int -> Text
formatCompactLimit n
  | n >= 1000000 =
      let d = (fromIntegral n :: Double) / 1000000.0
      in if n `mod` 1000000 == 0
           then T.pack (show (n `div` 1000000)) <> "M"
           else T.pack (printf "%.1f" d) <> "M"
  | n >= 1000 =
      let d = (fromIntegral n :: Double) / 1000.0
      in if n `mod` 1000 == 0
           then T.pack (show (n `div` 1000)) <> "k"
           else T.pack (printf "%.1f" d) <> "k"
  | otherwise = T.pack (show n)

-- | Render context and session token consumption widget with saturation warning styling.
renderTokensDisplay :: Int -> Int -> Maybe Int -> UsageStatus -> Widget Name
renderTokensDisplay ctxTokens sesTokens mLimit status =
  let (pctPart, limitPart, attr) = case mLimit of
        Just limit | limit > 0 ->
          let pct = contextSaturationPercent ctxTokens limit
              a = if pct >= 90
                    then tokenCritAttr
                    else if pct >= 80
                           then tokenWarnAttr
                           else tokenAttr
          in (" (" <> T.pack (show pct) <> "%)", "/" <> formatCompactLimit limit, a)
        _ -> ("", "/?", tokenAttr)
      missingTag = case status of
        UsageMissing  -> " [?]"
        UsageVerified -> ""
      ctxText = "ctx: " <> formatCompactLimit ctxTokens <> limitPart <> pctPart <> missingTag
      sesText = "ses: " <> formatCompactLimit sesTokens
  in hBox
       [ withAttr attr (txt ctxText)
       , withAttr dimAttr (txt " │ ")
       , withAttr tokenAttr (txt sesText)
       ]

-- | Modern, sleek status bar (Claude Code / AGY CLI style).
renderHeader :: TuiState -> Widget Name
renderHeader state@TuiState{..} =
  withBorderStyle unicodeRounded $
  border $
  hBox
    [ withAttr brandAttr (txt " ✻ agent ")
    , withAttr dimAttr (txt "│ ")
    , withAttr dimAttr (txt "model: ")
    , withAttr modelAttr (txt tsModelName)
    , withAttr dimAttr (txt "  │  turn: ")
    , withAttr turnAttr (txt (T.pack (show tsCurrentTurn) <> "/" <> renderMaxTurns tsMaxTurns))
    , withAttr dimAttr (txt "  │  ")
    , renderTokensDisplay tsContextTokens (stuTotalTokens tsDisplayedSessionTokens) (tuiModelContextLimit state) tsUsageStatus
    , withAttr dimAttr (txt "  │  ")
    , renderStatus tsStatus
    , padLeft Max (withAttr dimAttr (txt "press ? for help "))
    ]

renderStatus :: TuiStatus -> Widget Name
renderStatus = \case
  StatusIdle                   -> withAttr statusIdleAttr (txt "● idle")
  StatusThinking               -> withAttr statusThinkingAttr (txt "● thinking...")
  StatusRunningTool t          -> withAttr statusRunningAttr (txt ("● running " <> t <> "..."))
  StatusAwaitingPermission t   -> withAttr statusThinkingAttr (txt ("● approve " <> t <> "?"))
  StatusFinished               -> withAttr statusIdleAttr (txt "✔ ready")
  StatusError err              -> withAttr statusErrorAttr (txt ("✖ error: " <> T.take 25 err))

--------------------------------------------------------------------------------
-- Transcript Panel (Claude Code Style Unified Stream)
--------------------------------------------------------------------------------

-- | Full-width panel showing conversation dialogue and tool cards interleaved in chronological order.
renderTranscriptPanel :: TuiState -> Widget Name
renderTranscriptPanel state@TuiState{..} =
  let isFocused = tsFocus == FocusTranscript
      borderMod = if isFocused then withAttr activeBorderAttr else withAttr inactiveBorderAttr
      borderGlyph = if isFocused then unicodeBold else unicodeRounded
      headerText = if isFocused
                     then " [ 💬 Transcript (Active) ] "
                     else " 💬 Transcript "
      items = if null tsTranscript && tsStatus /= StatusThinking
                then [padAll 1 (withAttr dimAttr (txtWrap "No dialogue yet. Type a prompt below and press Enter to start."))]
                else renderTranscriptItems state
  in borderMod $
     withBorderStyle borderGlyph $
     borderWithLabel (txt headerText) $
     viewport VpTranscript Vertical (vBox items)

-- | Render all items in the transcript in chronological emission order.
renderTranscriptItems :: TuiState -> [Widget Name]
renderTranscriptItems TuiState{..} =
  let isFocused = tsFocus == FocusTranscript
      step (toolIdx, acc) item = case item of
        TiUser u ->
          (toolIdx, renderUser u : acc)
        TiAssistant a ->
          (toolIdx, renderAssistant a : acc)
        TiSystem s ->
          (toolIdx, renderSystem s : acc)
        TiNotice n ->
          (toolIdx, renderNotice n : acc)
        TiToolCard tc ->
          let selected = isFocused && tsSelectedToolIndex == toolIdx
              card = renderToolCard selected tc
              reveal = selected && not tsTranscriptManualScroll
          in (toolIdx + 1, (if reveal then visible card else card) : acc)
      (_, revWidgets) = L.foldl' step (0, []) tsTranscript
      thinkingWidget = [renderThinkingLine | tsStatus == StatusThinking]
  in reverse revWidgets ++ thinkingWidget

-- | Ephemeral thinking indicator drawn under the last transcript item
-- while the Harness is waiting on the model.
renderThinkingLine :: Widget Name
renderThinkingLine =
  padBottom (Pad 1) $
  padLeft (Pad 2) $
  withAttr statusThinkingAttr (txt "✻ Thinking...")

renderUser :: Text -> Widget Name
renderUser u =
  padBottom (Pad 1) $
  vBox
    [ hBox
        [ withAttr userPromptAttr (txt "❯ ")
        , withAttr userAttr (txt "You")
        ]
    , padLeft (Pad 2) $
      withAttr userTextAttr (txtWrap u)
    ]

renderAssistant :: Text -> Widget Name
renderAssistant a =
  padBottom (Pad 1) $
  vBox
    [ hBox
        [ withAttr asstAttr (txt "✦ ")
        , withAttr asstAttr (txt "Assistant")
        ]
    , padLeft (Pad 2) $
      renderAssistantBody a
    ]

renderSystem :: Text -> Widget Name
renderSystem s =
  padBottom (Pad 1) $
  padLeft (Pad 2) $
  withAttr sysAttr (txtWrap ("⚙ " <> s))

renderNotice :: Text -> Widget Name
renderNotice n =
  padBottom (Pad 1) $
  padLeft (Pad 2) $
  withAttr noticeAttr (txtWrap ("! " <> n))

-- | Render assistant response text, formatting code blocks cleanly.
renderAssistantBody :: Text -> Widget Name
renderAssistantBody content =
  let lns = T.lines content
      blocks = groupCodeBlocks lns
  in vBox (map renderBlock blocks)
  where
    renderBlock (Left textLines) =
      withAttr asstTextAttr (txtWrap (T.unlines textLines))
    renderBlock (Right (lang, codeLines)) =
      padTop (Pad 1) $
      padBottom (Pad 1) $
      withBorderStyle unicodeRounded $
      borderWithLabel (withAttr dimAttr (txt (if T.null lang then " code " else " " <> lang <> " "))) $
      padLeftRight 1 $
      withAttr codeBlockAttr (vBox (map (\l -> if T.null l then txt " " else txt l) codeLines))

-- | Group text lines into prose and fenced code blocks.
groupCodeBlocks :: [Text] -> [Either [Text] (Text, [Text])]
groupCodeBlocks = go []
  where
    go acc [] = case reverse acc of
      [] -> []
      xs -> [Left xs]
    go acc (l:ls)
      | "```" `T.isPrefixOf` T.stripStart l =
          let before = case reverse acc of
                [] -> []
                xs -> [Left xs]
              lang = T.strip (T.drop 3 (T.stripStart l))
              (code, rest) = break (\x -> "```" `T.isPrefixOf` T.stripStart x) ls
              after = case rest of
                (_:remLines) -> remLines
                []           -> []
          in before ++ [Right (lang, code)] ++ go [] after
      | otherwise =
          go (l:acc) ls

-- | Extract a clean, human-readable summary of tool arguments (Claude Code style).
formatToolTarget :: Text -> Text -> Text
formatToolTarget name rawArgs =
  maybe (truncateText 30 rawArgs) id (toolTarget (ToolCall "" name rawArgs))

truncateText :: Int -> Text -> Text
truncateText maxLen t
  | T.length t > maxLen = T.take maxLen t <> "..."
  | otherwise           = t

formatLifecycleSummary :: ToolLifecycle -> (Text, AttrName, AttrName)
formatLifecycleSummary = \case
  Pending ->
    ("◌ pending...", dimAttr, dimAttr)
  Running ->
    ("◌ running...", statusThinkingAttr, statusRunningAttr)
  Finished (ToolSuccess out) ->
    let chars = T.length out
        linesCount = length (T.lines out)
        tag = if linesCount > 1
                then "✓ success (" <> T.pack (show linesCount) <> " lines)"
                else "✓ success (" <> T.pack (show chars) <> " chars)"
    in (tag, toolSuccessAttr, toolSuccessAttr)
  Finished (ToolError err) ->
    ("✖ failed (" <> T.take 25 err <> ")", toolErrorAttr, toolErrorAttr)
  Denied reason ->
    ("✖ denied (" <> T.take 25 reason <> ")", toolErrorAttr, toolErrorAttr)
  Cancelled ->
    ("✖ cancelled", toolErrorAttr, toolErrorAttr)

renderToolCard :: Bool -> ToolCard -> Widget Name
renderToolCard isSelected ToolCard{..} =
  let cursorMark = if isSelected then withAttr userPromptAttr (txt "▸ ") else txt "  "
      (statusTxt, statusAttr, iconAttr) = formatLifecycleSummary tcLifecycle
      icon = withAttr iconAttr (txt "⏺ ")
      nameWidget = withAttr toolNameAttr (txt tcName)
      targetText = formatToolTarget tcName tcArgs
      targetWidget = if T.null targetText
                       then emptyWidget
                       else withAttr toolTargetAttr (txt (" " <> targetText))
      statusWidget = withAttr statusAttr (txt statusTxt)

      headerLine = hBox [cursorMark, icon, nameWidget, targetWidget]
      subLine = padLeft (Pad 4) $
                hBox
                  [ withAttr dimAttr (txt "⎿  ")
                  , statusWidget
                  , if tcExpanded
                      then withAttr dimAttr (txt "  (expanded)")
                      else withAttr dimAttr (txt "  (↵ details)")
                  ]

      expandedBody = if tcExpanded
        then padLeft (Pad 4) $
             padTop (Pad 1) $
             vBox
               [ withBorderStyle unicodeRounded $
                 borderWithLabel (withAttr dimAttr (txt " Arguments ")) $
                 padLeftRight 1 (withAttr codeBlockAttr (txtWrap tcArgs))
               , case tcLifecycle of
                   Pending ->
                     padTop (Pad 1) (withAttr dimAttr (txt "Pending execution..."))
                   Running ->
                     padTop (Pad 1) (withAttr statusThinkingAttr (txt "Waiting for execution output..."))
                   Finished (ToolSuccess out) ->
                     padTop (Pad 1) $
                     withBorderStyle unicodeRounded $
                     borderWithLabel (withAttr toolSuccessAttr (txt " Output ")) $
                     padLeftRight 1 (withAttr codeBlockAttr (txtWrap (if T.null out then "(empty output)" else out)))
                   Finished (ToolError err) ->
                     padTop (Pad 1) $
                     withBorderStyle unicodeRounded $
                     borderWithLabel (withAttr toolErrorAttr (txt " Error ")) $
                     padLeftRight 1 (withAttr toolErrorAttr (txtWrap err))
                   Denied reason ->
                     padTop (Pad 1) $
                     withBorderStyle unicodeRounded $
                     borderWithLabel (withAttr toolErrorAttr (txt " Denied ")) $
                     padLeftRight 1 (withAttr toolErrorAttr (txtWrap reason))
                   Cancelled ->
                     padTop (Pad 1) (withAttr toolErrorAttr (txt "Tool call was cancelled."))
               ]
        else emptyWidget

      cardWidget =
        padBottom (Pad 1) $
        vBox [headerLine, subLine, expandedBody]

  in cardWidget

--------------------------------------------------------------------------------
-- Task Input Panel
--------------------------------------------------------------------------------

-- | Terminal columns occupied by a character. Combining marks are width 0.
displayColWidth :: Char -> Int
displayColWidth c = max 0 (Vty.safeWcwidth c)

-- | Terminal columns occupied by a string.
displayWidth :: Text -> Int
displayWidth = T.foldl' (\n c -> n + displayColWidth c) 0

-- | Wrap at display-column width, breaking mid-token when a word is longer
-- than the line. Combining characters stay attached to the preceding mark;
-- a character wider than the line sits on its own row rather than being split.
wrapToDisplayWidth :: Int -> Text -> [Text]
wrapToDisplayWidth width text
  | width < 1 = [text]
  | T.null text = [""]
  | otherwise = go "" 0 text
  where
    go line lineW rest =
      case T.uncons rest of
        Nothing -> [line]
        Just (c, cs) ->
          let cw = displayColWidth c
          in if cw == 0
               then go (line `T.snoc` c) lineW cs
               else if lineW + cw <= width
                 then go (line `T.snoc` c) (lineW + cw) cs
                 else if T.null line
                   then T.singleton c : go "" 0 cs
                   else line : go (T.singleton c) cw cs

-- | Keep the input panel from crowding out the transcript: show at most this
-- many wrapped rows, always including the line that holds the typing cursor.
maxInputBodyLines :: Int
maxInputBodyLines = 6

promptPrefixText :: Text
promptPrefixText = "❯ "

promptPrefixCols :: Int
promptPrefixCols = displayWidth promptPrefixText

-- | Modern prompt input bar.
-- Long prompts wrap on display columns so the typing position stays visible.
-- Trailing slash-token completion (built-in commands and user-invocable
-- skills) is shown as dim ghost text; Tab accepts.
renderInputPanel :: TuiState -> Widget Name
renderInputPanel TuiState{..} =
  let isFocused = tsFocus == FocusInput
      borderMod = if isFocused then withAttr activeBorderAttr else withAttr inactiveBorderAttr
      borderGlyph = if isFocused then unicodeBold else unicodeRounded
      promptLabel = if isFocused then " [ ❯ Prompt (Active) ] " else " ❯ Prompt "
      body
        | T.null tsInputBuffer =
            let placeholder =
                  withAttr userPromptAttr (txt promptPrefixText)
                    <+> withAttr dimAttr (txt "Type a task prompt and press Enter...")
            in if isFocused
                 then showCursor VpInput (Location (promptPrefixCols, 0)) placeholder
                 else placeholder
        | otherwise =
            wrappedPromptBody isFocused tsInputBuffer
              (inputSlashCompletion tsSkills builtinCommands tsInputBuffer)
  in borderMod $
     withBorderStyle borderGlyph $
     borderWithLabel (txt promptLabel) $
     padLeftRight 1 $
     padRight Max body

-- | Wrap the prompt, place the cursor at the end of the buffer, and keep the
-- last wrapped rows when the prompt is taller than 'maxInputBodyLines'.
wrappedPromptBody :: Bool -> Text -> Maybe Text -> Widget Name
wrappedPromptBody focused buffer mSuffix =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let wrapW = max 1 (ctx ^. availWidthL - promptPrefixCols)
        takeN = min maxInputBodyLines (max 1 (ctx ^. availHeightL))
        bufLines = wrapToDisplayWidth wrapW buffer
        dropped = max 0 (length bufLines - takeN)
        shown =
          case drop dropped bufLines of
            [] -> [""]
            ls -> ls
        cursorRow = length shown - 1
        lastLine =
          case reverse shown of
            (l:_) -> l
            []    -> ""
        cursorCol = promptPrefixCols + displayWidth lastLine
        cursorLoc = Location (cursorCol, cursorRow)
        indent = txt (T.replicate promptPrefixCols " ")
        lineWidget i ln =
          let lead
                | dropped == 0 && i == 0 = withAttr userPromptAttr (txt promptPrefixText)
                | otherwise = indent
              ghost
                | i == cursorRow
                , Just suffix <- mSuffix
                = withAttr dimAttr (txt suffix)
                | otherwise = emptyWidget
          in lead <+> withAttr userTextAttr (txt ln) <+> ghost
        stacked = vBox (zipWith lineWidget [0..] shown)
        placed
          | focused = showCursor VpInput cursorLoc stacked
          | otherwise = stacked
    render placed

--------------------------------------------------------------------------------
-- Footer & Help
--------------------------------------------------------------------------------

-- | Quiet, minimalist shortcut bar.
renderFooter :: Widget Name
renderFooter =
  hCenter $
  hBox
    [ withAttr shortcutKeyAttr (txt "⇥ tab")
    , withAttr dimAttr (txt " complete/focus  •  ")
    , withAttr shortcutKeyAttr (txt "↵ enter")
    , withAttr dimAttr (txt " send  •  ")
    , withAttr shortcutKeyAttr (txt "esc")
    , withAttr dimAttr (txt " cancel  •  ")
    , withAttr shortcutKeyAttr (txt "c")
    , withAttr dimAttr (txt " clear  •  ")
    , withAttr shortcutKeyAttr (txt "?")
    , withAttr dimAttr (txt " help  •  ")
    , withAttr shortcutKeyAttr (txt "^q")
    , withAttr dimAttr (txt " quit")
    ]

permissionOverlay :: PermissionPrompt -> Widget Name
permissionOverlay PermissionPrompt{..} = Widget Greedy Greedy $ do
  ctx <- getContext
  let width = max 1 (ctx ^. availWidthL - 6)
      wrap = concatMap (wrapToDisplayWidth width) . T.splitOn "\n"
      commandRows = wrap (formatToolTarget ppTool ppArgs)
      reasonRows = wrap ppReason
      -- Border and padding use six rows; reserve the remaining fixed content
      -- before sizing the command viewport so approval controls stay visible.
      commandHeight = max 1 (ctx ^. availHeightL - 11 - length reasonRows)
      needsScroll = length commandRows > commandHeight
  -- 'centerLayer', not 'center': 'center' fills the surrounding area
  -- with spaces, blanking the base layout underneath this overlay.
  render $ centerLayer $
    withBorderStyle unicodeBold $
    withAttr activeBorderAttr $
    borderWithLabel (withAttr brandAttr (txt " Approval required ")) $
    padAll 2 $
    vBox
      [ withAttr toolNameAttr (txt ppTool)
      , withAttr toolTargetAttr $
          vLimit (min commandHeight (length commandRows)) $
          viewport (VpApproval ppId) Vertical (vBox (map txt commandRows))
      , withAttr dimAttr (txt (if needsScroll then "Up/Down/PgUp/PgDn: scroll command" else " "))
      , withAttr dimAttr (vBox (map txt reasonRows))
      , txt " "
      , hBox
          [ withAttr shortcutKeyAttr (txt "y")
          , withAttr dimAttr (txt " approve  •  ")
          , withAttr shortcutKeyAttr (txt "n")
          , withAttr dimAttr (txt " deny  •  ")
          , withAttr shortcutKeyAttr (txt "esc")
          , withAttr dimAttr (txt " cancel")
          ]
      ]

-- | Help guide content: a section title and its (key, description) pairs.
helpSections :: [(Text, [(Text, Text)])]
helpSections =
  [ ( "Navigation & Global:"
    , [ ("Tab / Shift+Tab", "Switch focus (Input ⇄ Transcript)")
      , ("Mouse wheel", "Scroll transcript without changing focus")
      , ("Tab (in input)", "Accept a /skill autocomplete suggestion, else switch focus")
      , ("Ctrl+Q", "Quit the application immediately")
      , ("Esc / Ctrl+C", "Cancel running agent turn or dismiss help")
      , ("? / F1", "Toggle this help overlay")
      ]
    )
  , ( "Task Input:"
    , [ ("Enter", "Submit prompt to the autonomous agent")
      , ("Up / Down", "Recall previous / next prompt history")
      , ("PgUp / PgDn", "Scroll transcript 5 lines")
      , ("Ctrl+U", "Clear current input line")
      ]
    )
  , ( "Transcript:"
    , [ ("Up / Down", "Select tool card (when present) or scroll 1 line")
      , ("PgUp / PgDn", "Scroll transcript 5 lines")
      , ("Enter / Space", "Expand / collapse selected tool card")
      , ("c", "Clear transcript and reset context")
      , ("q", "Quit application (when idle)")
      ]
    )
  ]

-- | Column the help descriptions start at, counting the key column.
helpKeyWidth :: Int
helpKeyWidth = maximum (18 : map (displayWidth . fst) (concatMap snd helpSections))

-- | Greedy word wrap at display-column width. A single word longer
-- than the width falls back to 'wrapToDisplayWidth' so it is kept
-- whole-piece by piece instead of dropped.
wrapWords :: Int -> Text -> [Text]
wrapWords width text
  | width < 1 = [text]
  | T.null text = [""]
  | otherwise = go "" (T.words text)
  where
    go line [] = [line]
    go line (w : ws)
      | displayWidth w > width =
          let chunks = wrapToDisplayWidth width w
          in (if T.null line then [] else [line]) ++ init chunks ++ go (last chunks) ws
      | T.null line = go w ws
      | displayWidth line + 1 + displayWidth w <= width = go (line <> " " <> w) ws
      | otherwise = line : go w ws

-- | One help entry: the key in a fixed column, the description
-- wrapped to the remaining width with continuation lines aligned
-- under the first description line.
helpEntry :: Int -> Text -> Text -> Widget Name
helpEntry width key desc =
  let descW = max 1 (width - helpKeyWidth)
      chunks = wrapWords descW desc
      keyWidget = withAttr shortcutKeyAttr (txt (key <> T.replicate (helpKeyWidth - displayWidth key) " "))
      rows (c : cs) =
        hBox [keyWidget, withAttr dimAttr (txt c)]
          : map (\ch -> padLeft (Pad helpKeyWidth) (withAttr dimAttr (txt ch))) cs
      rows [] = [hBox [keyWidget, withAttr dimAttr (txt " ")]]
  in vBox (rows chunks)

-- | Help dialog overlay. 'centerLayer', not 'center': 'center' fills
-- the surrounding area with spaces, blanking the base layout
-- underneath. Descriptions wrap to the available width so narrow
-- terminals see the whole guide instead of clipped lines.
helpOverlay :: Widget Name
helpOverlay = Widget Greedy Greedy $ do
  ctx <- getContext
  let contentW = max 20 (ctx ^. availWidthL - 6)
      entryW = max 1 (contentW - 2)
      section (title, entries) =
        vBox
          [ withAttr brandAttr (txt title)
          , padLeft (Pad 2) $ vBox (map (uncurry (helpEntry entryW)) entries)
          ]
      body = vBox $ go (map section helpSections)
        where
          go [] = []
          go [s] = [s]
          go (s : ss) = s : txt " " : go ss
  render $ centerLayer $
    withBorderStyle unicodeBold $
    withAttr activeBorderAttr $
    borderWithLabel (withAttr brandAttr (txt " ✻ Agent Keyboard Guide ")) $
    padAll 2 $
    body
