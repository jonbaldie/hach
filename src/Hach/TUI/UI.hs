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
import Hach.Types (SessionTokenUsage(..), ToolResult(..), modelContextLimit)
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Exception (handle)
import Data.Aeson (Value, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as Vty
import Text.Printf (printf)
import Graphics.Vty.UnicodeWidthTable.Install (TableInstallException, installUnicodeWidthTable)
import Graphics.Vty.UnicodeWidthTable.Types (UnicodeWidthTable(..), WidthTableRange(..))

data Name
  = VpTranscript
  | VpInput
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
  if tsShowHelp
    then [helpOverlay, baseLayout state]
    else [baseLayout state]

-- | The core dashboard layout with Claude Code / AGY CLI proportions.
baseLayout :: TuiState -> Widget Name
baseLayout state =
  vBox
    [ renderHeader state
    , vLimitPercent 78 (renderTranscriptPanel state)
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
renderTokensDisplay :: Int -> Int -> Int -> UsageStatus -> Widget Name
renderTokensDisplay ctxTokens sesTokens limit status =
  let pct = if limit <= 0 then 0 else (ctxTokens * 100) `div` limit
      attr = if pct >= 90
               then tokenCritAttr
               else if pct >= 80
                      then tokenWarnAttr
                      else tokenAttr
      missingTag = case status of
        UsageMissing  -> " [?]"
        UsageVerified -> ""
      ctxText = "ctx: " <> formatCompactLimit ctxTokens <> "/" <> formatCompactLimit limit <> " (" <> T.pack (show pct) <> "%)" <> missingTag
      sesText = "ses: " <> formatCompactLimit sesTokens
  in hBox
       [ withAttr attr (txt ctxText)
       , withAttr dimAttr (txt " │ ")
       , withAttr tokenAttr (txt sesText)
       ]

-- | Modern, sleek status bar (Claude Code / AGY CLI style).
renderHeader :: TuiState -> Widget Name
renderHeader TuiState{..} =
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
    , renderTokensDisplay tsContextTokens (stuTotalTokens tsSessionTokens) (modelContextLimit tsModelName) tsUsageStatus
    , withAttr dimAttr (txt "  │  ")
    , renderStatus tsStatus
    , padLeft Max (withAttr dimAttr (txt "press ? for help "))
    ]

renderStatus :: TuiStatus -> Widget Name
renderStatus = \case
  StatusIdle          -> withAttr statusIdleAttr (txt "● idle")
  StatusThinking      -> withAttr statusThinkingAttr (txt "● thinking...")
  StatusRunningTool t -> withAttr statusRunningAttr (txt ("● running " <> t <> "..."))
  StatusFinished      -> withAttr statusIdleAttr (txt "✔ ready")
  StatusError err     -> withAttr statusErrorAttr (txt ("✖ error: " <> T.take 25 err))

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
      (_, revWidgets) = foldl' step (0, []) tsTranscript
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
  case Aeson.decodeStrict (TE.encodeUtf8 rawArgs) :: Maybe Value of
    Just (Aeson.Object o) ->
      case name of
        "read_file" ->
          case AesonTypes.parseMaybe (\obj -> obj .: "path") o of
            Just (p :: Text) -> p
            Nothing          -> truncateText 30 rawArgs
        "write_file" ->
          case AesonTypes.parseMaybe (\obj -> obj .: "path") o of
            Just (p :: Text) -> p
            Nothing          -> truncateText 30 rawArgs
        -- Alias sets mirror executeCodingTool's dispatch in Hach.Tools.
        n | n `elem` ["replace_file_content", "Edit", "edit"] ->
          case AesonTypes.parseMaybe (\obj -> obj .: "path") o of
            Just (p :: Text) -> p
            Nothing          -> truncateText 30 rawArgs
        n | n `elem` ["find_files", "Glob", "glob"] ->
          case AesonTypes.parseMaybe (\obj -> obj .: "pattern") o of
            Just (p :: Text) -> p
            Nothing          -> truncateText 30 rawArgs
        n | n `elem` ["grep_search", "Grep", "grep"] ->
          case AesonTypes.parseMaybe (\obj -> obj .: "query") o of
            Just (q :: Text) -> q
            Nothing          -> case AesonTypes.parseMaybe (\obj -> obj .: "pattern") o of
              Just (q :: Text) -> q
              Nothing          -> truncateText 30 rawArgs
        n | n `elem` ["run_command", "Bash", "bash"] ->
          case AesonTypes.parseMaybe (\obj -> obj .: "command") o of
            Just (c :: Text) -> c
            Nothing          -> case AesonTypes.parseMaybe (\obj -> obj .: "cmd") o of
              Just (c :: Text) -> c
              Nothing          -> truncateText 30 rawArgs
        n | n `elem` ["list_dir", "ListDir"] ->
          case AesonTypes.parseMaybe (\obj -> obj .: "path") o of
            Just (p :: Text) -> p
            Nothing          -> "."
        _ -> truncateText 30 rawArgs
    _ -> truncateText 30 rawArgs

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

-- | Modern prompt input bar.
-- Trailing slash-token completion (built-in commands and user-invocable
-- skills) is shown as dim ghost text; Tab accepts.
renderInputPanel :: TuiState -> Widget Name
renderInputPanel TuiState{..} =
  let isFocused = tsFocus == FocusInput
      borderMod = if isFocused then withAttr activeBorderAttr else withAttr inactiveBorderAttr
      borderGlyph = if isFocused then unicodeBold else unicodeRounded
      promptLabel = if isFocused then " [ ❯ Prompt (Active) ] " else " ❯ Prompt "
      prefix = withAttr userPromptAttr (txt "❯ ")
      body
        | T.null tsInputBuffer =
            prefix <+> withAttr dimAttr (txt "Type a task prompt and press Enter...")
        | otherwise =
            case inputSlashCompletion tsSkills builtinCommands tsInputBuffer of
              Just suffix ->
                prefix <+> hBox
                  [ withAttr userTextAttr (txt tsInputBuffer)
                  , withAttr dimAttr (txt suffix)
                  ]
              Nothing ->
                prefix <+> withAttr userTextAttr (txt tsInputBuffer)
      cursor = if isFocused
                 then showCursor VpInput (Location (T.length tsInputBuffer + 2, 0))
                 else id
  in borderMod $
     withBorderStyle borderGlyph $
     borderWithLabel (txt promptLabel) $
     padLeftRight 1 $
     cursor (padRight Max body)

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

-- | Help dialog overlay.
helpOverlay :: Widget Name
helpOverlay =
  center $
  withBorderStyle unicodeBold $
  withAttr activeBorderAttr $
  borderWithLabel (withAttr brandAttr (txt " ✻ Agent Keyboard Guide ")) $
  padAll 2 $
  vBox
    [ withAttr brandAttr (txt "Navigation & Global:")
    , padLeft (Pad 2) $ vBox
        [ hBox [withAttr shortcutKeyAttr (txt "Tab / Shift+Tab   "), withAttr dimAttr (txt "Switch focus (Input ⇄ Transcript)")]
        , hBox [withAttr shortcutKeyAttr (txt "Mouse wheel       "), withAttr dimAttr (txt "Scroll transcript without changing focus")]
        , hBox [withAttr shortcutKeyAttr (txt "Tab (in input)    "), withAttr dimAttr (txt "Accept a /skill autocomplete suggestion, else switch focus")]
        , hBox [withAttr shortcutKeyAttr (txt "Ctrl+Q            "), withAttr dimAttr (txt "Quit the application immediately")]
        , hBox [withAttr shortcutKeyAttr (txt "Esc / Ctrl+C      "), withAttr dimAttr (txt "Cancel running agent turn or dismiss help")]
        , hBox [withAttr shortcutKeyAttr (txt "? / F1            "), withAttr dimAttr (txt "Toggle this help overlay")]
        ]
    , txt " "
    , withAttr brandAttr (txt "Task Input:")
    , padLeft (Pad 2) $ vBox
        [ hBox [withAttr shortcutKeyAttr (txt "Enter             "), withAttr dimAttr (txt "Submit prompt to the autonomous agent")]
        , hBox [withAttr shortcutKeyAttr (txt "Up / Down         "), withAttr dimAttr (txt "Recall previous / next prompt history")]
        , hBox [withAttr shortcutKeyAttr (txt "PgUp / PgDn       "), withAttr dimAttr (txt "Scroll transcript 5 lines")]
        , hBox [withAttr shortcutKeyAttr (txt "Ctrl+U            "), withAttr dimAttr (txt "Clear current input line")]
        ]
    , txt " "
    , withAttr brandAttr (txt "Transcript:")
    , padLeft (Pad 2) $ vBox
        [ hBox [withAttr shortcutKeyAttr (txt "Up / Down         "), withAttr dimAttr (txt "Select tool card (when present) or scroll 1 line")]
        , hBox [withAttr shortcutKeyAttr (txt "PgUp / PgDn       "), withAttr dimAttr (txt "Scroll transcript 5 lines")]
        , hBox [withAttr shortcutKeyAttr (txt "Enter / Space     "), withAttr dimAttr (txt "Expand / collapse selected tool card")]
        , hBox [withAttr shortcutKeyAttr (txt "c                 "), withAttr dimAttr (txt "Clear transcript and reset context")]
        , hBox [withAttr shortcutKeyAttr (txt "q                 "), withAttr dimAttr (txt "Quit application (when idle)")]
        ]
    ]
