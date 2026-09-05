{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.TUI.UI
  ( drawUI
  , tuiAttrMap
  , formatTokens
  , Name(..)
  ) where

import Agent.TUI.Types
import Agent.Types (ToolResult(..))
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Aeson (Value, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as Vty

data Name
  = VpHistory
  | VpTools
  | VpInput
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Theme Attributes (Claude Code / AGY CLI Style)
--------------------------------------------------------------------------------

brandAttr, modelAttr, turnAttr, tokenAttr, dimAttr :: AttrName
brandAttr = attrName "brand"
modelAttr = attrName "model"
turnAttr  = attrName "turn"
tokenAttr = attrName "token"
dimAttr   = attrName "dim"

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
    , vLimitPercent 78 (hBox [hLimitPercent 62 (renderHistoryPanel state), vBorder, renderToolsPanel state])
    , renderInputPanel state
    , renderFooter
    ]

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Format an integer token count with comma thousands separators.
formatTokens :: Int -> Text
formatTokens n
  | n < 0     = "-" <> formatTokens (abs n)
  | n < 1000  = T.pack (show n)
  | otherwise = formatTokens (n `div` 1000) <> "," <> padThree (n `mod` 1000)
  where
    padThree x =
      let s = show x
      in T.pack (replicate (3 - length s) '0' ++ s)

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
    , withAttr turnAttr (txt (T.pack (show tsCurrentTurn) <> "/" <> T.pack (show tsMaxTurns)))
    , withAttr dimAttr (txt "  │  tokens: ")
    , withAttr tokenAttr (txt (formatTokens tsContextTokens))
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
-- Dialogue History Panel
--------------------------------------------------------------------------------

-- | Left panel showing conversation dialogue with markdown code styling.
renderHistoryPanel :: TuiState -> Widget Name
renderHistoryPanel TuiState{..} =
  let isFocused = tsFocus == FocusHistory
      borderMod = if isFocused then withAttr activeBorderAttr else withAttr inactiveBorderAttr
      borderGlyph = if isFocused then unicodeBold else unicodeRounded
      headerText = if isFocused
                     then " [ 💬 Dialogue History (Active) ] "
                     else " 💬 Dialogue History "
      items = if null tsHistory
                then [padAll 1 (withAttr dimAttr (txtWrap "No dialogue yet. Type a prompt below and press Enter to start."))]
                else map renderDialogue tsHistory
  in borderMod $
     withBorderStyle borderGlyph $
     borderWithLabel (txt headerText) $
     viewport VpHistory Vertical (vBox items)

renderDialogue :: DialogueItem -> Widget Name
renderDialogue = \case
  DiUser u ->
    padBottom (Pad 1) $
    vBox
      [ hBox
          [ withAttr userPromptAttr (txt "❯ ")
          , withAttr userAttr (txt "You")
          ]
      , padLeft (Pad 2) $
        withAttr userTextAttr (txtWrap u)
      ]

  DiAssistant a ->
    padBottom (Pad 1) $
    vBox
      [ hBox
          [ withAttr asstAttr (txt "✦ ")
          , withAttr asstAttr (txt "Assistant")
          ]
      , padLeft (Pad 2) $
        renderAssistantBody a
      ]

  DiSystem s ->
    padBottom (Pad 1) $
    padLeft (Pad 2) $
    withAttr sysAttr (txtWrap ("⚙ " <> s))

  DiNotice n ->
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

--------------------------------------------------------------------------------
-- Tool Activity Panel (Claude Code Activity Stream)
--------------------------------------------------------------------------------

-- | Right panel showing active and past tool calls.
renderToolsPanel :: TuiState -> Widget Name
renderToolsPanel TuiState{..} =
  let isFocused = tsFocus == FocusTools
      borderMod = if isFocused then withAttr activeBorderAttr else withAttr inactiveBorderAttr
      borderGlyph = if isFocused then unicodeBold else unicodeRounded
      total = length tsTools
      headerText = if isFocused
                     then " [ ⚡ Tool Activity (" <> T.pack (show total) <> ") (Active) ] "
                     else " ⚡ Tool Activity (" <> T.pack (show total) <> ") "
      cards = if null tsTools
                then [padAll 1 (withAttr dimAttr (txtWrap "No tools executed yet. Tool calls will stream here."))]
                else zipWith (renderToolCard tsSelectedToolIndex isFocused) [0..] tsTools
  in borderMod $
     withBorderStyle borderGlyph $
     borderWithLabel (txt headerText) $
     viewport VpTools Vertical (vBox cards)

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
        "run_command" ->
          case AesonTypes.parseMaybe (\obj -> obj .: "cmd") o of
            Just (c :: Text) -> c
            Nothing          -> truncateText 30 rawArgs
        "list_dir" ->
          case AesonTypes.parseMaybe (\obj -> obj .: "path") o of
            Just (p :: Text) -> p
            Nothing          -> "."
        _ -> truncateText 30 rawArgs
    _ -> truncateText 30 rawArgs

truncateText :: Int -> Text -> Text
truncateText maxLen t
  | T.length t > maxLen = T.take maxLen t <> "..."
  | otherwise           = t

formatResultSummary :: Maybe ToolResult -> (Text, AttrName)
formatResultSummary = \case
  Nothing ->
    ("◌ running...", statusThinkingAttr)
  Just (ToolSuccess out) ->
    let chars = T.length out
        linesCount = length (T.lines out)
        tag = if linesCount > 1
                then "✓ success (" <> T.pack (show linesCount) <> " lines)"
                else "✓ success (" <> T.pack (show chars) <> " chars)"
    in (tag, toolSuccessAttr)
  Just (ToolError err) ->
    ("✖ failed (" <> T.take 25 err <> ")", toolErrorAttr)

renderToolCard :: Int -> Bool -> Int -> ToolItem -> Widget Name
renderToolCard selectedIdx isToolsFocused idx ToolItem{..} =
  let isSelected = isToolsFocused && selectedIdx == idx
      cursorMark = if isSelected then withAttr userPromptAttr (txt "▸ ") else txt "  "
      icon = withAttr toolIconAttr (txt "⏺ ")
      nameWidget = withAttr toolNameAttr (txt tiName)
      targetText = formatToolTarget tiName tiArgs
      targetWidget = if T.null targetText
                       then emptyWidget
                       else withAttr toolTargetAttr (txt (" " <> targetText))
      (statusTxt, statusAttr) = formatResultSummary tiResult
      statusWidget = withAttr statusAttr (txt statusTxt)

      headerLine = hBox [cursorMark, icon, nameWidget, targetWidget]
      subLine = padLeft (Pad 4) $
                hBox
                  [ withAttr dimAttr (txt "⎿  ")
                  , statusWidget
                  , if tiExpanded
                      then withAttr dimAttr (txt "  (expanded)")
                      else withAttr dimAttr (txt "  (↵ details)")
                  ]

      expandedBody = if tiExpanded
        then padLeft (Pad 4) $
             padTop (Pad 1) $
             vBox
               [ withBorderStyle unicodeRounded $
                 borderWithLabel (withAttr dimAttr (txt " Arguments ")) $
                 padLeftRight 1 (withAttr codeBlockAttr (txtWrap tiArgs))
               , case tiResult of
                   Nothing ->
                     padTop (Pad 1) (withAttr statusThinkingAttr (txt "Waiting for execution output..."))
                   Just (ToolSuccess out) ->
                     padTop (Pad 1) $
                     withBorderStyle unicodeRounded $
                     borderWithLabel (withAttr toolSuccessAttr (txt " Output ")) $
                     padLeftRight 1 (withAttr codeBlockAttr (txtWrap (if T.null out then "(empty output)" else out)))
                   Just (ToolError err) ->
                     padTop (Pad 1) $
                     withBorderStyle unicodeRounded $
                     borderWithLabel (withAttr toolErrorAttr (txt " Error ")) $
                     padLeftRight 1 (withAttr toolErrorAttr (txtWrap err))
               ]
        else emptyWidget

      cardWidget =
        padBottom (Pad 1) $
        vBox [headerLine, subLine, expandedBody]

  in if isSelected then visible cardWidget else cardWidget

--------------------------------------------------------------------------------
-- Task Input Panel
--------------------------------------------------------------------------------

-- | Modern prompt input bar.
renderInputPanel :: TuiState -> Widget Name
renderInputPanel TuiState{..} =
  let isFocused = tsFocus == FocusInput
      borderMod = if isFocused then withAttr activeBorderAttr else withAttr inactiveBorderAttr
      borderGlyph = if isFocused then unicodeBold else unicodeRounded
      promptLabel = if isFocused then " [ ❯ Prompt (Active) ] " else " ❯ Prompt "
      prefix = withAttr userPromptAttr (txt "❯ ")
      body = if T.null tsInputBuffer
               then prefix <+> withAttr dimAttr (txt "Type a task prompt and press Enter...")
               else prefix <+> withAttr userTextAttr (txt tsInputBuffer)
      cursor = if isFocused
                 then showCursor VpInput (Location (T.length tsInputBuffer + 2, 0))
                 else id
  in borderMod $
     withBorderStyle borderGlyph $
     borderWithLabel (txt promptLabel) $
     padLeftRight 1 $
     cursor body

--------------------------------------------------------------------------------
-- Footer & Help
--------------------------------------------------------------------------------

-- | Quiet, minimalist shortcut bar.
renderFooter :: Widget Name
renderFooter =
  hCenter $
  hBox
    [ withAttr shortcutKeyAttr (txt "⇥ tab")
    , withAttr dimAttr (txt " panels  •  ")
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
        [ hBox [withAttr shortcutKeyAttr (txt "Tab / Shift+Tab   "), withAttr dimAttr (txt "Switch panel focus (Input ⇄ History ⇄ Tools)")]
        , hBox [withAttr shortcutKeyAttr (txt "Ctrl+Q            "), withAttr dimAttr (txt "Quit the application immediately")]
        , hBox [withAttr shortcutKeyAttr (txt "Esc / Ctrl+C      "), withAttr dimAttr (txt "Cancel running agent turn or dismiss help")]
        , hBox [withAttr shortcutKeyAttr (txt "? / F1            "), withAttr dimAttr (txt "Toggle this help overlay")]
        ]
    , txt " "
    , withAttr brandAttr (txt "Task Input:")
    , padLeft (Pad 2) $ vBox
        [ hBox [withAttr shortcutKeyAttr (txt "Enter             "), withAttr dimAttr (txt "Submit prompt to the autonomous agent")]
        , hBox [withAttr shortcutKeyAttr (txt "Up / Down         "), withAttr dimAttr (txt "Recall previous / next prompt history")]
        , hBox [withAttr shortcutKeyAttr (txt "Ctrl+U            "), withAttr dimAttr (txt "Clear current input line")]
        ]
    , txt " "
    , withAttr brandAttr (txt "Dialogue History:")
    , padLeft (Pad 2) $ vBox
        [ hBox [withAttr shortcutKeyAttr (txt "Up / Down         "), withAttr dimAttr (txt "Scroll conversation 1 line")]
        , hBox [withAttr shortcutKeyAttr (txt "PgUp / PgDn       "), withAttr dimAttr (txt "Scroll conversation 5 lines")]
        , hBox [withAttr shortcutKeyAttr (txt "c                 "), withAttr dimAttr (txt "Clear conversation history")]
        , hBox [withAttr shortcutKeyAttr (txt "q                 "), withAttr dimAttr (txt "Quit application (when idle)")]
        ]
    , txt " "
    , withAttr brandAttr (txt "Tool Activity:")
    , padLeft (Pad 2) $ vBox
        [ hBox [withAttr shortcutKeyAttr (txt "Up / Down         "), withAttr dimAttr (txt "Navigate tool execution cards")]
        , hBox [withAttr shortcutKeyAttr (txt "Enter / Space     "), withAttr dimAttr (txt "Expand / collapse tool arguments & outputs")]
        , hBox [withAttr shortcutKeyAttr (txt "q                 "), withAttr dimAttr (txt "Quit application (when idle)")]
        ]
    ]
