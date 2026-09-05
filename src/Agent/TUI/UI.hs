{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.TUI.UI
  ( drawUI
  , tuiAttrMap
  , Name(..)
  ) where

import Agent.TUI.Types
import Agent.Types (ToolResult(..))
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

data Name
  = VpHistory
  | VpTools
  | VpInput
  deriving (Show, Eq, Ord)

-- Attribute Names
titleAttr, modelAttr, turnAttr :: AttrName
titleAttr = attrName "title"
modelAttr = attrName "model"
turnAttr  = attrName "turn"

statusIdleAttr, statusThinkingAttr, statusRunningAttr, statusErrorAttr :: AttrName
statusIdleAttr     = attrName "statusIdle"
statusThinkingAttr = attrName "statusThinking"
statusRunningAttr  = attrName "statusRunning"
statusErrorAttr    = attrName "statusError"

userAttr, asstAttr, sysAttr, noticeAttr, toolNameAttr, shortcutAttr, activeBorderAttr :: AttrName
userAttr         = attrName "user"
asstAttr         = attrName "assistant"
sysAttr          = attrName "system"
noticeAttr       = attrName "notice"
toolNameAttr     = attrName "toolName"
shortcutAttr     = attrName "shortcut"
activeBorderAttr = attrName "activeBorder"

tuiAttrMap :: AttrMap
tuiAttrMap = attrMap Vty.defAttr
  [ (titleAttr,          Vty.withStyle (fg Vty.cyan) Vty.bold)
  , (modelAttr,          Vty.withStyle (fg Vty.magenta) Vty.bold)
  , (turnAttr,           fg Vty.yellow)
  , (statusIdleAttr,     Vty.withStyle (fg Vty.green) Vty.bold)
  , (statusThinkingAttr, Vty.withStyle (fg Vty.yellow) Vty.bold)
  , (statusRunningAttr,  Vty.withStyle (fg Vty.cyan) Vty.bold)
  , (statusErrorAttr,    Vty.withStyle (fg Vty.red) Vty.bold)
  , (userAttr,           Vty.withStyle (fg Vty.cyan) Vty.bold)
  , (asstAttr,           Vty.withStyle (fg Vty.green) Vty.bold)
  , (sysAttr,            fg Vty.yellow)
  , (noticeAttr,         fg Vty.red)
  , (toolNameAttr,       Vty.withStyle (fg Vty.blue) Vty.bold)
  , (shortcutAttr,       fg (Vty.rgbColor (120 :: Int) (120 :: Int) (120 :: Int)))
  , (activeBorderAttr,   Vty.withStyle (fg Vty.cyan) Vty.bold)
  ]

-- | Draw the full TUI layout.
drawUI :: TuiState -> [Widget Name]
drawUI state@TuiState{..} =
  if tsShowHelp
    then [helpOverlay, baseLayout state]
    else [baseLayout state]

-- | The core dashboard layout.
baseLayout :: TuiState -> Widget Name
baseLayout state =
  vBox
    [ renderHeader state
    , vLimitPercent 75 (hBox [renderHistoryPanel state, vBorder, renderToolsPanel state])
    , renderInputPanel state
    , renderFooter
    ]

-- | Header bar showing app title, model, turns, and live status.
renderHeader :: TuiState -> Widget Name
renderHeader TuiState{..} =
  withBorderStyle unicodeRounded $
  border $
  hBox
    [ withAttr titleAttr (txt " 🤖 Haskell Coding Agent ")
    , padLeft Max (txt "Model: ")
    , withAttr modelAttr (txt tsModelName)
    , txt "  │ Turn: "
    , withAttr turnAttr (txt (T.pack (show tsCurrentTurn) <> "/" <> T.pack (show tsMaxTurns)))
    , txt "  │ Status: "
    , renderStatus tsStatus
    , txt " "
    ]

renderStatus :: TuiStatus -> Widget Name
renderStatus = \case
  StatusIdle            -> withAttr statusIdleAttr (txt "IDLE")
  StatusThinking        -> withAttr statusThinkingAttr (txt "THINKING...")
  StatusRunningTool t   -> withAttr statusRunningAttr (txt ("RUNNING TOOL: " <> t))
  StatusFinished        -> withAttr statusIdleAttr (txt "FINISHED")
  StatusError err       -> withAttr statusErrorAttr (txt ("ERROR: " <> T.take 30 err))

-- | Left panel showing the dialogue history.
renderHistoryPanel :: TuiState -> Widget Name
renderHistoryPanel TuiState{..} =
  let isFocused = tsFocus == FocusHistory
      borderMod = if isFocused then withAttr activeBorderAttr else id
      items = if null tsHistory
                then [txtWrap "No dialogue yet. Type a task below and press Enter."]
                else map renderDialogue tsHistory
  in borderMod $
     withBorderStyle unicodeRounded $
     borderWithLabel (txt (if isFocused then " [ Dialogue History (Active) ] " else " Dialogue History ")) $
     viewport VpHistory Vertical (vBox items)

renderDialogue :: DialogueItem -> Widget Name
renderDialogue = \case
  DiUser u ->
    padBottom (Pad 1) $
    vBox [ withAttr userAttr (txt "👤 User:")
         , padLeft (Pad 2) (txtWrap u)
         ]
  DiAssistant a ->
    padBottom (Pad 1) $
    vBox [ withAttr asstAttr (txt "🤖 Assistant:")
         , padLeft (Pad 2) (txtWrap a)
         ]
  DiSystem s ->
    padBottom (Pad 1) $
    vBox [ withAttr sysAttr (txt "⚙️ System:")
         , padLeft (Pad 2) (txtWrap s)
         ]
  DiNotice n ->
    padBottom (Pad 1) $
    withAttr noticeAttr (txtWrap ("⚠️ " <> n))

-- | Right panel showing the active/past tool calls.
renderToolsPanel :: TuiState -> Widget Name
renderToolsPanel TuiState{..} =
  let isFocused = tsFocus == FocusTools
      borderMod = if isFocused then withAttr activeBorderAttr else id
      total = length tsTools
      headerText = " Tool Activity (" <> T.pack (show total) <> ")" <> (if isFocused then " (Active) " else " ")
      cards = if null tsTools
                then [txtWrap "No tools executed yet."]
                else zipWith (renderToolCard tsSelectedToolIndex isFocused) [0..] tsTools
  in borderMod $
     withBorderStyle unicodeRounded $
     borderWithLabel (txt headerText) $
     viewport VpTools Vertical (vBox cards)

renderToolCard :: Int -> Bool -> Int -> ToolItem -> Widget Name
renderToolCard selectedIdx isToolsFocused idx ToolItem{..} =
  let isSelected = isToolsFocused && selectedIdx == idx
      cardStyle  = if isSelected then unicodeBold else unicodeRounded
      statusTag  = case tiResult of
        Nothing                 -> withAttr statusRunningAttr (txt " [RUNNING]")
        Just (ToolSuccess _)    -> withAttr statusIdleAttr (txt " [SUCCESS]")
        Just (ToolError _)      -> withAttr statusErrorAttr (txt " [FAILED]")
      header = hBox [withAttr toolNameAttr (txt (" " <> tiName <> " ")), statusTag]
      body = if tiExpanded
        then vBox
          [ txt ("Arguments: " <> tiArgs)
          , case tiResult of
              Nothing -> emptyWidget
              Just (ToolSuccess out) ->
                padTop (Pad 1) $
                vBox [withAttr statusIdleAttr (txt "Output:"), padLeft (Pad 2) (txtWrap out)]
              Just (ToolError err) ->
                padTop (Pad 1) $
                vBox [withAttr statusErrorAttr (txt "Error:"), padLeft (Pad 2) (txtWrap err)]
          ]
        else vBox
          [ txt ("Args: " <> if T.length tiArgs > 35 then T.take 35 tiArgs <> "..." else tiArgs)
          , withAttr shortcutAttr (txt "(Press Enter/Space to expand)")
          ]
      cardWidget =
        padBottom (Pad 1) $
        withBorderStyle cardStyle $
        borderWithLabel header $
        padAll 1 body
  in if isSelected then visible cardWidget else cardWidget

-- | Bottom panel for user prompt typing.
renderInputPanel :: TuiState -> Widget Name
renderInputPanel TuiState{..} =
  let isFocused = tsFocus == FocusInput
      borderMod = if isFocused then withAttr activeBorderAttr else id
      content = if T.null tsInputBuffer
                  then withAttr shortcutAttr (txt "Type a task prompt and press Enter...")
                  else txt tsInputBuffer
      cursor = if isFocused
                 then showCursor VpInput (Location (T.length tsInputBuffer, 0))
                 else id
  in borderMod $
     withBorderStyle unicodeRounded $
     borderWithLabel (txt (if isFocused then " [ Task Input (Focused) ] " else " Task Input ")) $
     padAll 1 $
     cursor content

-- | Footer line showing key shortcuts.
renderFooter :: Widget Name
renderFooter =
  hCenter $
  withAttr shortcutAttr $
  txt "[Enter] Send  │  [Tab] Switch Panel  │  [Esc/Ctrl+C] Cancel  │  [?] Help  │  [Ctrl+Q] Quit"

-- | Help dialog overlay.
helpOverlay :: Widget Name
helpOverlay =
  center $
  withBorderStyle unicodeBold $
  borderWithLabel (txt " Keyboard Shortcuts ") $
  padAll 2 $
  vBox
    [ withAttr titleAttr (txt "Navigation & Global:")
    , txt "  Tab / BackTab   Switch focus between panels (Input, History, Tools)"
    , txt "  Ctrl+Q          Quit the application"
    , txt "  Esc / Ctrl+C    Cancel ongoing agent turn or close help"
    , txt "  ?               Toggle this help dialog"
    , txt " "
    , withAttr titleAttr (txt "Input Panel:")
    , txt "  Enter           Submit task prompt to the agent"
    , txt "  Ctrl+U          Clear input buffer"
    , txt " "
    , withAttr titleAttr (txt "History Panel:")
    , txt "  Up / Down       Scroll conversation lines"
    , txt "  PageUp / PageDn Scroll conversation pages"
    , txt "  c               Clear conversation history"
    , txt " "
    , withAttr titleAttr (txt "Tools Panel:")
    , txt "  Up / Down       Select tool card"
    , txt "  Enter / Space   Expand or collapse tool output details"
    ]
