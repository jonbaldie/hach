{-# LANGUAGE OverloadedStrings #-}

-- | Layout regression tests for the TUI border alignment.
--
-- These render the real widget tree headlessly and re-measure each row the way
-- a terminal does, rather than the way vty does. The two only agree once
-- 'installWideGlyphWidths' has taught vty the true width of the UI's
-- double-width glyphs; without it the panel borders drift one column right on
-- every row containing 💬, ⚡ or ⏺.
module Agent.TUILayoutSpec (spec) where

import Agent.TUI.Types
import Agent.TUI.UI (drawUI, installWideGlyphWidths, tuiAttrMap, wideGlyphs)
import Agent.Types (ToolResult(..))
import qualified Brick.Main as M
import Data.List (group, nub, sort, sortOn)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Graphics.Text.Width (safeWcwidth)
import qualified Graphics.Vty.PictureToSpans as PTS
import qualified Graphics.Vty.Span as Span
import Test.Hspec

-- | A screen with content in both panels, including a tool card (which carries
-- the ⏺ bullet) and both panel labels (💬 and ⚡).
sampleState :: TuiState
sampleState = (initialTuiState "test/model" 10)
  { tsCurrentTurn = 3
  , tsStatus      = StatusThinking
  , tsFocus       = FocusHistory
  , tsHistory     = [DiUser "hello", DiAssistant "hi"]
  , tsTools       =
      [ ToolItem "list_dir" "{\"path\":\".\"}" (Just (ToolSuccess "ok")) False
      , ToolItem "run_command" "{\"command\":\"wc -l ./src/Agent/Tools.hs\"}"
          (Just (ToolSuccess "ok")) False
      ]
  }

-- | Render the UI and flatten each row to the characters vty places, in vty's
-- own column order.
renderRows :: (Int, Int) -> [T.Text]
renderRows region =
  let pic = M.renderWidget (Just tuiAttrMap) (drawUI sampleState) region
  in map flattenRow (V.toList (PTS.displayOpsForPic pic region))
  where
    flattenRow = V.foldl' step ""
    step acc op = case op of
      Span.TextSpan _ _ _ t -> acc <> TL.toStrict t
      Span.Skip n           -> acc <> T.replicate n " "
      Span.RowEnd n         -> acc <> T.replicate n " "

-- | Width of a character as the *terminal* draws it: the glyphs the UI declares
-- wide take two columns, everything else takes vty's width.
terminalWidth :: Char -> Int
terminalWidth c
  | c `elem` wideGlyphs = 2
  | otherwise           = max 0 (safeWcwidth c)

borderGlyphs :: String
borderGlyphs = "│┃╎╏║╽╿┐┓╮┘┛╯┌┏╭└┗╰"

-- | Terminal columns at which a row places a vertical/corner border glyph.
borderColumns :: T.Text -> [Int]
borderColumns = reverse . snd . T.foldl' step (0, [])
  where
    step (col, acc) c =
      let acc' = if c `elem` borderGlyphs then col : acc else acc
      in (col + terminalWidth c, acc')

-- | The panel band: the rows carrying the three inter-panel verticals. They are
-- identified by the most common border-glyph count among rows that have at
-- least four, which excludes the header/footer bars (different counts).
panelBand :: [T.Text] -> [[Int]]
panelBand rows =
  let cols   = map borderColumns rows
      counts = [length c | c <- cols, length c >= 4]
      modal  = head (last (sortOn length (group (sort counts))))
  in [c | c <- cols, length c == modal]

spec :: Spec
spec = do
  describe "installWideGlyphWidths" $ do
    it "gives the UI's wide glyphs their real two-column width" $ do
      installWideGlyphWidths
      map safeWcwidth wideGlyphs `shouldBe` map (const 2) wideGlyphs

    it "leaves other characters' widths alone" $ do
      installWideGlyphWidths
      map safeWcwidth "aX│─ ●✦⚙" `shouldBe` replicate 8 1
      safeWcwidth '\x4E16' `shouldBe` 2  -- CJK stays wide

  describe "panel borders" $ do
    it "land in identical terminal columns on every row of the panel band" $ do
      let region@(cols, _) = (259, 54)
          band = panelBand (renderRows region)
      band `shouldSatisfy` (not . null)
      nub band `shouldBe` [head band]
      -- and nothing is pushed off the right-hand edge
      concatMap (take 1 . reverse) band `shouldSatisfy` all (< cols)

    it "stays aligned at other terminal widths" $
      mapM_ (\cols -> do
        let band = panelBand (renderRows (cols, 40))
        (cols, nub band) `shouldBe` (cols, [head band]))
        [80, 100, 120, 160, 200, 259]

  describe "glyph inventory" $ do
    it "renders no unclassified double-width glyph" $ do
      let chars = nub (concatMap T.unpack (renderRows (259, 54)))
          -- every glyph the UI is allowed to draw, all of them one column
          -- wide apart from those declared in 'wideGlyphs'
          known = wideGlyphs ++ " —•↵⇄⇥⎿│─┌┐└┘"
                             ++ "━┃┏┓┗┛╭╮╯╰"
                             ++ "▸◌●⚙✓✔✖✦✻❯"
          stray = sort [c | c <- chars, c > '\x7F', c `notElem` known]
      stray `shouldBe` []
