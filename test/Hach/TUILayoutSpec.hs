{-# LANGUAGE OverloadedStrings #-}

-- | Layout regression tests for the TUI border alignment.
--
-- These render the real widget tree headlessly and re-measure each row the way
-- a terminal does, rather than the way vty does. The two only agree once
-- 'installWideGlyphWidths' has taught vty the true width of the UI's
-- double-width glyphs; without it the panel borders drift one column right on
-- every row containing 💬, ⚡ or ⏺.
module Hach.TUILayoutSpec (spec) where

import Hach.TUI.Types
import Hach.TUI.UI (drawUI, installWideGlyphWidths, tuiAttrMap, wideGlyphs)
import Hach.Types (ToolResult(..))
import qualified Brick.Main as M
import Data.List (nub, sort)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Graphics.Text.Width (safeWcwidth)
import qualified Graphics.Vty.PictureToSpans as PTS
import qualified Graphics.Vty.Span as Span
import Test.Hspec

-- | A screen with unified transcript, including a tool card (which carries
-- the ⏺ bullet) and transcript panel label (💬). Parameterised over the turn
-- limit so both header forms are covered: a turn count, and the unlimited ∞.
sampleState :: Maybe Int -> TuiState
sampleState maxTurns = (initialTuiState "test/model" maxTurns)
  { tsCurrentTurn = 3
  , tsStatus      = StatusThinking
  , tsFocus       = FocusTranscript
  , tsTranscript  =
      [ TiUser "hello"
      , TiAssistant "hi"
      , TiToolCard (ToolCard "call-1" "list_dir" "{\"path\":\".\"}" (Finished (ToolSuccess "ok")) False)
      , TiToolCard (ToolCard "call-2" "run_command" "{\"command\":\"wc -l ./src/Hach/Tools.hs\"}"
          (Finished (ToolSuccess "ok")) False)
      ]
  }

-- | Render the UI and flatten each row to the characters vty places, in vty's
-- own column order.
renderRows :: Maybe Int -> (Int, Int) -> [T.Text]
renderRows maxTurns region =
  let pic = M.renderWidget (Just tuiAttrMap) (drawUI (sampleState maxTurns)) region
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

-- | The panel band: the rows carrying the single-column panel verticals.
-- In a single column layout, every row with panel borders places them at [0, cols - 1].
panelBand :: [T.Text] -> [[Int]]
panelBand rows =
  let cols = map borderColumns rows
  in [c | c <- cols, length c == 2]

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
          band = panelBand (renderRows (Just 10) region)
      band `shouldSatisfy` (not . null)
      nub band `shouldBe` [[0, cols - 1]]
      -- and nothing is pushed off the right-hand edge
      concatMap (take 1 . reverse) band `shouldSatisfy` all (< cols)

    it "stays aligned at other terminal widths" $
      mapM_ (\cols -> do
        let band = panelBand (renderRows (Just 10) (cols, 40))
        (cols, nub band) `shouldBe` (cols, [[0, cols - 1]]))
        [80, 100, 120, 160, 200, 259]

    it "stays aligned with an unlimited turn count" $
      mapM_ (\cols -> do
        let band = panelBand (renderRows Nothing (cols, 40))
        (cols, nub band) `shouldBe` (cols, [[0, cols - 1]]))
        [80, 120, 259]

  describe "glyph inventory" $ do
    it "renders no unclassified double-width glyph" $ do
      let chars = nub (concatMap T.unpack
                        (renderRows (Just 10) (259, 54) ++ renderRows Nothing (259, 54)))
          -- every glyph the UI is allowed to draw, all of them one column
          -- wide apart from those declared in 'wideGlyphs'
          known = wideGlyphs ++ " —•↵⇄⇥⎿│─┌┐└┘"
                             ++ "━┃┏┓┗┛╭╮╯╰"
                             ++ "▸◌●⚙✓✔✖✦✻❯∞"
          stray = sort [c | c <- chars, c > '\x7F', c `notElem` known]
      stray `shouldBe` []
