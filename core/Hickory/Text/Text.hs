module Hickory.Text.Text where

import Hickory.Text.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Char (ord)
import Hickory.Math.Vector
import qualified Data.Text as Text
import Data.List
import Linear (V2(..), V3(..))
import Hickory.Text.ParseJson (Glyph (..), Font (..), GlyphVerts (..), Metrics (..), Atlas (..))
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Applicative ((<|>))

lineShiftX :: Fractional a => a -> XAlign -> a
lineShiftX width AlignRight = negate width
lineShiftX width AlignCenter = negate (width / 2)
lineShiftX _width AlignLeft = 0

lineShiftY :: (Fractional b) => b -> YAlign -> b
lineShiftY _ AlignMiddle      = 0
lineShiftY _ AlignLowerMiddle = 48 - 12
lineShiftY height AlignBottom = height
lineShiftY _ AlignTop = 0

kernGlyphs :: [(Glyph, Maybe GlyphVerts)] -> HashMap.HashMap (Int,Int) Scalar -> [((Glyph, Maybe GlyphVerts), Scalar)]
kernGlyphs glyphs kernMap = primary ++ [(last glyphs, 0)]
  where
  primary = zip glyphs (drop 1 glyphs) <&> \((g1,gv1), (g2, _)) -> ((g1, gv1), fromMaybe 0 $ HashMap.lookup (unicode g1, unicode g2) kernMap)

transformTextCommandToVerts :: TextCommand -> Font -> (Int, [V3 Scalar], [V2 Scalar])
transformTextCommandToVerts (TextCommand text align valign) Font {..}
  = (num_squares, posLstResult, tcLstResult)
  where
  Metrics {..} = metrics
  Atlas {..} = atlas
  defaultGlyph = \case
    10 -> Just (Glyph 10 0 Nothing Nothing, Nothing)
    _  -> Nothing
  glyphs = mapMaybe (\c -> HashMap.lookup (ord c) glyphMap <|> defaultGlyph (ord c)) (Text.unpack text)
  kernedGlyphs = kernGlyphs glyphs kerningMap
  fullwidth = (size*) . sum $ kernedGlyphs <&> \((g,_), k) -> advance g + k
  xoffset = lineShiftX (realToFrac fullwidth) align
  yoffset = lineShiftY (realToFrac lineHeight) valign
  accum :: (Scalar, Int, Int, [V3 Scalar], [V2 Scalar]) -> ((Glyph, Maybe GlyphVerts), Scalar) -> (Scalar, Int, Int, [V3 Scalar], [V2 Scalar])
  accum = \(leftBump, linenum, numsquares, posLst, tcLst) ((Glyph {..}, gv), kerning) -> case unicode of
    10 -> (0, linenum + 1, numsquares, posLst, tcLst)
    _  -> case gv of
      Just GlyphVerts {..} ->
        let placeGlyph :: V2 Scalar -> V3 Scalar
            placeGlyph = \(V2 vx vy) -> V3 (vx + leftBump)
                                          (realToFrac linenum * realToFrac lineHeight * size + (vy + yoffset))
                                          0
            new_verts = map placeGlyph verts
        in (leftBump + (advance + kerning) * size, linenum, numsquares + 1, new_verts ++ posLst, texCoords ++ tcLst)
      Nothing -> (leftBump + advance * size, linenum, numsquares, posLst, tcLst)
  (_, _, num_squares, posLstResult, tcLstResult) = foldl' accum (xoffset, 0, 0, [], []) kernedGlyphs
