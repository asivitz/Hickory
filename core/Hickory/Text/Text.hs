{-# LANGUAGE OverloadedRecordDot #-}

module Hickory.Text.Text where

import Hickory.Text.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Char (ord)
import Hickory.Math.Vector
import qualified Data.Text as Text
import Data.List
import Linear (V2(..), V3(..), (^*))
import Hickory.Text.ParseJson (Glyph (..), Font (..), GlyphVerts (..), Metrics (..), Atlas (..))
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Applicative ((<|>))

lineShiftX :: Fractional a => a -> XAlign -> a
lineShiftX width AlignRight = negate width
lineShiftX width AlignCenter = negate (width / 2)
lineShiftX _width AlignLeft = 0


kernGlyphs :: HashMap.HashMap (Int,Int) Scalar -> [(Glyph, Maybe GlyphVerts)] -> [((Glyph, Maybe GlyphVerts), Scalar)]
kernGlyphs _ [] = []
kernGlyphs kernMap glyphs = primary ++ [(last glyphs, 0)]
  where
  primary = zip glyphs (drop 1 glyphs) <&> \((g1,gv1), (g2, _)) -> ((g1, gv1), fromMaybe 0 $ HashMap.lookup (unicode g1, unicode g2) kernMap)

transformTextCommandToVerts :: TextCommand -> Font -> (Int, [V3 Scalar], [V2 Scalar])
transformTextCommandToVerts (TextCommand text align valign) Font {..}
  = (num_squares, posLstResult, tcLstResult)
  where
  Metrics {..} = metrics
  Atlas {..} = atlas

  yoffset :: Scalar = case valign of
    AlignMiddle      -> ((ascender + descender) / 2 - descender) / 2
    AlignBottom      -> 0
    AlignTop         -> (ascender + descender) / 2 - descender

  defaultGlyph = \case
    10 -> Just (Glyph 10 0 Nothing Nothing, Nothing)
    _  -> Nothing
  glyphs = mapMaybe (\c -> HashMap.lookup (ord c) glyphMap <|> defaultGlyph (ord c)) <$> lines (Text.unpack text)
  kernedGlyphs = kernGlyphs kerningMap <$> glyphs
  lineWidth kerned = sum $ kerned <&> \((g,_), k) -> advance g + k
  xoffset kerned = lineShiftX (realToFrac $ lineWidth kerned) align

  accum :: (Scalar, Int, Int, [V3 Scalar], [V2 Scalar]) -> [((Glyph, Maybe GlyphVerts), Scalar)] -> (Scalar, Int, Int, [V3 Scalar], [V2 Scalar])
  accum (leftBump, linenum, numsquares, posLst, tcLst) line = case layoutLine of
    (_, numsquares', posLst', tcLst') -> (leftBump, linenum + 1, numsquares + numsquares', posLst' ++ posLst, tcLst' ++ tcLst)
    where
    layoutLine = foldl' lineAccum (leftBump + xoffset line, 0, [], []) line
    lineAccum (left, num, pl, tl) ((Glyph {..}, gv), kerning) = case gv of
      Just GlyphVerts {..} ->
        let placeGlyph :: V2 Scalar -> V3 Scalar
            placeGlyph (V2 vx vy) = V3 (vx + left) (realToFrac linenum * realToFrac lineHeight + (vy + yoffset)) 0 ^* size
            new_verts = map placeGlyph verts
        in (left + (advance + kerning), num + 1, new_verts ++ pl, texCoords ++ tl)
      Nothing -> (left + advance, num, pl, tl)

  (_, _, num_squares, posLstResult, tcLstResult) = foldl' accum (0, 0, 0, [], []) kernedGlyphs
