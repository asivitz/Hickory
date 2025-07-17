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
import Control.Lens (_1, _2, _3, over, sumOf, each, toListOf)

lineShiftX :: Fractional a => a -> XAlign -> a
lineShiftX width AlignRight = negate width
lineShiftX width AlignCenter = negate (width / 2)
lineShiftX _width AlignLeft = 0

kernGlyphs :: HashMap.HashMap (Int,Int) Scalar -> [(Glyph, Maybe GlyphVerts)] -> [((Glyph, Maybe GlyphVerts), Scalar)]
kernGlyphs _ [] = []
kernGlyphs kernMap glyphs = primary ++ [(last glyphs, 0)]
  where
  primary = zip glyphs (drop 1 glyphs) <&> \((g1,gv1), (g2, _)) -> ((g1, gv1), fromMaybe 0 $ HashMap.lookup (unicode g1, unicode g2) kernMap)

data PrintedLine = PrintedLine
  { printedVerts :: [V3 Scalar]
  , printedTCs   :: [V2 Scalar]
  , numPrinted :: Word
  , xOffset :: Scalar -- Where to position the next character laterally
  }

type Kerned = ((Glyph, Maybe GlyphVerts), Scalar)

transformTextCommandToVerts :: TextCommand -> Font -> (Int, [V3 Scalar], [V2 Scalar])
transformTextCommandToVerts (TextCommand text align valign mCutoff mCursor mWrapWidth) Font {..}
  = (allNum, allVerts, allTCs)
  where
  wordSplit = Text.splitOn " " <$> Text.splitOn "\n" text
  kernedWordGlyphs :: [[[Kerned]]]
  kernedWordGlyphs =
    wordSplit <&> \line ->
      let kerned = line <&> \word ->
            kernGlyphs kerningMap (mapMaybe (\c -> HashMap.lookup (ord c) glyphMap) (Text.unpack word))
      in case mWrapWidth of
        Just ww -> layoutWrapped ww (0, []) kerned
        Nothing -> [intercalate [(spaceGlyph, 0)] kerned]

  -- Add a placeholder for the cursor onto each line
  ended = kernedWordGlyphs <&> fmap (\line -> line ++ [lineEnder])

  wordWidthf kerned = sum $ kerned <&> \((g,_), k) -> g.advance + k
  layoutWrapped :: Scalar -> (Scalar, [[Kerned]]) -> [[Kerned]] -> [[Kerned]]
  layoutWrapped wrapWidth (xoff, printed) = \case
   [] -> reverse (map reverse printed)
   (w:ws) ->
     let ww = wordWidthf w
     in
      if size * (xoff + wordWidthf w) > wrapWidth
      then layoutWrapped wrapWidth (0, map reverse (splitOnWrapWidth wrapWidth w) ++ printed) ws
      else layoutWrapped wrapWidth (xoff + ww, case printed of
        (p:ps) -> (reverse w ++ [(spaceGlyph, 0)] ++ p) : ps
        [] -> [reverse w]
        ) ws

  allPrinted :: [[[(Int, [V3 Scalar], [V2 Scalar])]]]
  allPrinted = snd $ (\g -> mapAccumL g (0,0) ended) \(lineNum, cursorLineNum) kernedLine ->
    let printedLines :: [[(Int, [V3 Scalar], [V2 Scalar])]]
        printedLines =
          snd $ (\f -> mapAccumL f 0 (zip [0..] kernedLine)) \wrappedLineCursorX (wrappedLineNum, wrappedLine) ->
            let printLineNum = lineNum + wrappedLineNum
            in case mCutoff of
              Just (from, to) | from > realToFrac printLineNum || to < realToFrac printLineNum -> (wrappedLineCursorX + fromIntegral (length wrappedLine), [])
              _ ->
                let baseY = (realToFrac printLineNum - fromOffset) * realToFrac lineHeight
                    ((totalX,totalCursorX), printedSet) = (\f -> mapAccumL f (0, 0) wrappedLine) \(currentX, currentLineCursorX) ((g, mGVs), kerning) ->
                      let placeGlyph :: V2 Scalar -> V3 Scalar
                          placeGlyph (V2 vx vy) = V3 (vx + currentX) (baseY + (vy + yoffset)) 0 ^* size
                          (cursor_num, cursor_verts, cursor_tcs) = case (cursorGlyph, mCursor) of
                            (Just (_cg, Just cgvs), Just (cLine, cCol)) | cursorLineNum == cLine && cCol == currentLineCursorX + wrappedLineCursorX ->
                              let cverts = map (offset . placeGlyph) cgvs.verts
                                  offset (V3 x y z) = (V3 (x - 24) y z)
                              in (1, cverts, cgvs.texCoords)
                            _ -> (0, [], [])
                          (char_num, char_verts, char_tcs) =
                            case mGVs of
                              Just GlyphVerts {..} ->
                                let new_verts = map placeGlyph verts
                                in (1, new_verts, texCoords)
                              Nothing -> (0, [], [])
                      in ((currentX + g.advance + kerning, currentLineCursorX + 1), (char_num + cursor_num, cursor_verts ++ char_verts, cursor_tcs ++ char_tcs))
                    shift = lineShiftX totalX align * size
                    in (wrappedLineCursorX + fromIntegral totalCursorX, over (each . _2 . each) (\(V3 x y z) -> (V3 (x + shift) y z)) printedSet)
    in ((lineNum + fromIntegral (length printedLines), cursorLineNum + 1), printedLines)
  allNum :: Int
  allNum = sumOf (each . each . each . _1) allPrinted
  allVerts = toListOf (each . each . each . _2 . each) allPrinted
  allTCs = toListOf (each . each . each . _3 . each) allPrinted

  splitOnWrapWidth :: Scalar -> [Kerned] -> [[Kerned]]
  splitOnWrapWidth wrapWidth incoming =
    let f (finishedLines, currentLine, currentX) cur@((gl, _), kerning) =
          if size * (currentX + gl.advance + kerning) > wrapWidth
          then (reverse currentLine : finishedLines, [cur], 0)
          else (finishedLines, cur : currentLine, currentX + gl.advance + kerning)
        (lines' :: [[Kerned]], extra :: [Kerned], _) = foldl' f ([], [], 0) incoming
    in reverse (reverse extra : lines')
  Metrics {..} = metrics
  Atlas {..} = atlas

  fromOffset = case mCutoff of
    Just (from, to) -> from
    Nothing -> 0

  yoffset :: Scalar = case valign of
    AlignMiddle      -> ((ascender + descender) / 2 - descender) / 2
    AlignBottom      -> 0
    AlignTop         -> (ascender + descender) / 2 - descender

  cursorGlyph = HashMap.lookup (ord '|') glyphMap
  spaceGlyph = fromMaybe (error "No space glyph in font") $ HashMap.lookup (ord ' ') glyphMap
  lineEnder = ((Glyph 0 0 Nothing Nothing, Nothing),0) -- For properly displaying a line ending cursor
