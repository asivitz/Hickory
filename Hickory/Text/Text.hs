{-# LANGUAGE NamedFieldPuns #-}

module Hickory.Text.Text where

import Hickory.Text.Font
import Hickory.Color
import Hickory.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Char (ord)
import Hickory.Math.Vector
import qualified Data.Text as Text
import Data.Hashable
import Data.List
import Linear (V2(..), V3(..))

type CharTable a = HashMap.HashMap CharIdent (Glyph a)
type KerningTable a = HashMap.HashMap KerningPair (KerningSpec a)

data Font a = Font String (FontInfo a) (CharTable a) (KerningTable a) deriving Show

fontName :: Font a -> String
fontName (Font n _ _ _) = n

addItem :: (Data.Hashable.Hashable k, Eq k) => (v -> k) -> HashMap.HashMap k v -> v -> HashMap.HashMap k v
addItem keyfunc table item = HashMap.insert (keyfunc item) item table

makeVerts :: V2 Scalar -> Scalar -> Scalar -> [V2 Scalar]
makeVerts bottomleft@(V2 leftx bottomy) width height =
        let final_height = negate height
            rightx = (leftx + width)
            topy = (bottomy + final_height) in
                [bottomleft,
                V2 leftx topy,
                V2 rightx bottomy,
                V2 rightx topy]

makeGlyph :: Real a => FontInfo a -> GlyphSpec a -> Glyph a
makeGlyph FontInfo { lineHeight, base } spec@GlyphSpec { x, y, width, height, xoffset, yoffset} = Glyph spec (GlyphVerts verts tcverts)
    where [tcx, tcy, tcw, tch] = map ((/512) . realToFrac) [x, y, width, height]
          centerPoint = V2 (realToFrac xoffset) (realToFrac (lineHeight - yoffset - base))
          verts = makeVerts centerPoint (realToFrac width) (realToFrac height)
          tcverts = [V2 tcx tcy,
                    V2 tcx (tcy + tch),
                    V2 (tcx + tcw) tcy,
                    V2 (tcx + tcw) (tcy + tch)]

makeCharTable :: Real a => FontInfo a -> [GlyphSpec a] -> CharTable a
makeCharTable fi specs = let glyphs = map (makeGlyph fi) specs in
    foldl (addItem (\(Glyph gs _) -> ident gs)) HashMap.empty glyphs

makeKerningTable :: [KerningSpec a] -> KerningTable a
makeKerningTable = foldl (addItem (\(KerningSpec pair _) -> pair)) HashMap.empty

makeFont :: Integral a => Text.Text -> String -> Either String (Font a)
makeFont text name = case runParseFont text of
                    Right (info, glyphspecs, kernings) ->
                        Right $ Font name info (makeCharTable info glyphspecs) (makeKerningTable kernings)
                    Left s -> Left s

data GlyphVerts = GlyphVerts [V2 Scalar] [V2 Scalar] deriving Show

data Glyph a
  = Glyph (GlyphSpec a) GlyphVerts
  | Control Int
  | Null deriving Show

data XAlign = AlignRight | AlignCenter | AlignLeft deriving (Show, Eq)
data YAlign = Middle | LowerMiddle | Bottom | Top deriving (Show, Eq)

lineShiftX :: Fractional a => a -> XAlign -> a
lineShiftX width AlignRight = negate width
lineShiftX width AlignCenter = negate (width / 2)
lineShiftX width AlignLeft = 0

lineShiftY :: (Real a, Fractional b) => a -> b -> YAlign -> b
lineShiftY fontSize _ Middle = 43 - realToFrac fontSize * 12
lineShiftY fontSize _ LowerMiddle = 48 - realToFrac fontSize * 12
lineShiftY _ height Bottom = height
lineShiftY _ _ Top = 0

data TextCommand = TextCommand
  { text :: Text.Text
  , fontSize :: Scalar
  , align :: XAlign
  , valign :: YAlign
  , color :: Color
  , leftBump :: Scalar }
  deriving (Show, Eq)

data PositionedTextCommand = PositionedTextCommand (V3 Scalar) TextCommand deriving (Show)

fontGlyphs :: Text.Text -> Font a -> [Glyph a]
fontGlyphs text (Font _ _ chartable _) =
  Text.foldr (\char lst -> let x = ord char
                               glyph = if x < 32 then Just $ Control x else HashMap.lookup (CharIdent x) chartable
                           in maybe lst (:lst) glyph) [] text

kerningForGlyphs :: Real a => Glyph a -> Glyph a -> KerningTable a -> a
kerningForGlyphs a b table = 0

displayWidth :: Real a => KerningTable a -> [Glyph a] -> a
displayWidth kerningtable glyphs = fst $ foldl accum (0,Null) glyphs
  where
  accum (width,prev) g@(Glyph GlyphSpec { xadvance } verts) =
    let kerning = kerningForGlyphs prev g kerningtable
    in (xadvance + kerning + width, g)
  accum (width,prev) g@(Control _) = (width,g)
  accum (width,prev) g@Null = (width,g)

renderedTextSize :: Font Scalar -> TextCommand -> Size Scalar
renderedTextSize font@(Font _ info chartable kerningtable) (TextCommand text fontSize align valign color commandBump) =
  let glyphs = fontGlyphs text font
      width = displayWidth kerningtable glyphs
  in Size (width * fontSize) (10 * fontSize)

transformTextCommandToVerts :: (Real a, Fractional b) => PositionedTextCommand -> Font a -> (Int, [b])
transformTextCommandToVerts (PositionedTextCommand (V3 x y z) (TextCommand text fontSize align valign color commandBump) )
  font@(Font _ FontInfo { lineHeight } chartable kerningtable) =
    let fsize = fontSize / 12
        glyphs = fontGlyphs text font
        width = displayWidth kerningtable glyphs
        xoffset = commandBump + lineShiftX (realToFrac width) align
        yoffset = lineShiftY fsize (realToFrac lineHeight) valign
        accum :: (Real a, Fractional b) => (Scalar, Int, Int, [b], [b]) -> Glyph a -> (Scalar, Int, Int, [b], [b])
        accum = \(leftBump, linenum, numsquares, vertlst, color_verts) glyph ->
            case glyph of
                Null -> (leftBump, linenum, numsquares, vertlst, color_verts)
                (Control 9) -> (leftBump + fsize * 500, linenum, numsquares, vertlst, color_verts)
                (Control 10) -> (0, linenum + 1, numsquares, vertlst, color_verts)
                (Control 94) -> (leftBump, linenum, numsquares, vertlst, color_verts)
                (Control _) -> (leftBump, linenum, numsquares, vertlst, color_verts)
                Glyph GlyphSpec { xadvance } (GlyphVerts gverts tc) ->
                    let dotransform :: V2 Scalar -> V3 Scalar
                        dotransform = \(V2 vx vy) -> V3 (x + fsize * (vx + leftBump)) (realToFrac linenum * fsize * realToFrac lineHeight * (-1) + y + fsize * (vy + yoffset)) z
                        new_verts = map dotransform gverts
                        vert_set = foldr (\(v, w) lst -> vunpackFractional v ++ vunpackFractional w ++ color_verts ++ lst) [] (zip new_verts tc) in
                            (leftBump + realToFrac xadvance, linenum, numsquares + 1, vert_set ++ vertlst, color_verts)
        (_, _, num_squares, vert_result, _) = foldl' accum (xoffset, 0, 0, [], vunpackFractional color) glyphs
        in (num_squares, vert_result)

transformTextCommandsToVerts :: (Real a, Fractional b) => [PositionedTextCommand] -> Font a -> (Int, [b])
transformTextCommandsToVerts commands font = foldl processCommand (0, []) commands
  where
  processCommand :: Fractional b => (Int, [b]) -> PositionedTextCommand -> (Int, [b])
  processCommand (numsquares, verts) command =
    let (num', verts') = transformTextCommandToVerts command font
    in (num' + numsquares, verts' ++ verts)

                    {- Embedded textures
                    (Control 96) ->
                                           (let* ([next (cadr lst)])
                                             (draw-embedded-tex (if (integer? next) next (font-character-id next))
                                                                left-bump
                                                                pos
                                                                fsize
                                                                yoffset
                                                                color-verts)
                                             (accum (cddr lst) (+ left-bump embedded-tex-x-advance) color-verts res)
                                             )])
                                             -}
