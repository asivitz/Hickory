{-# LANGUAGE NamedFieldPuns #-}

module Text.Text where

import Text.Font
import Types.Color
import Types.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Char (ord)
import Data.Maybe
import Math.Vector
import qualified Data.Text as Text
import Data.Hashable

type CharTable a = HashMap.HashMap CharIdent (Glyph a)
type KerningTable a = HashMap.HashMap KerningPair (KerningSpec a)

data Font a = Font (FontInfo a) (CharTable a) (KerningTable a) deriving Show

addItem :: (Data.Hashable.Hashable k, Eq k) => (v -> k) -> HashMap.HashMap k v -> v -> HashMap.HashMap k v
addItem keyfunc table item = HashMap.insert (keyfunc item) item table

makeVerts :: V2 -> Scalar -> Scalar -> [V2]
makeVerts bottomleft@(Vector2 leftx bottomy) width height =
        let final_height = (negate height)
            rightx = (leftx + width)
            topy = (bottomy + final_height) in
                [bottomleft,
                v2 leftx topy,
                v2 rightx bottomy,
                v2 rightx topy]

makeGlyph :: Real a => FontInfo a -> GlyphSpec a -> Glyph a
makeGlyph FontInfo { lineHeight, base } spec@GlyphSpec { x, y, width, height, xoffset, yoffset} = Glyph spec (GlyphVerts verts tcverts)
    where [tcx, tcy, tcw, tch] = map ((/512) . realToFrac) [x, y, width, height]
          centerPoint = v2 (realToFrac xoffset) (realToFrac (lineHeight - yoffset - base))
          verts = makeVerts centerPoint (realToFrac width) (realToFrac height)
          tcverts = [v2 tcx tcy,
                    v2 tcx (tcy + tch),
                    v2 (tcx + tcw) tcy,
                    v2 (tcx + tcw) (tcy + tch)]

makeCharTable :: Real a => FontInfo a -> [GlyphSpec a] -> CharTable a
makeCharTable fi specs = let glyphs = map (makeGlyph fi) specs in
    foldl (addItem (\(Glyph gs _) -> ident gs)) HashMap.empty glyphs

makeKerningTable :: [KerningSpec a] -> KerningTable a
makeKerningTable specs = foldl (addItem (\(KerningSpec pair _) -> pair)) HashMap.empty specs

makeFont :: Integral a => Text.Text -> Either String (Font a)
makeFont text = case runParseFont text of
                    Right (info, glyphspecs, kernings) ->
                        Right $ Font info (makeCharTable info glyphspecs) (makeKerningTable kernings)
                    Left s -> Left s

data GlyphVerts = GlyphVerts [V2] [V2] deriving Show

data Glyph a = Glyph (GlyphSpec a) GlyphVerts
           | Control Int
           | Null deriving Show

data XAlign = AlignRight | AlignCenter | AlignLeft deriving Show
data YAlign = Middle | LowerMiddle | Bottom | Top deriving Show

lineShiftX :: Fractional a => a -> XAlign -> a
lineShiftX width AlignRight = negate width
lineShiftX width AlignCenter = negate (width / 2)
lineShiftX width AlignLeft = 0

lineShiftY :: (Real a, Fractional b) => a -> b -> YAlign -> b
lineShiftY fontSize _ Middle = 43 - (realToFrac fontSize) * 12
lineShiftY fontSize _ LowerMiddle = 48 - (realToFrac fontSize) * 12
lineShiftY _ height Bottom = height
lineShiftY _ _ Top = 0

data TextCommand = TextCommand {
                 text :: String,
                 fontSize :: Scalar,
                 align :: XAlign,
                 valign :: YAlign,
                 color :: Color,
                 leftBump :: Scalar } deriving Show

data PositionedTextCommand = PositionedTextCommand V3 TextCommand deriving (Show)

fontGlyphs :: Real a => String -> Font a -> [Glyph a]
fontGlyphs text (Font _ chartable _) = mapMaybe (\x -> HashMap.lookup x chartable) idents
    where idents = map (CharIdent . ord) text

kerningForGlyphs :: Real a => Glyph a -> Glyph a -> KerningTable a -> a
kerningForGlyphs a b table = 0

displayWidth :: Real a => KerningTable a -> [Glyph a] -> a
displayWidth kerningtable glyphs = fst $ foldl accum (0,Null) glyphs
    where accum (width,prev) g@(Glyph GlyphSpec { xadvance } verts) = let kerning = kerningForGlyphs prev g kerningtable in
            (xadvance + kerning + width, g)
          accum (width,prev) g@(Control _) = (width,g)
          accum (width,prev) g@(Null) = (width,g)

renderedTextSize :: Font Scalar -> TextCommand -> Size Scalar
renderedTextSize font@(Font info chartable kerningtable) (TextCommand text fontSize align valign color commandBump) =
        let glyphs = fontGlyphs text font
            width = displayWidth kerningtable glyphs
            in Size (width * fontSize) (10 * fontSize)

transformTextCommandToVerts :: Real a => PositionedTextCommand -> Font a -> [[Float]]
transformTextCommandToVerts (PositionedTextCommand (Vector3 x y z) (TextCommand text fontSize align valign color commandBump) )
                            font@(Font FontInfo { lineHeight } chartable kerningtable) = 
        let fsize = fontSize / 12
            glyphs = fontGlyphs text font
            width = displayWidth kerningtable glyphs
            xoffset = commandBump + (lineShiftX (realToFrac width) align)
            yoffset = lineShiftY fsize (realToFrac lineHeight) valign 
            accum :: Real a => (Scalar, [[Scalar]], [Scalar]) -> Glyph a -> (Scalar, [[Scalar]], [Scalar])
            accum = \(leftBump, vertlst, color_verts) glyph ->
                case glyph of
                    Null -> (leftBump, vertlst, color_verts)
                    (Control 94) -> (leftBump, vertlst, color_verts)
                    (Control _) -> (leftBump, vertlst, color_verts)
                    {-
                    (let* ([next (cadr lst)]
                        [num (if (integer? next) next (font-character-id next))]
                                [newcolor (color-for-code num)]
                                [new-color-verts (append newcolor alpha-list)]
                                )
                                (accum (cddr lst) left-bump new-color-verts res)
                                )] -}
                    Glyph GlyphSpec { xadvance } (GlyphVerts gverts tc) ->
                        let dotransform :: V2 -> V3
                            dotransform = \(Vector2 vx vy) -> v3 (x + fsize * (vx + leftBump)) (y + fsize * (vy + yoffset)) z
                            new_verts = map dotransform gverts
                            vert_set = foldr (\(v, w) lst -> (vunpack v) ++ (vunpack w) ++ color_verts ++ lst) [] (zip new_verts tc) in
                                ((leftBump + (realToFrac xadvance)), (vert_set : vertlst), color_verts)
            (_, vert_result, _) = foldl accum (xoffset, [], (vunpack color)) glyphs
            in map (map realToFrac) vert_result

transformTextCommandsToVerts :: Real a => [PositionedTextCommand] -> Font a -> [[Float]]
transformTextCommandsToVerts commands font = foldl processCommand [] commands
    where processCommand :: [[Float]] -> PositionedTextCommand -> [[Float]]
          processCommand verts command = (transformTextCommandToVerts command font) ++ verts

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

type PrinterID = Int

