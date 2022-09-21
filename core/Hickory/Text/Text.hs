{-# LANGUAGE NamedFieldPuns, DerivingStrategies, DeriveAnyClass, DeriveGeneric #-}

module Hickory.Text.Text where

import Hickory.Text.Font
import Hickory.Color
import Hickory.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Char (ord)
import Hickory.Math.Vector
import qualified Data.Text as Text
import Data.Hashable (Hashable)
import Data.List
import Linear (V2(..), V3(..))
import GHC.Generics (Generic)

type CharTable a = HashMap.HashMap CharIdent (Glyph a)
type KerningTable a = HashMap.HashMap KerningPair (KerningSpec a)

data Font a = Font
  { name     :: String
  , info         :: FontInfo a
  , charTable    :: CharTable a
  , kerningTable :: KerningTable a
  } deriving Show

data GlyphVerts = GlyphVerts
  { verts     :: [V2 Scalar]
  , texCoords :: [V2 Scalar]
  } deriving Show

data Glyph a
  = Glyph (GlyphSpec a) GlyphVerts -- Something we can display
  | Control Int -- E.g. Non-displayable, e.g. color change
  | Null deriving Show

data XAlign
  = AlignRight
  | AlignCenter
  | AlignLeft
  deriving (Show, Eq, Generic)
  deriving anyclass Hashable

data YAlign
  = Middle
  | LowerMiddle
  | Bottom
  | Top
  deriving (Show, Eq, Generic)
  deriving anyclass Hashable

data TextCommand = TextCommand
  { text     :: Text.Text
  , align    :: XAlign
  , valign   :: YAlign
  , leftBump :: Scalar
  }
  deriving (Show, Eq, Generic)
  deriving anyclass Hashable

data PositionedTextCommand = PositionedTextCommand (V3 Scalar) TextCommand deriving (Show)

addItem :: (Hashable k, Eq k) => (v -> k) -> HashMap.HashMap k v -> v -> HashMap.HashMap k v
addItem keyfunc table item = HashMap.insert (keyfunc item) item table

makeVerts :: V2 Scalar -> Scalar -> Scalar -> [V2 Scalar]
makeVerts topLeft@(V2 leftx topy) width height =
  [ topLeft
  , V2 rightx topy
  , V2 leftx  bottomy
  , V2 rightx bottomy
  ]
  where
  rightx  = leftx + width
  bottomy = topy + height -- in Vulkan, increasing Y goes to bottom of screen

-- |Generate positions and texture coords for a glyph
makeGlyph :: Real a => FontInfo a -> GlyphSpec a -> Glyph a
makeGlyph FontInfo { lineHeight, base, scaleW, scaleH }
          spec@GlyphSpec { x, y, width, height, xoffset, yoffset} = Glyph spec (GlyphVerts verts tcverts)
  where
  [tcx, tcy, tcw, tch] :: [Scalar] =
    [ realToFrac x / realToFrac scaleW
    , realToFrac y / realToFrac scaleH
    , realToFrac width / realToFrac scaleW
    , realToFrac height / realToFrac scaleH
    ]
  tcverts =
    [ V2 tcx tcy
    , V2 (tcx + tcw) tcy
    , V2 tcx (tcy + tch)
    , V2 (tcx + tcw) (tcy + tch)
    ]
  topLeft = V2 (realToFrac xoffset) (realToFrac (base - lineHeight + yoffset))
  verts = makeVerts topLeft (realToFrac width) (realToFrac height)

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

lineShiftX :: Fractional a => a -> XAlign -> a
lineShiftX width AlignRight = negate width
lineShiftX width AlignCenter = negate (width / 2)
lineShiftX width AlignLeft = 0

lineShiftY :: (Fractional b) => b -> YAlign -> b
lineShiftY _ Middle      = 43 - 12
lineShiftY _ LowerMiddle = 48 - 12
lineShiftY height Bottom = height
lineShiftY _ Top = 0

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
renderedTextSize font@(Font _ info chartable kerningtable) (TextCommand text align valign commandBump) =
  let glyphs = fontGlyphs text font
      width = displayWidth kerningtable glyphs
  in Size width 10

transformTextCommandToVerts :: forall a. (Real a) => TextCommand -> Font a -> (Int, [V3 Scalar], [V2 Scalar])
transformTextCommandToVerts (TextCommand text align valign commandBump)
  font@(Font _ FontInfo { lineHeight } chartable kerningtable) = (num_squares, posLstResult, tcLstResult)
  where
  glyphs = fontGlyphs text font
  width = displayWidth kerningtable glyphs
  xoffset = commandBump + lineShiftX (realToFrac width) align
  yoffset = lineShiftY (realToFrac lineHeight) valign
  accum :: (Scalar, Int, Int, [V3 Scalar], [V2 Scalar]) -> Glyph a -> (Scalar, Int, Int, [V3 Scalar], [V2 Scalar])
  accum = \(leftBump, linenum, numsquares, posLst, tcLst) glyph -> case glyph of
    Null -> (leftBump, linenum, numsquares, posLst, tcLst)
    (Control 9)  -> (leftBump + 500, linenum, numsquares, posLst, tcLst)
    (Control 10) -> (0, linenum + 1, numsquares, posLst, tcLst)
    (Control 94) -> (leftBump, linenum, numsquares, posLst, tcLst)
    (Control _)  -> (leftBump, linenum, numsquares, posLst, tcLst)
    Glyph GlyphSpec { xadvance } (GlyphVerts glyphVerts glyphTCs) ->
      let placeGlyph :: V2 Scalar -> V3 Scalar
          placeGlyph = \(V2 vx vy) -> V3 (vx + leftBump)
                                          (realToFrac linenum * realToFrac lineHeight + (vy + yoffset))
                                          0
          new_verts = map placeGlyph glyphVerts
          in (leftBump + realToFrac xadvance, linenum, numsquares + 1, new_verts ++ posLst, glyphTCs ++ tcLst)
  (_, _, num_squares, posLstResult, tcLstResult) = foldl' accum (xoffset, 0, 0, [], []) glyphs

{-
transformTextCommandsToVerts :: (Real a, Fractional b) => [PositionedTextCommand] -> Font a -> (Int, [b])
transformTextCommandsToVerts commands font = foldl processCommand (0, []) commands
  where
  processCommand :: Fractional b => (Int, [b]) -> PositionedTextCommand -> (Int, [b])
  processCommand (numsquares, verts) command =
    let (num', verts') = transformTextCommandToVerts command font
    in (num' + numsquares, verts' ++ verts)
    -}
