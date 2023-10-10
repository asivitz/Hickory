{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hickory.Text.ParseJson where

import Hickory.Math (Scalar)
import Data.Text (Text)
import GHC.Generics (Generic)
import Deriving.Aeson
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Linear (V2 (..))
import qualified Data.HashMap.Strict as HashMap
import Data.Functor ((<&>))
import Data.Char (toLower)

-- Format from: https://github.com/Chlumsky/msdf-atlas-gen
{-
{
  "atlas": {
    "type": "msdf",
    "distanceRange": 2,
    "size": 32.34375,
    "width": 204,
    "height": 204,
    "yOrigin": "bottom"
  },
  "metrics": {
    "emSize": 1,
    "lineHeight": 1.212,
    "ascender": 0.95400000000000007,
    "descender": -0.25800000000000001,
    "underlineY": -0.17500000000000002,
    "underlineThickness": 0.089999999999999997
  },
  "glyphs": [
    { "unicode": 32, "advance": 0.22 },
    {
      "unicode": 33,
      "advance": 0.31,
      "planeBounds": {
        "left": 0.046787439613526591,
        "bottom": -0.049973429951690851,
        "right": 0.26321256038647345,
        "top": 0.72297342995169089
      },
      "atlasBounds": {
        "left": 41.5,
        "bottom": 143.5,
        "right": 48.5,
        "top": 168.5
      }
    },
    ...
  "kerning": [
    { "unicode1": 32, "unicode2": 84, "advance": -0.02 },
    ...
  ]
}
-}

data ToLower
instance StringModifier ToLower where
  getStringModifier "" = ""
  getStringModifier (c : xs) = toLower c : xs

data MSDFFont = MSDFFont
  { atlas   :: Atlas
  , metrics :: Metrics
  , glyphs  :: [Glyph]
  , kerning :: [KerningPair]
  } deriving stock Generic
    deriving (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[Rename "type_" "type"], ConstructorTagModifier '[ToLower]] MSDFFont

data Atlas = Atlas
  { type_         :: Text
  , distanceRange :: Scalar
  , size          :: Scalar
  , width         :: Int
  , height        :: Int
  , yOrigin       :: YOrigin
  } deriving stock (Generic)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[Rename "type_" "type"], ConstructorTagModifier '[ToLower]] Atlas

data YOrigin = Bottom | Top
  deriving stock (Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[Rename "type_" "type"], ConstructorTagModifier '[ToLower]] YOrigin

data Metrics = Metrics
  { emSize             :: Scalar
  , lineHeight         :: Scalar
  , ascender           :: Scalar
  , descender          :: Scalar
  , underlineY         :: Scalar
  , underlineThickness :: Scalar
  } deriving stock Generic
    deriving (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[Rename "type_" "type"], ConstructorTagModifier '[ToLower]] Metrics

data Glyph = Glyph
  { unicode     :: Int
  , advance     :: Scalar -- Cursor advancement in world X
  , planeBounds :: Maybe Bounds -- World coordinates relative to cursor
  , atlasBounds :: Maybe Bounds -- Position within the texture atlas
  } deriving stock Generic
    deriving (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[Rename "type_" "type"], ConstructorTagModifier '[ToLower]] Glyph

data KerningPair = KerningPair
  { unicode1 :: Int
  , unicode2 :: Int
  , advance  :: Scalar
  } deriving stock Generic
    deriving (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[Rename "type_" "type"], ConstructorTagModifier '[ToLower]] KerningPair

data Bounds = Bounds
  { left   :: Scalar
  , bottom :: Scalar
  , right  :: Scalar
  , top    :: Scalar
  } deriving stock Generic
    deriving (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[Rename "type_" "type"], ConstructorTagModifier '[ToLower]] Bounds

data Font = Font
  { atlas      :: Atlas
  , metrics    :: Metrics
  , glyphMap   :: HashMap.HashMap Int (Glyph, Maybe GlyphVerts)
  , kerningMap :: HashMap.HashMap (Int,Int) Scalar
  } deriving stock Generic

data GlyphVerts = GlyphVerts
  { verts     :: [V2 Scalar]
  , texCoords :: [V2 Scalar]
  } deriving Show

makeFont :: ByteString -> Either String Font
makeFont text = case eitherDecode text of
  Right MSDFFont {..} ->
    let -- Metrics {..} = metrics
        glyphMap     = HashMap.fromList $ glyphs <&> \g@Glyph {..} -> (unicode, (g, makeGlyphVerts atlas metrics g))
        kerningMap   = HashMap.fromList $ kerning <&> \KerningPair {..} -> ((unicode1, unicode2), advance)
    in Right Font {..}
  Left s -> Left s
  where
  makeGlyphVerts Atlas {..} Metrics {..} Glyph {..} = case (planeBounds, atlasBounds) of
    (Just (Bounds pl pb pr pt), Just (Bounds al ab ar at)) ->
      let
        texCoords =
          [ V2 (al / realToFrac width) (at / realToFrac height)
          , V2 (ar / realToFrac width) (at / realToFrac height)
          , V2 (al / realToFrac width) (ab / realToFrac height)
          , V2 (ar / realToFrac width) (ab / realToFrac height)
          ]
        verts =
          [ V2 pl (-pt)
          , V2 pr (-pt)
          , V2 pl (-pb)
          , V2 pr (-pb)
          ]
      in Just GlyphVerts {..}
    _ -> Nothing
