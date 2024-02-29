module Hickory.Vulkan.Renderer.DrawingPrimitives where

import Linear (M44, V3 (..), V4, _w, _xyz, _m33, (!*), inv44, (^/), V2 (..), (^*), norm, zero, inv33, transpose)
import Hickory.Vulkan.Renderer.Types
import Hickory.Vulkan.Types (Mesh(..), Attribute (..))
import qualified Data.Vector.Storable as SV
import Hickory.Resources (getMesh, Resources (..), getTexture, ResourcesMonad)
import Hickory.Math (Scalar, v2tov3, v2rotate, mkRotation)
import Hickory.Graphics (askMatrix, MatrixMonad, xform)
import Hickory.Vulkan.Renderer.Renderer (ndcBoundaryPoints)
import Control.Lens ((^.))
import Foreign (Storable, poke)
import Data.Foldable (toList)
import Data.Functor ((<&>))

drawLine :: (CommandMonad m, MatrixMonad m) => MaterialConfig StaticConstants -> V4 Float -> V3 Float -> V3 Float -> m ()
drawLine materialConfig color (V3 p1x p1y p1z) (V3 p2x p2y p2z) = do
  mat <- askMatrix
  addCommand $ DrawCommand
    { instances = [(0,mat)]
    , mesh = Dynamic mesh
    , pokeData = flip poke $ StaticConstants
        { modelMat    = mat
        , normalMat   = transpose . inv33 $ mat ^. _m33
        , color       = color
        , specularity = 0
        , tiling      = zero
        }
    , cull = False
    , doCastShadow = False
    , doBlend = False
    , descriptorSet = Nothing
    , materialConfig = materialConfig
    }
  where
  mesh = Mesh { vertices = [ (Position, SV.fromList [p1x, p1y, p1z, p2x, p2y, p2z]) ], indices = Just $ SV.fromList [0, 1], minPosition = zero, maxPosition = zero, morphTargets = [], name = Just "Line" }

drawPoint :: (CommandMonad m, MatrixMonad m) => MaterialConfig StaticConstants -> V4 Float -> V3 Float -> m ()
drawPoint materialConfig color (V3 px py pz)  = do
  mat <- askMatrix
  addCommand $ DrawCommand
    { instances = [(0,mat)]
    , mesh = Dynamic mesh
    , pokeData = flip poke $ StaticConstants
        { modelMat    = mat
        , normalMat   = transpose . inv33 $ mat ^. _m33
        , color       = color
        , specularity = 0
        , tiling      = zero
        }
    , cull = False
    , doCastShadow = False
    , doBlend = False
    , descriptorSet = Nothing
    , materialConfig = materialConfig
    }
  where
  mesh = Mesh { vertices = [ (Position, SV.fromList [px, py, pz]) ], indices = Just $ SV.fromList [0], minPosition = V3 px py pz, maxPosition = V3 px py pz, morphTargets = [], name = Just "Point" }

drawSolidCube :: (CommandMonad m, ResourcesMonad m, MatrixMonad m) => MaterialConfig StaticConstants -> V4 Float -> m ()
drawSolidCube materialConfig color = do
  cube <- getMesh "cube"
  whiteTex <- getTexture "white"
  mat <- askMatrix
  addCommand $ DrawCommand
    { instances = [(0,mat)]
    , mesh = Buffered cube
    , pokeData = flip poke $ StaticConstants
        { modelMat    = mat
        , normalMat   = transpose . inv33 $ mat ^. _m33
        , color       = color
        , specularity = 0
        , tiling      = V2 1 1
        }
    , cull = False
    , doCastShadow = False
    , doBlend = False
    , descriptorSet = Just whiteTex
    , materialConfig = materialConfig
    }

drawWireCube :: (CommandMonad m, MatrixMonad m) => MaterialConfig StaticConstants -> V4 Float -> m ()
drawWireCube materialConfig color = do
  drawFace
  xform (mkRotation (V3 1 0 0) (pi/2)) drawFace
  xform (mkRotation (V3 1 0 0) pi) drawFace
  xform (mkRotation (V3 1 0 0) (-pi/2)) drawFace
  xform (mkRotation (V3 0 1 0) (-pi/2)) drawFace
  xform (mkRotation (V3 0 1 0) (pi/2)) drawFace
  where
  drawFace = do
    drawLine materialConfig color (V3 (-0.5) (-0.5) (-0.5)) (V3 0.5 (-0.5) (-0.5))
    drawLine materialConfig color (V3 0.5 (-0.5) (-0.5)) (V3 0.5 0.5 (-0.5))
    drawLine materialConfig color (V3 0.5 0.5 (-0.5)) (V3 (-0.5) 0.5 (-0.5))
    drawLine materialConfig color (V3 (-0.5) 0.5 (-0.5)) (V3 (-0.5) (-0.5) (-0.5))

drawFrustum :: (CommandMonad m, MatrixMonad m) => MaterialConfig StaticConstants -> V4 Float -> M44 Float -> m ()
drawFrustum materialConfig color mat = do
  let [p1, p2, p3, p4, p5, p6, p7, p8] = (^. _xyz) . (\v -> v ^/ (v ^. _w)) . (inv44 mat !*) <$> ndcBoundaryPoints
  drawLine materialConfig color p1 p2
  drawLine materialConfig color p2 p3
  drawLine materialConfig color p3 p4
  drawLine materialConfig color p4 p1

  drawLine materialConfig color p5 p6
  drawLine materialConfig color p6 p7
  drawLine materialConfig color p7 p8
  drawLine materialConfig color p8 p5

  drawLine materialConfig color p1 p5
  drawLine materialConfig color p2 p6
  drawLine materialConfig color p3 p7
  drawLine materialConfig color p4 p8

data ArcStyle = RadialCenter | RadialBegin | RadialEnd

drawWideArc
  :: (CommandMonad m, MatrixMonad m, ResourcesMonad m)
  => MaterialConfig StaticConstants
  -> V4 Scalar
  -> ArcStyle -- Is the radial in the middle or edge of arc
  -> Scalar -- Distance from front edge of arc to back edge of arc
  -> V2 Scalar -- Point that the arc curves around
  -> V2 Scalar -- Vec from center to front edge of arc
  -> Scalar -- How wide of an arc to draw (0 to 2pi)
  -> Int -- Sections (level of detail)
  -> m ()
drawWideArc materialConfig color arcStyle bandDepth circleCenterPos radial arcWidthAngle (realToFrac -> sections) = do
  mat <- askMatrix
  whiteTex <- getTexture "white"
  let radialLen = norm radial
      angles   = case arcStyle of
        RadialCenter -> [0..sections-1] <&> \s -> arcWidthAngle/2 - arcWidthAngle/(sections - 1) * s
        RadialBegin  -> [0..sections-1] <&> \s -> arcWidthAngle   - arcWidthAngle/(sections - 1) * s
        RadialEnd    -> [0..sections-1] <&> \s ->                 - arcWidthAngle/(sections - 1) * s
      dirVecs  = angles <&> \a -> v2rotate radial a
      dirPairs = successivePairs dirVecs
      points   = concat $ dirPairs <&> \(dv1, dv2) -> (`v2tov3` 0) <$>
                   [ circleCenterPos + dv1
                   , circleCenterPos + dv1 ^* (1 + bandDepth/radialLen)
                   , circleCenterPos + dv2
                   , circleCenterPos + dv2 ^* (1 + bandDepth/radialLen)
                   ]
      indices  = [0..length dirPairs - 1] <&> \(fromIntegral . (4*) -> s) -> [s, s+2, s+1, s+2, s+3, s+1]
      mesh = Mesh { indices = Just (packVecs indices)
                  , vertices = [(Position, packVecs points)
                               ,(TextureCoord, SV.replicate (length points * 2) 0) -- Need dummy TCs for renderer
                               ]
                  , minPosition = zero
                  , maxPosition = zero
                  , morphTargets = []
                  , name = Just "Wide Arc"
                  }
  addCommand $ DrawCommand
    { instances = [(0,mat)]
    , mesh = Dynamic mesh
    , pokeData = flip poke $ StaticConstants
        { modelMat    = mat
        , normalMat   = transpose . inv33 $ mat ^. _m33
        , color       = color
        , specularity = 0
        , tiling      = V2 1 1
        }
    , cull = False
    , doCastShadow = False
    , doBlend = True
    , descriptorSet = Just whiteTex
    , materialConfig = materialConfig
    }

drawLineArc
  :: (CommandMonad m, MatrixMonad m, ResourcesMonad m)
  => MaterialConfig StaticConstants
  -> V4 Scalar
  -> ArcStyle -- Is the radial in the middle or edge of arc
  -> V2 Scalar -- Point that the arc curves around
  -> V2 Scalar -- Vec from center to front edge of arc
  -> Scalar -- How wide of an arc to draw (0 to 2pi)
  -> Int -- Sections (level of detail)
  -> m ()
drawLineArc materialConfig color arcStyle circleCenterPos radial arcWidthAngle (realToFrac -> sections) = do
  mat <- askMatrix
  let angles   = case arcStyle of
        RadialCenter -> [0..sections-1] <&> \s -> arcWidthAngle/2 - arcWidthAngle/(sections - 1) * s
        RadialBegin  -> [0..sections-1] <&> \s -> arcWidthAngle   - arcWidthAngle/(sections - 1) * s
        RadialEnd    -> [0..sections-1] <&> \s ->                 - arcWidthAngle/(sections - 1) * s
      dirVecs  = angles <&> \a -> v2rotate radial a
      dirPairs = successivePairs dirVecs
      points   = concat $ dirPairs <&> \(dv1, dv2) -> (`v2tov3` 0) <$>
                   [ circleCenterPos + dv1
                   , circleCenterPos + dv2
                   ]
      indices  = [0..length dirPairs - 1] <&> \(fromIntegral . (2*) -> s) -> [s, s+1]
      mesh = Mesh { indices = Just (packVecs indices)
                  , vertices = [(Position, packVecs points)]
                  , minPosition = zero
                  , maxPosition = zero
                  , morphTargets = []
                  , name = Just "Line Arc"
                  }
  addCommand $ DrawCommand
    { instances = [(0,mat)]
    , mesh = Dynamic mesh
    , pokeData = flip poke $ StaticConstants
        { modelMat    = mat
        , normalMat   = transpose . inv33 $ mat ^. _m33
        , color       = color
        , specularity = 0
        , tiling      = V2 1 1
        }
    , cull = False
    , doCastShadow = False
    , doBlend = True
    , descriptorSet = Nothing
    , materialConfig = materialConfig
    }

packVecs :: (Storable a, Foldable f) => [f a] -> SV.Vector a
packVecs = SV.fromList . concatMap toList

successivePairs :: [a] -> [(a, a)]
successivePairs (x1:x2:xs) = (x1,x2) : successivePairs (x2:xs)
successivePairs _ = []
