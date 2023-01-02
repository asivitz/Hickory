module Hickory.Vulkan.Forward.DrawingPrimitives where

import Linear (M44, V3 (..), V4, (!*!), _w, _xyz, (!*), inv44, (^/), V2 (..), (^*), norm)
import Hickory.Vulkan.Forward.Types
import Hickory.Vulkan.Types (Mesh(..), Attribute (..))
import qualified Data.Vector.Storable as SV
import Hickory.Resources (getMesh, Resources (..), getTexture)
import Control.Monad.Reader.Class (MonadReader)
import Hickory.Math (mkTranslation, mkScale, Scalar, v2tov3, v2rotate)
import Hickory.Graphics (askMatrix, MatrixMonad)
import Hickory.Vulkan.Forward.Renderer (ndcBoundaryPoints)
import Control.Lens ((^.))
import Foreign (Storable)
import Data.Foldable (toList)
import Data.Functor ((<&>))

drawLine :: (CommandMonad m, MatrixMonad m) => V4 Float -> V3 Float -> V3 Float -> m ()
drawLine color (V3 p1x p1y p1z) (V3 p2x p2y p2z) = do
  mat <- askMatrix
  addCommand $ DrawCommand
    { modelMat = mat
    , mesh = Dynamic mesh
    , color = color
    , drawType = Lines
    , lit = False
    , castsShadow = False
    , blend = False
    , ident = Nothing
    , specularity = 0
    }
  where
  mesh = Mesh { vertices = [ (Position, SV.fromList [p1x, p1y, p1z, p2x, p2y, p2z]) ], indices = Just $ SV.fromList [0, 1] }

drawPoint :: (CommandMonad m, MonadReader Resources m, MatrixMonad m) => V4 Float -> V3 Float -> m ()
drawPoint color p = do
  cube <- getMesh "cube"
  whiteTex <- getTexture "white"
  mat <- askMatrix
  addCommand $ DrawCommand
    { modelMat = mat !*! mkScale (V3 0.1 0.1 0.1) !*! mkTranslation p
    , mesh = Buffered cube
    , color = color
    , drawType = Static $ StaticMesh whiteTex (V2 1 1)
    , lit = False
    , castsShadow = False
    , blend = False
    , ident = Nothing
    , specularity = 0
    }

drawFrustum :: (CommandMonad m, MatrixMonad m) => V4 Float -> M44 Float -> m ()
drawFrustum color mat = do
  let [p1, p2, p3, p4, p5, p6, p7, p8] = (^. _xyz) . (\v -> v ^/ (v ^. _w)) . (inv44 mat !*) <$> ndcBoundaryPoints
  drawLine color p1 p2
  drawLine color p2 p3
  drawLine color p3 p4
  drawLine color p4 p1

  drawLine color p5 p6
  drawLine color p6 p7
  drawLine color p7 p8
  drawLine color p8 p5

  drawLine color p1 p5
  drawLine color p2 p6
  drawLine color p3 p7
  drawLine color p4 p8

data ArcStyle = RadialCenter | RadialBegin | RadialEnd

drawArc
  :: (CommandMonad m, MatrixMonad m, MonadReader Resources m)
  => V4 Scalar
  -> ArcStyle -- Is the radial in the middle or edge of arc
  -> Scalar -- Distance from front edge of arc to back edge of arc
  -> V2 Scalar -- Point that the arc curves around
  -> V2 Scalar -- Vec from center to front edge of arc
  -> Scalar -- How wide of an arc to draw (0 to 2pi)
  -> Int -- Sections (level of detail)
  -> m ()
drawArc color arcStyle bandDepth circleCenterPos radial arcWidthAngle (realToFrac -> sections) = do
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
                               ] }
  addCommand $ DrawCommand
    { modelMat = mat
    , mesh = Dynamic mesh
    , color = color
    , drawType = Static $ StaticMesh whiteTex (V2 1 1)
    , lit = False
    , castsShadow = False
    , blend = True
    , ident = Nothing
    , specularity = 8
    }

packVecs :: (Storable a, Foldable f) => [f a] -> SV.Vector a
packVecs = SV.fromList . concatMap toList

successivePairs :: [a] -> [(a, a)]
successivePairs (x1:x2:xs) = (x1,x2) : successivePairs (x2:xs)
successivePairs _ = []
