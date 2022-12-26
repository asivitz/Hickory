module Hickory.Vulkan.Forward.DrawingPrimitives where

import Linear (M44, V3 (..), V4, (!*!), _w, _xyz, (!*), inv44, (^/))
import Hickory.Vulkan.Forward.Types
import Hickory.Vulkan.Types (Mesh(..), Attribute (..))
import qualified Data.Vector.Storable as SV
import Hickory.Resources (getMesh, Resources (..))
import Control.Monad.Reader.Class (MonadReader)
import Hickory.Math (mkTranslation, mkScale)
import Hickory.Graphics (askMatrix, MatrixMonad)
import Hickory.Vulkan.Forward.Renderer (ndcBoundaryPoints)
import Control.Lens ((^.))

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
  mat <- askMatrix
  addCommand $ DrawCommand
    { modelMat = mat !*! mkScale (V3 0.1 0.1 0.1) !*! mkTranslation p
    , mesh = Buffered cube
    , color = color
    , drawType = Lines
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
