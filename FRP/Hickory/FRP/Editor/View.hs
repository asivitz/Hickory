{-# LANGUAGE FlexibleContexts #-}

module Hickory.FRP.Editor.View where

import qualified Hickory.Graphics as H
import Hickory.Color (rgba, black, green, red, blue)
import Hickory.Math (mkScale, mkRotation, mkTranslation, Scalar, v2angle)
import Hickory.Types (Size (..))
import Linear (V3 (..), V2 (..), (!*!), _x, _y, _z, V4 (..), norm, normalize, (^*), unit, zero, _m33, inv33, transpose)
import Data.Fixed (div')
import qualified Data.HashMap.Strict as Map
import qualified Hickory.Vulkan.Renderer.Types as H
import Data.HashMap.Strict (HashMap)
import Hickory.Graphics (askMatrix, MatrixMonad)
import Hickory.FRP.Editor.Types
import Hickory.Resources (getTexture, getMesh, ResourcesMonad)
import Hickory.Vulkan.Renderer.Types (CommandMonad, StaticMesh (..), MeshType (..), DrawType (..))
import Control.Lens ((.~), (&), (^.))
import Control.Monad (when)
import Data.Foldable (for_)
import Hickory.Camera (Camera(..), project, isOrthographic)
import Foreign (poke)

editorWorldView :: (ResourcesMonad m, CommandMonad m, MatrixMonad m) => HashMap String (Component m a) -> Camera -> HashMap Int Object -> HashMap Int Object -> Maybe (ObjectManipMode, V3 Scalar) -> m ()
editorWorldView componentDefs cs@Camera {..} selected objects manipMode = do
  let zBias =
        mkTranslation (V3 0 0 (((focusPos - angleVec) ^. _z) * (-0.01))) -- So that if a plane is on z=0, draw the coordinate lines under
  linesMesh <- getMesh "lines"
  lineMesh <- getMesh "line"
  let drawCoordinatePlane c1 c2 = do
        -- H.xform coordinateRotation do
          do
            let lineColor = V4 0.11 0.11 0.11 1
            H.xform (mkTranslation (snapVec 1 focusPos & _z .~ 0)) do
              mat <- askMatrix
              drawLines mat linesMesh lineColor

            H.xform (mkTranslation (snapVec 1 focusPos & _z .~ 0) !*! mkRotation (V3 0 0 1) (pi/2)) do
              mat <- askMatrix
              drawLines mat linesMesh lineColor

          H.xform (mkScale (V3 10 10 10)) do
            let lineColor = V4 0.2 0.2 0.2 1
            H.xform (mkTranslation (snapVec 1 focusPos & _z .~ 0)) do
              mat <- askMatrix
              drawLines mat linesMesh lineColor

            H.xform (mkTranslation (snapVec 1 focusPos & _z .~ 0) !*! mkRotation (V3 0 0 1) (pi/2)) do
              mat <- askMatrix
              drawLines mat linesMesh lineColor

          do
            mat <- askMatrix
            drawLines mat lineMesh c1

          H.xform (mkRotation (V3 0 0 1) (pi/2)) do
            mat <- askMatrix
            drawLines mat lineMesh c2
  H.xform zBias $ drawCoordinatePlane red green
  when (isOrthographic cs) do
    H.xform (zBias !*! mkRotation (V3 1 0 0) (pi/2)) $ drawCoordinatePlane red blue
    H.xform (zBias !*! mkRotation (V3 0 0 1) (pi/2) !*! mkRotation (V3 1 0 0) (pi/2)) $ drawCoordinatePlane green blue
  case manipMode of
    Nothing -> pure ()
    Just (_, axes) -> when (norm axes < norm (V3 1 1 1)) do
      let objp = avgObjTranslation selected
      when (axes ^. _x > 0) do
        drawLines (mkTranslation objp) lineMesh (rgba 1 0.2 0.2 1)
      when (axes ^. _y > 0) do
        drawLines (mkTranslation objp !*! mkRotation (V3 0 0 1) (pi/2)) lineMesh (rgba 0.2 1.0 0.2 1)
      when (axes ^. _z > 0) do
        drawLines (mkTranslation objp !*! mkRotation (V3 0 1 0) (pi/2)) lineMesh (rgba 0.2 0.2 1.0 1)

  do
    for_ (Map.toList objects) \(k, o) -> do
      drawObject componentDefs Nothing k o

  where
  snapVec by = fmap (snap by)
  snap by x = let r :: Int = x `div'` by in realToFrac r * by
  drawLines mat mesh color = pure ()
    -- H.addCommand $ H.DrawCommand
    --   { modelMat = mat
    --   , mesh = H.Buffered mesh
    --   , color = color
    --   , drawType = H.Lines
    --   , lit = False
    --   , castsShadow = False
    --   , blend = False
    --   , ident = Nothing
    --   , specularity = 0
    --   }

drawObject :: (ResourcesMonad m, CommandMonad m, MatrixMonad m) => HashMap String (Component m a) -> Maybe a -> Int -> Object -> m ()
drawObject componentDefs state objId Object {..} = do
  H.xform transform $ do
    for_ components \(compName, vals) -> case Map.lookup compName componentDefs of
      Just Component {..} -> draw vals state objId
      Nothing -> error $ "Can't find component definition: " ++ compName

editorOverlayView :: (ResourcesMonad m, CommandMonad m) => H.MaterialConfig H.StaticConstants -> Size Int -> Camera -> V2 Scalar -> HashMap Int Object -> Maybe ObjectManipMode -> m ()
editorOverlayView materialConfig scrSize cs cursorLoc selected mode = do
  case mode of
    Nothing -> pure ()
    Just OScale     -> drawArr objCenter cursorLoc
    Just ORotate    -> drawArr objCenter cursorLoc
    Just OTranslate -> pure ()
  where
  objCenter = project scrSize cs (avgObjTranslation selected)
  drawArr p1 p2 = do
    squareMesh <- getMesh "cube"
    tex        <- getTexture "white"
    for_ [0..num] \i -> do
      let mat = mkTranslation (p1 + normalize diff ^* (realToFrac i * stride)) !*! mkRotation (V3 0 0 1) (negate $ v2angle diff (unit _x)) !*! mkScale (V2 (-on) width)
      H.addCommand $ H.DrawCommand
        { modelMat = mat
        , mesh = Buffered squareMesh
        , hasIdent = Nothing
        , doCastShadow = False
        , doBlend = False
        , cull = False
        , materialConfig = materialConfig
        , instanceCount = 1
        , descriptorSet = Just tex
        , pokeData = flip poke $ H.StaticConstants
            { modelMat    = mat
            , normalMat   = transpose . inv33 $ mat ^. _m33
            , color       = black
            , specularity = 0
            , tiling      = zero
            }
        }
    where
    diff = p2 - p1
    num = norm diff / stride
    stride = on + off
    on = 8
    off = 4
    width = 3
