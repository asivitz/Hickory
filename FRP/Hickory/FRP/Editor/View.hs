{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.FRP.Editor.View where

import qualified Hickory.Graphics as H
import Hickory.Color (rgba, black)
import Hickory.Math (mkScale, mkRotation, mkTranslation, Scalar, v2angle)
import Hickory.Types (Size (..))
import Linear (identity, V3 (..), V2 (..), (!*!), _x, _y, _z, V4 (..), norm, normalize, (^*), unit)
import Control.Monad.Reader (MonadReader)
import Data.Fixed (div')
import qualified Data.HashMap.Strict as Map
import qualified Hickory.Vulkan.Forward.Types as H
import Data.HashMap.Strict (HashMap)
import Hickory.Graphics (askMatrix)
import Hickory.FRP.Editor.Types
import Hickory.Resources (Resources, getTextureMay, getMeshMay, getTexture, getMesh)
import Hickory.Vulkan.Forward.Types (CommandMonad, StaticMesh (..), MeshType (..), DrawType (..))
import Control.Lens ((.~), (&), (^.), set, each)
import Control.Monad (when)
import Data.Foldable (for_)
import Control.Monad.Writer.Strict (execWriterT, tell)
import Hickory.FRP.Camera (CameraState(..), project, CameraViewMode (..))

editorWorldView :: (MonadReader Resources m, CommandMonad m) => HashMap String Component -> CameraState -> HashMap Int Object -> HashMap Int Object -> Maybe (ObjectManipMode, V3 Scalar) -> m ()
editorWorldView componentDefs CameraState {..} selected objects manipMode = H.runMatrixT do
  let coordinateRotation =
        mkTranslation (V3 0 0 (((focusPos - angleVec) ^. _z) * (-0.01))) -- So that if a plane is on z=0, draw the coordinate lines under
        !*! case viewMode of
        OrthoFront -> mkRotation (V3 1 0 0) (pi/2)
        _ -> identity
  linesMesh <- getMesh "lines"
  lineMesh <- getMesh "line"
  H.xform coordinateRotation do
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
      drawLines mat lineMesh (rgba 1 0 0 1)

    H.xform (mkRotation (V3 0 0 1) (pi/2)) do
      mat <- askMatrix
      drawLines mat lineMesh (rgba 0 1 0 1)
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
      dcs <- execWriterT $ drawObject componentDefs o
      tell $ set (each . #ident) (Just k) dcs -- censor hangs for some reason. so we run/write back

  where
  snapVec by = fmap (snap by)
  snap by x = let r = x `div'` by in realToFrac r * by
  drawLines mat mesh color =
    H.addCommand $ H.DrawCommand
      { modelMat = mat
      , mesh = H.Buffered mesh
      , color = color
      , drawType = H.Lines
      , lit = False
      , castsShadow = False
      , blend = False
      , ident = Nothing
      , specularity = 0
      }

drawObject :: (MonadReader Resources m, CommandMonad m) => HashMap String Component -> Object -> m ()
drawObject componentDefs Object {..} = do
  tex <- getTextureMay texture
  mesh <- getMeshMay model
  tex' <- case tex of
    Just t -> pure t
    _ -> getTexture "white"
  mesh' <- case mesh of
    Just m -> pure m
    _ -> getMesh "cube"

  H.runMatrixT . H.xform transform $ do
    for_ (Map.toList components) \(compName, vals) -> case Map.lookup compName componentDefs of
      Just Component {..} -> draw vals
      Nothing -> error "Can't find component definition"

  H.addCommand H.DrawCommand
    { modelMat = transform
    , mesh = H.Buffered mesh'
    , color
    , drawType = H.Static $ H.StaticMesh tex' (V2 1 1)
    , lit
    , castsShadow
    , blend
    , ident = Nothing
    , specularity
    }

editorOverlayView :: (MonadReader Resources m, CommandMonad m) => Size Int -> CameraState -> V2 Scalar -> HashMap Int Object -> Maybe ObjectManipMode -> m ()
editorOverlayView scrSize cs cursorLoc selected mode = do
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
    for_ [0..num] \i -> H.addCommand $ H.DrawCommand
      { modelMat = mkTranslation (p1 + normalize diff ^* (realToFrac i * stride)) !*! mkRotation (V3 0 0 1) (negate $ v2angle diff (unit _x)) !*! mkScale (V2 (-on) width)
      , mesh = Buffered squareMesh
      , color = black
      , drawType = Static StaticMesh { albedo = tex, tiling = V2 1 1}
      , lit = False
      , castsShadow = False
      , blend = False
      , ident = Nothing
      , specularity = 0
      }
    where
    diff = p2 - p1
    num = norm diff / stride
    stride = on + off
    on = 8
    off = 4
    width = 3
