{-# LANGUAGE FlexibleContexts, OverloadedRecordDot #-}

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
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))
import GHC.Word (Word32)

editorWorldView :: (ResourcesMonad m, CommandMonad m, MatrixMonad m) => HashMap String (Component m a) -> Camera -> HashMap Word32 Object -> HashMap Word32 Object -> Maybe (ObjectManipMode, V3 Scalar) -> m ()
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

  drawObjects componentDefs objects Nothing

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

{-
drawObject :: (ResourcesMonad m, CommandMonad m, MatrixMonad m) => HashMap String (Component m a) -> HashMap Int Object -> Maybe a -> Int -> Object -> m ()
drawObject componentDefs objects state objectId object = do
  H.xform object.transform do
    doDraw object
  where
  doDraw obj = do
    for_ obj.baseObj \baseId -> do
      for_ (Map.lookup baseId objects) \base -> do
        doDraw base
    for_ obj.components \(compName, vals) -> case Map.lookup compName componentDefs of
      Just Component {..} -> draw vals state objectId []
      Nothing -> error $ "Can't find component definition: " ++ compName
      -}

drawObjects :: (ResourcesMonad m, CommandMonad m, MatrixMonad m) => HashMap String (Component m a) -> HashMap Word32 Object -> Maybe a -> m ()
drawObjects componentDefs objects state = do
  for_ objectsWithDupeTransforms \(objId, obj, dupes) -> do
    for_ obj.components \(compName, vals) -> case Map.lookup compName componentDefs of
      Just Component {..} -> draw vals state ((objId, obj.transform) : dupes)
      Nothing -> error $ "Can't find component definition: " ++ compName
  where
  parentChildrenMap = Map.fromListWith (++) . mapMaybe (\(k,v) -> (,[k]) <$> v.baseObj) . Map.toList $ objects
  objectsWithDupeTransforms = flip mapMaybe (Map.toList objects) \(k,v) ->
    case v.baseObj of
      Nothing -> let children = maybe [] (mapMaybe (\ck -> (ck,) <$> Map.lookup ck objects)) $ Map.lookup k parentChildrenMap
                 in Just (k, v, fmap (.transform) <$> children)
      Just _ -> Nothing

editorOverlayView :: (ResourcesMonad m, CommandMonad m) => H.MaterialConfig H.StaticConstants -> Size Int -> Camera -> V2 Scalar -> HashMap Word32 Object -> Maybe ObjectManipMode -> m ()
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
        { instances = [(0,mat)]
        , mesh = Buffered squareMesh
        , doCastShadow = False
        , doBlend = False
        , cull = False
        , materialConfig = materialConfig
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
