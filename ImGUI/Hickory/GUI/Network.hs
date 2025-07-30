{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Hickory.GUI.Network where

import Hickory.Math (mkScale, viewTarget, mkTranslation, glerp, Scalar)
import Hickory.Types (Size (..), aspectRatio)
import Hickory.Camera (shotMatrix, Projection (..), Camera (..), project, cameraFocusPlaneSize)
import Data.Maybe (fromMaybe, isJust)
import Hickory.Input (Key(..), PointUp(..), InputFrame(..))
import Linear (axisAngle, identity, Quaternion (..), translation, mkTransformationMat, fromQuaternion, m33_to_m44, V3 (..), V2 (..), V4 (..), (!*!), normalize, (^*), cross, norm, zero, (^/), _xyz)
import qualified Data.HashMap.Strict as Map
import Hickory.Math.Vector (v2angle)
import Hickory.Vulkan.Renderer.Renderer (renderToRenderer, pickObjectID)
import Control.Lens (traversed, (^.), (&), (%~), (<&>), sumOf)
import Data.HashMap.Strict (HashMap)
import Hickory.Editor.Types
import Hickory.Editor.GUI (drawObjectEditorUI, drawMainEditorUI)
import Hickory.Editor.View (editorWorldView, editorOverlayView)
import Hickory.Vulkan.Renderer.Types (Renderer(..), CommandMonad, RenderSettings (..), OverlayGlobals (..), WorldSettings (..), worldSettingsDefaults, Scene, DrawCommand, Command, SSAOSettings (..), PostConstants (..))
import qualified Data.Vector.Storable as SV
import qualified Hickory.Vulkan.Types as H
import qualified Hickory.Vulkan.Mesh as H
import Hickory.Resources (ResourcesStore (..), loadResource', ResourcesMonad, runResources, getResourcesStoreResources, Resources)
import Safe (maximumMay, headMay)
import Hickory.Editor.Post (GraphicsParams (..))
import Data.Traversable (for)
import Hickory.Camera.Omniscient (omniscientCamera)
import Hickory.Graphics (MatrixMonad)
import Control.Monad (join, mfilter, void, when)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef', IORef)
import Control.Applicative ((<|>), asum)
import Data.List.Extra (notNull)
import qualified Data.Enum.Set as ES
import Control.Monad.Writer.Strict (Writer)
import qualified Data.HashMap.Strict as HashMap
import Data.Word (Word32)

data ObjectManip = ObjectManip
  { mode       :: Maybe ObjectManipMode
  , captured   :: Maybe (HashMap Word32 Object, V2 Scalar)
  , activeAxes :: V3 Scalar
  }

objectManip
  :: IO (Size Int -> InputFrame -> Camera -> HashMap Word32 Object -> Maybe (HashMap Word32 Object) -> IO (Maybe (ObjectManipMode, V3 Scalar), Maybe (HashMap Word32 Object)))
objectManip = do
  state <- newIORef $
    ObjectManip
      { mode = Nothing
      , captured = Nothing
      , activeAxes = V3 1 1 1
      }
  pure \size inFr camera selectedObjects eEnterMoveMode -> do
    st <- readIORef state

    let keyPressed k = ES.member k inFr.pressedKeys
        keyHeld k    = ES.member k inFr.heldKeys
        whenE f = mfilter (const f)

    let eCancelManip = whenE (isJust st.mode) $ fmap fst $ st.captured <* asum
          [ whenE (keyPressed Key'Escape) (Just ())
          , whenE ((== Just 2) . fmap (.ident) . headMay $ inFr.touchesUp) (Just ())
          ]

    let eSelectMode :: Maybe (Maybe (ObjectManipMode, HashMap Word32 Object))
        eSelectMode = asum
          [ whenE (notNull $ filter ((==1) . (.ident)) inFr.touchesUp) (pure Nothing)
          , Nothing <$ eCancelManip
          , whenE (not . Map.null $ selectedObjects) $ asum
            [ whenE (keyPressed Key'G) $ Just $ Just (OTranslate, selectedObjects)
            , whenE (keyPressed Key'S) $ Just $ Just (OScale,selectedObjects)
            , whenE (keyPressed Key'R) $ Just $ Just (ORotate,selectedObjects)
            ]
          , (\m -> Just (OTranslate, m)) <$> eEnterMoveMode
          ]
        eInitialObjects = join $ fmap snd <$> eSelectMode

        mode :: Maybe ObjectManipMode = join $ (fmap fst <$> eSelectMode) <|> fmap Just st.mode

    let activeAxes :: V3 Scalar = fromMaybe st.activeAxes $ asum
          [ V3 1 1 1 <$ eSelectMode
          , whenE (keyHeld Key'LeftShift && keyPressed Key'Z) (Just $ V3 1 1 0)
          , whenE (keyHeld Key'LeftShift && keyPressed Key'X) (Just $ V3 0 1 1)
          , whenE (keyHeld Key'LeftShift && keyPressed Key'Y) (Just $ V3 1 0 1)
          , whenE (keyPressed Key'X) (Just $ V3 1 0 0)
          , whenE (keyPressed Key'Y) (Just $ V3 0 1 0)
          , whenE (keyPressed Key'Z) (Just $ V3 0 0 1)
          ]

    let cursorLoc = fromMaybe zero . fmap fst . headMay $ inFr.touchesLoc

    let captured :: Maybe (HashMap Word32 Object, V2 Scalar) = asum
          [ (,cursorLoc) <$> eInitialObjects
          , st.captured
          ]
        Size scrW scrH = size
        Camera {..} = camera

    let eMoveObject :: Maybe (HashMap Word32 Object) = whenE (mode == Just OTranslate) $
          let
            f (objects, start) v =
                let yaxis = up
                    xaxis = cross (normalize angleVec) (normalize up)
                    V2 vx vy = v - start
                    Size focusW focusH = cameraFocusPlaneSize size camera
                in objects & traversed . #transform %~
                  (mkTranslation (liftA2 (*) activeAxes (xaxis ^* (vx / realToFrac scrW * focusW) - yaxis ^* (vy / realToFrac scrH * focusH))) !*!)
          in f <$> captured <*> (fmap fst . headMay $ inFr.touchesLoc)

        eScaleObject :: Maybe (HashMap Word32 Object) = whenE (mode == Just OScale) $
          let
            f (objects, start) v =
                let objv = project size camera (avgObjTranslation objects)
                    ratio = norm (v - objv) / norm (start - objv)
                in objects & traversed . #transform %~ (!*! mkScale ((\fr -> glerp fr 1 ratio) <$> activeAxes))
          in f <$> captured <*> (fmap fst . headMay $ inFr.touchesLoc)

        eRotateObject :: Maybe (HashMap Word32 Object) = whenE (mode == Just ORotate) $
          let
            f (objects, start) v =
                let objv = project size camera (avgObjTranslation objects)
                    angle = negate $ v2angle (v - objv) (start - objv)
                in objects & traversed . #transform %~ (\tr ->
                  mkTranslation (tr ^. translation)
                      !*! m33_to_m44 (fromQuaternion (axisAngle angleVec angle))
                      !*! mkTranslation (-tr ^. translation)
                      !*! tr
                  )
          in f <$> captured <*> (fmap fst . headMay $ inFr.touchesLoc)

    let eModifyObject = asum
          [ eInitialObjects
          , eScaleObject
          , eRotateObject
          , eMoveObject
          , eCancelManip
          ]

    writeIORef state $ ObjectManip {..}

    pure ((,) <$> mode <*> Just activeAxes, eModifyObject)

data Editor = Editor
  { selectedObjectIDs :: [Word32]
  }

editorScene
  :: forall swapchainResources m a.
    (ResourcesMonad m, MatrixMonad m, CommandMonad m)
  => H.VulkanResources
  -> ResourcesStore
  -> IORef GraphicsParams
  -> FilePath
  -- -> CoreEvents (Renderer, FrameContext)
  -> IORef (HashMap Word32 Object)
  -> HashMap String (Component m a)
  -> (H.FrameContext -> swapchainResources -> Resources -> Camera -> Size Int -> m () -> Command ())
  -> IO ()
  -- -> B.Event (HashMap Int Object, FilePath)
  -- -> B.MomentIO (B.Behavior (Scene m), B.Behavior (HashMap Int Object))
  -> IO (Scene (Renderer, swapchainResources))
editorScene vulkanResources resourcesStore graphicsParamsRef sceneFile objectsRef componentDefs renderComponent quitAction = do
  -- sceneFile <- B.stepper (error "No scene file") (snd <$> eLoadScene)
  -- let eReplaceObjects = fst <$> eLoadScene

  let ress@ResourcesStore {..} = resourcesStore
  join $ loadResource' meshes "line" $ H.withBufferedMesh vulkanResources (Just "Line") $ H.Mesh
    { vertices = [(H.Position, SV.fromList [-1000, 0, 0, 1000, 0, 0])]
    , indices = Nothing
    , minPosition = zero -- TODO
    , maxPosition = zero -- TODO
    , morphTargets = mempty
    }
  join $ loadResource' meshes "lines" $ H.withBufferedMesh vulkanResources (Just "Lines") $ H.Mesh
    { vertices =
        [ ( H.Position
          , SV.fromList $ concatMap (\i -> [-1000, realToFrac i, 0, 1000, realToFrac i, 0]) ([-1000..1000] :: [Int])
          )
        ]
    , indices = Nothing
    , minPosition = zero -- TODO
    , maxPosition = zero -- TODO
    , morphTargets = mempty
    }

  state <- newIORef $ Editor []
  (setFocusPos, stepCamera) <- omniscientCamera
  stepObjManip <- objectManip
  guiPickObjIDMailbox <- newIORef Nothing
  let guiPickObjectID = writeIORef guiPickObjIDMailbox . Just

  pure \inputFrame -> do
    when (ES.member Key'LeftShift inputFrame.heldKeys && ES.member Key'Escape inputFrame.pressedKeys) quitAction

    pure \frac renderInputFrame size ((renderer, sr),frameContext) -> do
      let cursorLoc = fromMaybe zero . fmap fst . headMay $ renderInputFrame.touchesLoc
      st <- readIORef state
      objects <- readIORef objectsRef
      camera@Camera {..} <- stepCamera size renderInputFrame

      let keyPressed k = ES.member k renderInputFrame.pressedKeys
          keyHeld k    = ES.member k renderInputFrame.heldKeys
          whenE f = mfilter (const f)

      res <- getResourcesStoreResources ress
      graphicsParams <- readIORef graphicsParamsRef

      let eLeftClick = (\PointUp { location = V2 x y } -> (x/ realToFrac size.width, y/ realToFrac size.height))
                   <$> headMay (filter (\t -> t.duration < 0.3 && t.ident == 1) renderInputFrame.touchesUp)
      eScreenPickedObjectID <- for eLeftClick $ \cl ->
        mfilter (>0) . Just <$> pickObjectID frameContext renderer cl
      eGUIPickedObjectID :: Maybe Word32 <- atomicModifyIORef' guiPickObjIDMailbox (Nothing,)
      let ePickedObjectID :: Maybe (Maybe Word32) = asum
            [ eScreenPickedObjectID
            , Just <$> eGUIPickedObjectID
            ]
      let eSelectObject :: Maybe Object = join $ ePickedObjectID <&> \oid -> oid >>= (`Map.lookup` objects)

      let selectedObjects :: HashMap Word32 Object = Map.filterWithKey (\k _ -> k `elem` st.selectedObjectIDs) objects

      let defaultObject = Object (mkTransformationMat identity focusPos) mempty Nothing
          eAddObj = whenE (keyPressed Key'A) $ Just (nextObjId objects, defaultObject)
          eDupeObjs = whenE (keyHeld Key'LeftShift && keyPressed Key'D) $ Just $ recalcIds objects selectedObjects
          nextObjId m = fromMaybe 0 (maximumMay (Map.keys m)) + 1
          recalcIds objs forObjs = Map.fromList $ zip [nextObjId objs..] (Map.toList forObjs <&> \(i,o) -> o { baseObj = Just $ topParent i, components = mempty})
          topParent k = case HashMap.lookup k objects of
            Just Object {..} -> maybe k topParent baseObj
            Nothing -> k
          eDeleteObjs = whenE (keyPressed Key'X) $ Just ()

      (manipMode, eManipObjects) <- stepObjManip size renderInputFrame camera selectedObjects eDupeObjs

      let eModifyObjects :: Maybe (HashMap Word32 Object) = asum
            [ eManipObjects
            ]

      let newObjects :: HashMap Word32 Object = objects & fromMaybe id (asum
            [ uncurry Map.insert <$> eAddObj
            , Map.union <$> eModifyObjects
            , Map.union <$> eDupeObjs
            , flip Map.difference selectedObjects <$ eDeleteObjs
            -- , const <$> eReplaceObjects
            ])

      let selectedObjectIDs :: [Word32] = st.selectedObjectIDs & fromMaybe id (asum
            [ const . pure . fst <$> eAddObj
            , const . Map.keys <$> eDupeObjs
            , const [] <$ eDeleteObjs
            , (\case
                  Just i -> if keyHeld Key'LeftShift then (i:) else const [i]
                  Nothing -> const []

              ) <$> ePickedObjectID
            ])

      when (keyPressed Key'Period) do
        setFocusPos $ (\os -> sumOf (traversed . #transform . translation . _xyz) os ^/ realToFrac (HashMap.size os)) selectedObjects

      writeIORef state $ Editor {..}
      writeIORef objectsRef newObjects

      drawMainEditorUI sceneFile selectedObjects objects guiPickObjectID
      drawObjectEditorUI componentDefs objectsRef st.selectedObjectIDs

      let worldRender :: Writer [DrawCommand] ()
          worldRender = renderComponent frameContext sr res camera size $ editorWorldView renderer.lineDirectMaterialConfig componentDefs camera selectedObjects objects manipMode
          overlayRender :: Writer [DrawCommand] ()
          overlayRender = runResources res $ editorOverlayView renderer.staticDirectMaterialConfig size camera cursorLoc selectedObjects (fst <$> manipMode)

      void $ renderToRenderer frameContext renderer (renderSettings size graphicsParams (V4 0.07 0.07 0.07 1) camera st.selectedObjectIDs)
        worldRender
        overlayRender


-- https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles#Quaternion_to_Euler_angles_conversion
-- https://creativecommons.org/licenses/by-sa/3.0/
quaternionToEuler :: RealFloat a => Quaternion a -> V3 a
quaternionToEuler (Quaternion w (V3 x y z)) = V3 roll pitch yaw
  where
  -- roll (x-axis rotation)
  sinr_cosp = 2 * (w * x + y * z)
  cosr_cosp = 1 - 2 * (x * x + y * y)
  roll = atan2 sinr_cosp cosr_cosp

  -- pitch (y-axis rotation)
  sinp = 2 * (w * y - z * x)
  pitch =
    if abs sinp  >= 1
    then pi/2 * signum sinp -- use 90 degrees if out of range
    else asin sinp

  -- yaw (z-axis rotation)
  siny_cosp = 2 * (w * z + x * y)
  cosy_cosp = 1 - 2 * (y * y + z * z)
  yaw = atan2 siny_cosp cosy_cosp

renderSettings :: Size Int -> GraphicsParams -> V4 Scalar -> Camera -> [Word32] -> RenderSettings
renderSettings size@(Size w _h) GraphicsParams {..} clearColor camera selectedObjIds = RenderSettings
  { worldSettings = worldSettingsDefaults
    { camera
    , lightTransform = identity
    , lightDirection = sunDirection
    , sunColor = sunLight ^* sunStrength
    , ambientColor = ambientLight ^* ambientStrength
    , envMap = Nothing
    }
  , overlayGlobals = OverlayGlobals
    { viewMat = ovm
    , projMat = opm
    , viewProjMat = opm !*! ovm
    }
  , postSettings = PostConstants exposure colorShift saturation filmGrain falseColor
  , clearColor = clearColor
  , highlightObjs = selectedObjIds
  , ssaoSettings = SSAOSettings (fromIntegral ssaoKernelSize) ssaoKernelRadius
  , shadowBiasSlope = shadowBiasSlope
  , features = features
  , lut = Nothing
  }
  where
  ovm = viewTarget zero (V3 0 0 1) (V3 0 (-1) 0)
  opm = shotMatrix (Ortho (realToFrac w) 0 1 False) (aspectRatio size)

editorLayer :: FilePath -> HashMap String (Component m a) -> IORef (HashMap Word32 Object) -> [Word32] -> (Word32 -> IO ()) -> IO ()
editorLayer sceneFile componentDefs objectsRef selectedObjectIds guiPickedObjectID = do
  objects <- readIORef objectsRef
  let selectedObjects :: HashMap Word32 Object = Map.filterWithKey (\k _ -> k `elem` selectedObjectIds) objects
  drawMainEditorUI sceneFile selectedObjects objects guiPickedObjectID
  drawObjectEditorUI componentDefs objectsRef selectedObjectIds
