{-# LANGUAGE OverloadedLabels, OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Hickory.FRP.Editor.Network where

import Hickory.Math (mkScale, viewTarget, mkTranslation, glerp, Scalar)
import Hickory.Types (Size (..), aspectRatio)
import Hickory.Camera (shotMatrix, Projection (..), Camera (..), project, cameraFocusPlaneSize)
import Data.Maybe (fromMaybe, isJust)
import Hickory.Input (Key(..), PointUp(..), InputFrame(..))
import Linear (axisAngle, identity, Quaternion (..), M44, translation, mkTransformationMat, fromQuaternion, m33_to_m44, unit, Epsilon(..), column, V3 (..), V2 (..), V4 (..), (!*!), normalize, (^*), _x, _y, _z, cross, norm, zero)
import qualified Data.HashMap.Strict as Map
import Hickory.Math.Vector (v2angle)
import Hickory.Vulkan.Forward.Renderer (renderToRenderer, pickObjectID)
import Hickory.FRP.DearImGUIHelpers (tripleToV3, imVec4ToV4, v4ToImVec4, v3ToTriple)
import Control.Lens (traversed, (^.), (&), (%~), (.~), (^?), ix, (<&>), (?~), at, _Just)
import Data.HashMap.Strict (HashMap, traverseWithKey)
import Hickory.FRP.Editor.Types
import Hickory.FRP.Editor.GUI (drawObjectEditorUI, drawMainEditorUI, mkEditorState)
import Hickory.FRP.Editor.View (editorWorldView, editorOverlayView)
import Hickory.FRP.Editor.General (matEuler, matScale, refChangeEvent)
import Hickory.Vulkan.Forward.Types (Renderer(..), CommandMonad, RenderSettings (..), OverlayGlobals (..), WorldSettings (..), worldSettingsDefaults, Scene, DrawCommand, Command)
import Data.Text (unpack, pack)
import qualified Data.Vector.Storable as SV
import qualified Hickory.Vulkan.Types as H
import qualified Hickory.Vulkan.Mesh as H
import Hickory.Resources (ResourcesStore (..), loadResource', ResourcesMonad, runResources, getResourcesStoreResources, loadMeshResource, loadTextureResource, Resources)
import Safe (maximumMay, headMay)
import Data.Foldable (for_, traverse_)
import Hickory.FRP.Editor.Post (GraphicsParams (..), PostEditorState, readGraphicsParams)
import Data.Functor.Const (Const(..))
import Data.Traversable (for)
import Data.Functor.Identity (Identity(..))
import Type.Reflection ((:~~:)(..))
import Hickory.FRP.Camera (omniscientCamera)
import Hickory.Graphics (MatrixMonad)
import Control.Monad (join, mfilter, void, when)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef', IORef)
import Control.Applicative ((<|>), asum, liftA2)
import Data.List.Extra (notNull)
import qualified Data.Enum.Set as ES
import Control.Monad.Writer.Strict (Writer)
import Vulkan (Filter(..), SamplerAddressMode (..))

data ObjectManip = ObjectManip
  { mode       :: Maybe ObjectManipMode
  , captured   :: Maybe (HashMap Int Object, V2 Scalar)
  , activeAxes :: V3 Scalar
  }

objectManip
  :: IO (Size Int -> InputFrame -> Camera -> HashMap Int Object -> Maybe (HashMap Int Object) -> IO (Maybe (ObjectManipMode, V3 Scalar), Maybe (HashMap Int Object)))
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

    let eSelectMode :: Maybe (Maybe (ObjectManipMode, HashMap Int Object))
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

    let captured :: Maybe (HashMap Int Object, V2 Scalar) = asum
          [ (,cursorLoc) <$> eInitialObjects
          , st.captured
          ]
        Size scrW scrH = size
        Camera {..} = camera

    let eMoveObject :: Maybe (HashMap Int Object) = whenE (mode == Just OTranslate) $
          let
            f (objects, start) v =
                let yaxis = up
                    xaxis = cross (normalize angleVec) (normalize up)
                    V2 vx vy = v - start
                    Size focusW focusH = cameraFocusPlaneSize size camera
                in objects & traversed . #transform %~
                  (mkTranslation (liftA2 (*) activeAxes (xaxis ^* (vx / realToFrac scrW * focusW) - yaxis ^* (vy / realToFrac scrH * focusH))) !*!)
          in f <$> captured <*> (fmap fst . headMay $ inFr.touchesLoc)

        eScaleObject :: Maybe (HashMap Int Object) = whenE (mode == Just OScale) $
          let
            f (objects, start) v =
                let objv = project size camera (avgObjTranslation objects)
                    ratio = norm (v - objv) / norm (start - objv)
                in objects & traversed . #transform %~ (!*! mkScale ((\fr -> glerp fr 1 ratio) <$> activeAxes))
          in f <$> captured <*> (fmap fst . headMay $ inFr.touchesLoc)

        eRotateObject :: Maybe (HashMap Int Object) = whenE (mode == Just ORotate) $
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

    pure ((,) <$> mode <*> (Just activeAxes), eModifyObject)

writeEditorState :: EditorChangeEvents -> Object -> IO ()
writeEditorState EditorChangeEvents {..} Object {..} = do
  setVal posChange (transform ^. translation)
  setVal rotChange (matEuler transform)
  setVal scaChange (matScale transform)
  setVal componentsChange  (Map.keys components)

  for_ (Map.toList componentChanges) \((compName, attrName), SomeAttribute attr change) ->
    case components ^? ix compName . ix attrName of
      Just (SomeAttribute attr' (Identity v)) -> case eqAttr attr attr' of
        Just HRefl -> setVal change v
        Nothing -> error "Attributes don't match"
      _ -> setVal change (defaultAttrVal attr)

mkChangeEvents :: HashMap String (Component m state) -> EditorState -> IO EditorChangeEvents
mkChangeEvents componentDefs EditorState {..} = do
  posChange   <- bimapEditorChange tripleToV3 (. v3ToTriple) <$> refChangeEvent posRef
  scaChange   <- bimapEditorChange tripleToV3 (. v3ToTriple) <$> refChangeEvent scaRef
  rotChange   <- bimapEditorChange tripleToV3 (. v3ToTriple) <$> refChangeEvent rotRef
  colorChange <- bimapEditorChange imVec4ToV4 (. v4ToImVec4) <$> refChangeEvent colorRef
  modelChange <- bimapEditorChange unpack     (. pack)       <$> refChangeEvent modelRef
  textureChange     <- bimapEditorChange unpack (. pack)     <$> refChangeEvent textureRef
  litChange         <- refChangeEvent litRef
  castsShadowChange <- refChangeEvent castsShadowRef
  blendChange       <- refChangeEvent blendRef
  specularityChange <- refChangeEvent specularityRef
  componentsChange <- refChangeEvent componentsRef

  componentChanges <- Map.fromList . concat <$> for (Map.toList componentDefs) \(name, Component{..}) ->
    for attributes \(SomeAttribute attr (Const attrName)) ->
      case Map.lookup (name, attrName) componentData of
        Just (SomeAttributeRef attr' ref) -> case eqAttr attr attr' of
          Just HRefl -> case proveAttrClasses attr of
            AttrClasses -> ((name, attrName),) . SomeAttribute attr . bimapEditorChange fromAttrRefType (. toAttrRefType) <$> refChangeEvent ref
          _ -> error "Can't find attribute ref"
        _ -> error "Attribute types don't match"
  pure EditorChangeEvents {..}

setScale :: (Floating a, Epsilon a) => V3 a -> M44 a -> M44 a
setScale v m = m & column _x %~ (^* (v ^. _x)) . normalize . (\x -> if nearZero x then unit _x else x)
                 & column _y %~ (^* (v ^. _y)) . normalize . (\x -> if nearZero x then unit _y else x)
                 & column _z %~ (^* (v ^. _z)) . normalize . (\x -> if nearZero x then unit _z else x)

setRotation :: (RealFloat a, Epsilon a) => V3 a -> M44 a -> V4 (V4 a)
setRotation (V3 rx ry rz) m = (m33_to_m44 (fromQuaternion quat) !*! mkScale (matScale m)) & translation .~ (m ^. translation)
  where
  quat = axisAngle (V3 0 0 1) rz
       * axisAngle (V3 0 1 0) ry
       * axisAngle (V3 1 0 0) rx

mkObjectChangeEvent
  :: H.VulkanResources
  -> HashMap String (Component m state)
  -> ResourcesStore
  -> EditorState
  -> IO (HashMap Int Object -> Maybe Object -> IO (Maybe (HashMap Int Object)))
mkObjectChangeEvent vulkanResources componentDefs rs editorState = do
  eca@EditorChangeEvents {..} <- mkChangeEvents componentDefs editorState

  pure $ \selectedObjects ePopulateEditorState -> do
    for_ (Map.toList componentChanges) \((compName, _attrName), SomeAttribute _attr ch) ->
      case Map.lookup compName componentDefs of
        Just Component {..} -> traverse_ (\_v -> void $ flip traverseWithKey selectedObjects (\k o -> acquire (fromMaybe mempty (Map.lookup compName o.components)) k vulkanResources rs)) =<< ev ch
        Nothing -> error $ "Component not found: " ++ compName


    traverse_ (writeEditorState eca) ePopulateEditorState
    traverse_ (loadMeshResource vulkanResources rs) =<< ev modelChange
    traverse_ (\t -> loadTextureResource vulkanResources rs t (FILTER_NEAREST, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, Nothing)) =<< ev textureChange

    compEvs :: [Maybe (HashMap Int Object)] <- sequence $ Map.toList componentChanges <&> \((compName, attrName), SomeAttribute attr ch) ->
          fmap (\v -> selectedObjects & traversed . #components . at compName . _Just . at attrName ?~ SomeAttribute attr (Identity v))
            <$> ev ch
    staticEvs <- sequence
      [ fmap (\v -> selectedObjects & traversed . #transform . translation .~ v) <$> ev posChange
      , fmap (\v -> selectedObjects & traversed . #transform %~ setScale v)      <$> ev scaChange
      , fmap (\v -> selectedObjects & traversed . #transform %~ setRotation v)   <$> ev rotChange
      , fmap (\v -> selectedObjects & traversed . #components %~ syncMap v)      <$> ev componentsChange
      ]

    pure $ mconcat $ staticEvs ++ compEvs
  where
  syncMap ks m = let om = Map.fromList $ (,mempty) <$> ks
                 in Map.intersection (Map.union m om) om

data Editor = Editor
  { -- objects :: HashMap Int Object
    selectedObjectIDs :: [Int]
  }

editorScene
  :: forall swapchainResources m a.
    (ResourcesMonad m, MatrixMonad m, CommandMonad m)
  => H.VulkanResources
  -> ResourcesStore
  -> PostEditorState
  -> FilePath
  -- -> CoreEvents (Renderer, FrameContext)
  -> IORef (HashMap Int Object)
  -> HashMap String (Component m a)
  -> (swapchainResources -> Resources -> Camera -> m () -> Command ())
  -> IO ()
  -- -> B.Event (HashMap Int Object, FilePath)
  -- -> B.MomentIO (B.Behavior (Scene m), B.Behavior (HashMap Int Object))
  -> IO (Scene (Renderer, swapchainResources))
editorScene vulkanResources resourcesStore postEditorState sceneFile objectsRef componentDefs renderComponent quitAction = do
  editorState <- mkEditorState componentDefs
  -- sceneFile <- B.stepper (error "No scene file") (snd <$> eLoadScene)
  -- let eReplaceObjects = fst <$> eLoadScene

  let ress@ResourcesStore {..} = resourcesStore
  join $ loadResource' meshes "line" $ H.withBufferedMesh vulkanResources $ H.Mesh
    { vertices = [(H.Position, SV.fromList [-1000, 0, 0, 1000, 0, 0])]
    , indices = Nothing
    , minPosition = zero -- TODO
    , maxPosition = zero -- TODO
    , morphTargets = mempty
    }
  join $ loadResource' meshes "lines" $ H.withBufferedMesh vulkanResources $ H.Mesh
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
  stepCamera <- omniscientCamera
  stepObjManip <- objectManip
  stepObjChange <- mkObjectChangeEvent vulkanResources componentDefs ress editorState
  guiPickObjIDMailbox <- newIORef Nothing
  let guiPickObjectID = writeIORef guiPickObjIDMailbox . Just


  -- renInfo <- B.stepper undefined (eRender coreEvents)





  -- let scene = Scene <$> worldRender
  --                   <*> overlayRender
  --                   <*> cameraState
  --                   <*> selectedObjectIDs
  --                   <*> pure (set #clearColor (V4 0.07 0.07 0.07 1))

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
      graphicsParams <- readGraphicsParams postEditorState

      let eLeftClick = (\PointUp { location = V2 x y } -> (x/ realToFrac size.width, y/ realToFrac size.height))
                   <$> headMay (filter (\t -> t.duration < 0.3 && t.ident == 1) renderInputFrame.touchesUp)
      eScreenPickedObjectID <- for eLeftClick $ \cl ->
        mfilter (>0) . Just <$> pickObjectID frameContext renderer cl
      eGUIPickedObjectID :: Maybe Int <- atomicModifyIORef' guiPickObjIDMailbox (Nothing,)
      let ePickedObjectID :: Maybe (Maybe Int) = asum
            [ eScreenPickedObjectID
            , Just <$> eGUIPickedObjectID
            ]
      let eSelectObject :: Maybe Object = join $ ePickedObjectID <&> \oid -> oid >>= (`Map.lookup` objects)

      let selectedObjects :: HashMap Int Object = Map.filterWithKey (\k _ -> k `elem` st.selectedObjectIDs) objects

      let defaultObject = Object (mkTransformationMat identity focusPos) mempty
          eAddObj = whenE (keyPressed Key'A) $ Just (nextObjId objects, defaultObject)
          eDupeObjs = whenE (keyHeld Key'LeftShift && keyPressed Key'D) $ Just $ recalcIds objects selectedObjects
          nextObjId m = fromMaybe 0 (maximumMay (Map.keys m)) + 1
          recalcIds objs forObjs = Map.fromList $ zip [nextObjId objs..] (Map.elems forObjs)
          eDeleteObjs = whenE (keyPressed Key'X) $ Just ()
          pickExampleObject = fmap snd . headMay . Map.toList

      (manipMode, eManipObjects) <- stepObjManip size renderInputFrame camera selectedObjects eDupeObjs
      let editingObject = isJust manipMode
          ePopulateEditorState = asum
            [ pickExampleObject =<< eManipObjects
            , eSelectObject
            , snd <$> eAddObj
            ]

      ePropertyEditedObjects :: Maybe (HashMap Int Object) <- stepObjChange selectedObjects ePopulateEditorState

      let eModifyObjects :: Maybe (HashMap Int Object) = asum
            [ eManipObjects
            , ePropertyEditedObjects
            ]

      let newObjects :: HashMap Int Object = objects & fromMaybe id (asum
            [ uncurry Map.insert <$> eAddObj
            , Map.union <$> eModifyObjects
            , Map.union <$> eDupeObjs
            , flip Map.difference selectedObjects <$ eDeleteObjs
            -- , const <$> eReplaceObjects
            ])

      let selectedObjectIDs :: [Int] = st.selectedObjectIDs & fromMaybe id (asum
            [ const . pure . fst <$> eAddObj
            , const . Map.keys <$> eDupeObjs
            , const [] <$ eDeleteObjs
            , (\case
                  Just i -> if keyHeld Key'LeftShift then (i:) else const [i]
                  Nothing -> const []

              ) <$> ePickedObjectID
            -- , const [] <$ eReplaceObjects
            ])

      writeIORef state $ Editor {..}
      writeIORef objectsRef newObjects

      drawMainEditorUI editorState sceneFile selectedObjects objects guiPickObjectID
      drawObjectEditorUI componentDefs editorState selectedObjects

      let worldRender :: Writer [DrawCommand] ()
          worldRender = renderComponent sr res camera $ editorWorldView componentDefs camera selectedObjects objects manipMode
          overlayRender :: Writer [DrawCommand] ()
          overlayRender = runResources res $ editorOverlayView renderer.staticDirectMaterialConfig size camera cursorLoc selectedObjects (fst <$> manipMode)

      renderToRenderer frameContext renderer (renderSettings size graphicsParams (V4 0.07 0.07 0.07 1) camera st.selectedObjectIDs)
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

renderSettings :: Size Int -> GraphicsParams -> V4 Scalar -> Camera -> [Int] -> RenderSettings
renderSettings size@(Size w _h) GraphicsParams {..} clearColor camera selectedObjIds = RenderSettings
  { worldSettings = worldSettingsDefaults
    { camera
    , lightTransform = identity
    , lightDirection = sunDirection
    , sunColor = sunLight ^* sunStrength
    , ambientColor = ambientLight ^* ambientStrength
    }
  , overlayGlobals = OverlayGlobals
    { viewMat = ovm
    , projMat = opm
    , viewProjMat = opm !*! ovm
    }
  , postSettings = H.PostConstants exposure colorShift saturation filmGrain
  , clearColor = clearColor
  , highlightObjs = selectedObjIds
  }
  where
  ovm = viewTarget zero (V3 0 0 1) (V3 0 (-1) 0)
  opm = shotMatrix (Ortho (realToFrac w) 0 1 False) (aspectRatio size)
