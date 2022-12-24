{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.FRP.Editor.Network where

-- import Render.Common (RenderMonadNoMatrix)
import qualified Reactive.Banana as B
import Hickory.FRP.CoreEvents (CoreEvents (..), concatTouchEvents, maskCoreEvents)
import qualified Reactive.Banana.Frameworks as B
import Reactive.Banana ((<@>), (<@), liftA2)
import Hickory.Color (white)
import Hickory.Math (mkScale, viewTarget, mkTranslation, glerp, Scalar, Mat44, viewDirection)
import Hickory.Types (Size (..), aspectRatio)
import Hickory.Camera (shotMatrix, Projection (..))
import Hickory.FRP.UI (trackTouches, TouchChange(..))
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import Hickory.FRP.Combinators (unionFirst)
import Hickory.Input (Key(..))
import Linear (rotate, axisAngle, identity, Quaternion (..), M44, translation, mkTransformationMat, fromQuaternion, m33_to_m44, unit, Epsilon(..), column, V3 (..), V2 (..), V4 (..), (!*!), normalize, (^*), _x, _y, _z, cross, norm, zero)
import qualified Data.HashMap.Strict as Map
import Hickory.Math.Vector (v2angle)
import Hickory.Vulkan.Forward.Renderer (pickObjectID, renderToRenderer)
import Control.Monad.IO.Class (liftIO)
import Hickory.FRP.DearImGUIHelpers (tripleToV3, v3ToTriple, v4ToImVec4, imVec4ToV4)
import Control.Lens (traversed, Bifunctor (..), (^.), (&), (%~), (<&>), view, _1, _3, (.~))
import Data.HashMap.Strict (HashMap)
import Hickory.FRP.Editor.Types
import Hickory.FRP.Editor.GUI (drawObjectEditorUI, drawMainEditorUI, mkEditorState)
import Hickory.FRP.Editor.View (editorWorldView, editorOverlayView)
import Hickory.FRP.Editor.General (mkCursorLoc, matEuler, matScale, refChangeEvent, project)
import Hickory.Vulkan.Types (FrameContext)
import Hickory.Vulkan.Forward.Types (Renderer, CommandMonad, RenderSettings (..), OverlayGlobals (..), WorldGlobals(..), worldGlobalDefaults)
import Data.Text (unpack, pack)
import Vulkan (SamplerAddressMode (..), Filter (..))
import Control.Monad.Reader (ReaderT(..), MonadReader)
import qualified Data.Vector.Storable as SV
import qualified Hickory.Vulkan.Types as H
import qualified Hickory.Vulkan.Mesh as H
import Hickory.Resources (ResourcesStore (..), loadResource', getResourcesStoreResources, loadResource, Resources (..))
import Safe (maximumMay, headMay)
import Data.Foldable (for_)
import Hickory.FRP.Editor.Post (GraphicsParams (..))

editorFOV :: Floating a => a
editorFOV = pi/4

editorViewMat :: V3 Scalar -> V3 Scalar -> V3 Scalar -> Mat44 -- used to build the shadowmap
editorViewMat center towardCenter up
    = viewTarget (center - towardCenter) center up

editorProjMat :: Size Int -> Scalar -> CameraViewMode -> Mat44
editorProjMat (aspectRatio -> scrRat) width = \case
  OrthoTop   -> orthoMat
  OrthoFront -> orthoMat
  PerspView  -> shotMatrix (Perspective editorFOV 0.1 1000) scrRat
  where
  orthoMat = shotMatrix (Ortho width 0.1 1000 True) scrRat

viewManip :: CoreEvents a -> B.MomentIO (B.Behavior CameraState)
viewManip coreEvents = mdo
  (eChanges, _bTouches) <- trackTouches (concatTouchEvents coreEvents)

  let eOrthoTop = B.filterE (==Key'0) $ keyDown coreEvents
  let eOrthoFront = B.filterE (==Key'1) $ keyDown coreEvents

  let mode = (\cmd shift -> if cmd then Zoom else if shift then Pan else Rotate) <$> keyHeldB coreEvents Key'LeftSuper <*> keyHeldB coreEvents Key'LeftShift

  let clickMove = B.filterJust $ fmap headMay $ eChanges <&> mapMaybe \case
        LocTouch _ 0 v -> Just v
        _ -> Nothing
      clickStart = B.filterJust $ fmap headMay $ eChanges <&> mapMaybe \case
        AddTouch _ 0 v -> Just v
        _ -> Nothing
      clickEnd = B.filterJust $ fmap headMay $ eChanges <&> mapMaybe \case
        AddTouch _ 0 v -> Just v
        _ -> Nothing

      eRepositionCamera :: B.Event (V3 Scalar) = B.whenE ((==Pan) <$> mode) $
        let f (Size scrW scrH) upv (Size orthoW orthoH) (Just (start, focusPos, triple)) v =
              let angle = buildCameraAngleVec triple
                  xaxis = normalize $ cross (normalize angle) upv
                  yaxis = normalize $ cross xaxis (normalize angle)
                  (V2 vx vy) = v - start
              in focusPos - xaxis ^* (vx / realToFrac scrW * orthoW) + yaxis ^* (vy / realToFrac scrH * orthoH)
        in f <$> scrSizeB coreEvents <*> up <*> cameraFocusPlaneSize <*> captured <@> clickMove

      eZoomCamera :: B.Event Scalar = B.whenE ((==Zoom) <$> mode) $((,) <$> captured <@> clickMove ) <&> \(Just (start, _, (_,zoom)), v) ->
        let V2 _vx vy = v - start
        in zoom - vy / 10

      eRotateCamera :: B.Event (Scalar,Scalar) = B.whenE ((==Rotate) <$> mode) $((,) <$> captured <@> clickMove ) <&> \(Just (start, _, ((zang,ele),_)), v) ->
        let V2 vx vy = v - start
        in (zang - vx / 100, ele + vy / 100)

  captured :: B.Behavior (Maybe (V2 Scalar, V3 Scalar, ((Scalar, Scalar), Scalar))) <- B.stepper Nothing $ unionFirst
    [ (\cfp ct ps -> Just (ps, cfp, ct)) <$> cameraFocusPos <*> cameraTriple <@> clickStart
    , Nothing <$ clickEnd
    ]

  cameraAngles <- B.stepper (pi/4, pi/4) $ unionFirst
    [ eRotateCamera
    , (0, pi/2) <$ eOrthoTop
    , (0, 0) <$ eOrthoFront
    ]
  cameraZoom     :: B.Behavior Scalar      <- B.stepper 20 eZoomCamera
  cameraFocusPos :: B.Behavior (V3 Scalar) <- B.stepper (V3 0 0 0) eRepositionCamera
  let cameraTriple = (,) <$> cameraAngles <*> cameraZoom
  let buildCameraAngleVec ((zang,ele),zoom) = (^* zoom)
        . rotate (axisAngle (V3 0 0 1) zang)
        . rotate (axisAngle (V3 1 0 0) ele)
        $ V3 0 (-1) 0

  cameraViewMode <- B.stepper PerspView $ unionFirst
    [ OrthoTop   <$ eOrthoTop
    , OrthoFront <$ eOrthoFront
    , PerspView  <$ eRotateCamera
    ]

  let up = cameraAngles <&> \(zang,ele)
        -> rotate (axisAngle (V3 0 0 1) zang)
         . rotate (axisAngle (V3 1 0 0) ele)
         $ V3 0 0 1

  let cameraAngleVec :: B.Behavior (V3 Scalar) = buildCameraAngleVec <$> cameraTriple
      cameraFocusPlaneHeight = (\z -> tan (editorFOV / 2) * z * 2) <$> cameraZoom
      cameraFocusPlaneWidth = (\(aspectRatio -> scrRat) height -> height * scrRat) <$> scrSizeB coreEvents <*> cameraFocusPlaneHeight
      cameraFocusPlaneSize = Size <$> cameraFocusPlaneWidth <*> cameraFocusPlaneHeight

  let viewMat = editorViewMat <$> cameraFocusPos <*> cameraAngleVec <*> up
      projMat = editorProjMat <$> scrSizeB coreEvents <*> cameraFocusPlaneWidth <*> cameraViewMode
  pure $ CameraState <$> viewMat <*> projMat <*> cameraViewMode <*> cameraFocusPos <*> cameraAngleVec <*> cameraFocusPlaneSize <*> up

objectManip :: CoreEvents a -> B.Behavior CameraState -> B.Behavior (HashMap Int Object) -> B.Event (HashMap Int Object) -> B.MomentIO (B.Behavior (Maybe (ObjectManipMode, V3 Scalar)), B.Event (HashMap Int Object))
objectManip coreEvents cameraState selectedObjects eEnterMoveMode = mdo
  let eSelectMode :: B.Event (Maybe (ObjectManipMode, HashMap Int Object))
      eSelectMode = unionFirst
        [ Nothing <$ B.filterE ((==1) . view _3 . head) (eTouchesUp coreEvents)
        , Nothing <$ eCancelManip
        , B.whenE (not . Map.null <$> selectedObjects) $ unionFirst
          [ (\m -> Just (OTranslate, m)) <$> selectedObjects <@ B.filterE (==Key'G) (keyDown coreEvents)
          , (\m -> Just (OScale,m))      <$> selectedObjects <@ B.filterE (==Key'S) (keyDown coreEvents)
          , (\m -> Just (ORotate,m))     <$> selectedObjects <@ B.filterE (==Key'R) (keyDown coreEvents)
          ]
        , (\m -> Just (OTranslate, m)) <$> eEnterMoveMode
        ]

  mode :: B.Behavior (Maybe ObjectManipMode) <- B.stepper Nothing (fmap fst <$> eSelectMode)

  activeAxes :: B.Behavior (V3 Scalar) <- B.stepper (V3 1 1 1) $ unionFirst
    [ V3 1 1 1 <$ eSelectMode
    , B.whenE (keyHeldB coreEvents Key'LeftShift) $ V3 1 1 0 <$ B.filterE (==Key'Z) (keyDown coreEvents)
    , B.whenE (keyHeldB coreEvents Key'LeftShift) $ V3 0 1 1 <$ B.filterE (==Key'X) (keyDown coreEvents)
    , B.whenE (keyHeldB coreEvents Key'LeftShift) $ V3 1 0 1 <$ B.filterE (==Key'Y) (keyDown coreEvents)
    , V3 1 0 0 <$ B.filterE (==Key'X) (keyDown coreEvents)
    , V3 0 1 0 <$ B.filterE (==Key'Y) (keyDown coreEvents)
    , V3 0 0 1 <$ B.filterE (==Key'Z) (keyDown coreEvents)
    ]

  let
      eMoveObject :: B.Event (HashMap Int Object) = B.filterJust $ B.whenE ((==Just OTranslate) <$> mode) $
        let
          f :: Size Int -> CameraState -> Maybe (HashMap Int Object, V2 Scalar) -> V3 Scalar -> V2 Scalar -> Maybe (HashMap Int Object)
          f _ _ Nothing _ _ = Nothing
          f (Size scrW scrH) CameraState {..} (Just (objects, start)) axes v =
              let yaxis = up
                  xaxis = cross (normalize angleVec) (normalize up)
                  (V2 vx vy) = v - start
                  Size focusW focusH = focusPlaneSize
              in Just $ objects & traversed . #transform %~
                (mkTranslation (liftA2 (*) axes (xaxis ^* (vx / realToFrac scrW * focusW) - yaxis ^* (vy / realToFrac scrH * focusH))) !*!)
        in f <$> scrSizeB coreEvents <*> cameraState <*> captured <*> activeAxes <@> (fst . head <$> eTouchesLoc coreEvents)

      eScaleObject :: B.Event (HashMap Int Object) = B.filterJust $ B.whenE ((==Just OScale) <$> mode) $
        let
          f _ _ Nothing _ _ = Nothing
          f ss CameraState {..} (Just (objects, start)) axes v =
              let objv = project ss (projMat !*! viewMat !*! mkTranslation (avgObjTranslation objects)) zero
                  ratio = norm (v - objv) / norm (start - objv)
              in Just $ objects & traversed . #transform %~ (!*! mkScale ((\fr -> glerp fr 1 ratio) <$> axes))
        in f <$> scrSizeB coreEvents <*> cameraState <*> captured <*> activeAxes <@> (fst . head <$> eTouchesLoc coreEvents)

      eRotateObject :: B.Event (HashMap Int Object) = B.filterJust $ B.whenE ((==Just ORotate) <$> mode) $
        let
          f _ _ Nothing _ = Nothing
          f ss CameraState {..} (Just (objects, start)) v =
              let objv = project ss (projMat !*! viewMat !*! mkTranslation (avgObjTranslation objects)) zero
                  angle = negate $ v2angle (v - objv) (start - objv)
              in Just $ objects & traversed . #transform %~ (\tr ->
                mkTranslation (tr ^. translation)
                    !*! m33_to_m44 (fromQuaternion (axisAngle angleVec angle))
                    !*! mkTranslation (-tr ^. translation)
                    !*! tr
                )
        in f <$> scrSizeB coreEvents <*> cameraState <*> captured <@> (fst . head <$> eTouchesLoc coreEvents)

  let eInitialObjects = snd <$> B.filterJust eSelectMode
      eCancelManip = B.whenE (isJust <$> mode) $ fmap fst . B.filterJust $ captured <@ unionFirst
        [ () <$ B.filterE (==Key'Escape) (keyDown coreEvents)
        , () <$ B.filterE ((==2) . view _3 . head) (eTouchesUp coreEvents)
        ]

  cursorLoc :: B.Behavior (V2 Scalar) <- mkCursorLoc coreEvents

  captured :: B.Behavior (Maybe (HashMap Int Object, V2 Scalar)) <- B.stepper Nothing $
    (\x y -> Just (y,x)) <$> cursorLoc <@> eInitialObjects

  let eModifyObject = unionFirst
        [ eInitialObjects
        , eScaleObject
        , eRotateObject
        , eMoveObject
        , eCancelManip
        ]

  pure (liftA2 (,) <$> mode <*> (Just <$> activeAxes), eModifyObject)

writeEditorState :: EditorChangeEvents -> Object -> IO ()
writeEditorState EditorChangeEvents {..} Object {..} = do
  snd posChange (transform ^. translation)
  snd rotChange (matEuler transform)
  snd scaChange (matScale transform)
  snd colorChange color
  snd modelChange model
  snd textureChange texture
  snd litChange lit
  snd castsShadowChange castsShadow
  snd blendChange blend
  snd specularityChange specularity

mkChangeEvents :: CoreEvents a -> EditorState -> B.MomentIO EditorChangeEvents
mkChangeEvents coreEvents EditorState {..} = do
  posChange   <- bimap (fmap tripleToV3) (.v3ToTriple) <$> refChangeEvent coreEvents posRef
  scaChange   <- bimap (fmap tripleToV3) (.v3ToTriple) <$> refChangeEvent coreEvents scaRef
  rotChange   <- bimap (fmap tripleToV3) (.v3ToTriple) <$> refChangeEvent coreEvents rotRef
  colorChange <- bimap (fmap imVec4ToV4) (.v4ToImVec4) <$> refChangeEvent coreEvents colorRef
  modelChange <- bimap (fmap unpack)     (.pack)       <$> refChangeEvent coreEvents modelRef
  textureChange     <- bimap (fmap unpack) (.pack)     <$> refChangeEvent coreEvents textureRef
  litChange         <- refChangeEvent coreEvents litRef
  castsShadowChange <- refChangeEvent coreEvents castsShadowRef
  blendChange       <- refChangeEvent coreEvents blendRef
  specularityChange <- refChangeEvent coreEvents specularityRef

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
  :: CoreEvents a
  -> ResourcesStore
  -> EditorState
  -> B.Behavior (HashMap Int Object)
  -> B.Event Object
  -> B.MomentIO (B.Event (HashMap Int Object))
mkObjectChangeEvent coreEvents ResourcesStore {..} editorState selectedObjects ePopulateEditorState = do
  eca@EditorChangeEvents {..} <- mkChangeEvents coreEvents editorState

  B.reactimate $ writeEditorState eca <$> ePopulateEditorState
  B.reactimate $ loadResource meshes <$> fst modelChange
  B.reactimate $ (\t -> loadResource textures (t,FILTER_NEAREST, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)) <$> fst textureChange

  pure $ unionFirst
    [ (\os v -> os & traversed . #transform . translation .~ v) <$> selectedObjects <@> fst posChange
    , (\os v -> os & traversed . #transform %~ setScale v)      <$> selectedObjects <@> fst scaChange
    , (\os v -> os & traversed . #transform %~ setRotation v)   <$> selectedObjects <@> fst rotChange
    , (\os v -> os & traversed . #color .~ v)                   <$> selectedObjects <@> fst colorChange
    , (\os v -> os & traversed . #model .~ v)                   <$> selectedObjects <@> fst modelChange
    , (\os v -> os & traversed . #texture .~ v)                 <$> selectedObjects <@> fst textureChange
    , (\os v -> os & traversed . #lit .~ v)                     <$> selectedObjects <@> fst litChange
    , (\os v -> os & traversed . #castsShadow .~ v)             <$> selectedObjects <@> fst castsShadowChange
    , (\os v -> os & traversed . #blend .~ v)                   <$> selectedObjects <@> fst blendChange
    , (\os v -> os & traversed . #specularity .~ v)             <$> selectedObjects <@> fst specularityChange
    ]

editorNetwork
  :: H.VulkanResources
  -> ResourcesStore
  -> CoreEvents (Renderer, FrameContext)
  -> B.Behavior GraphicsParams
  -> FilePath
  -> B.Event FilePath
  -> B.MomentIO (B.Behavior [Object])
editorNetwork vulkanResources resourcesStore coreEvents graphicsParams initialSceneFile eLoadScene = mdo
  editorState <- liftIO mkEditorState
  sceneFile <- B.stepper initialSceneFile eLoadScene

  initialScene <- liftIO do
   objs <- read <$> readFile initialSceneFile
   for_ objs \Object {..} -> loadResource (resourcesStore ^. #meshes) model
   for_ objs \Object {..} -> loadResource (resourcesStore ^. #textures) (texture, FILTER_NEAREST, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)
   pure objs
  eReplaceObjects <- B.execute $ fmap read . liftIO . readFile <$> eLoadScene

  liftIO do
    let ResourcesStore {..} = resourcesStore
    loadResource' meshes "line" $ H.withBufferedMesh vulkanResources $ H.Mesh
      { vertices = [(H.Position, SV.fromList [-1000, 0, 0, 1000, 0, 0])]
      , indices = Nothing
      }
    loadResource' meshes "lines" $ H.withBufferedMesh vulkanResources $ H.Mesh
      { vertices =
          [ ( H.Position
            , SV.fromList $ concatMap (\i -> [-1000, realToFrac i, 0, 1000, realToFrac i, 0]) ([-1000..1000] :: [Int])
            )
          ]
      , indices = Nothing
      }
  resources <- B.fromPoll . getResourcesStoreResources $ resourcesStore

  let objectEditingMaskedEvents = maskCoreEvents (not <$> editingObject) coreEvents
  cameraState <- viewManip objectEditingMaskedEvents

  let defaultObject CameraState {..} = Object (mkTransformationMat identity focusPos) white "cube" "white.png" True True False 8
      eAddObj = (\o m -> (nextObjId m, o)) <$> (defaultObject <$> cameraState) <*> objects
        <@ B.filterE (==Key'A) (keyDown coreEvents)
      eDupeObjs = recalcIds <$> objects <*> selectedObjects <@ (B.whenE (keyHeldB coreEvents Key'LeftShift) $ B.filterE (==Key'D) (keyDown coreEvents))
      nextObjId m = fromMaybe 0 (maximumMay (Map.keys m)) + 1
      recalcIds objs forObjs = Map.fromList $ zip [nextObjId objs..] (Map.elems forObjs)
      eDeleteObjs = B.filterE (==Key'X) (keyDown coreEvents)
      pickExampleObject = fmap snd . headMay . Map.toList
      ePopulateEditorState = unionFirst
        [ B.filterJust $ pickExampleObject <$> eManipObjects
        , eSelectObject
        , snd <$> eAddObj
        ]

  let eModifyObjects :: B.Event (HashMap Int Object) = unionFirst
        [ eManipObjects
        , ePropertyEditedObjects
        ]
  -- let st = Map.fromList [(1, Object (mkTranslation (V3 1 0 0)) green), (2, Object (mkTranslation (V3 3 1 1)) white)]
  objects :: B.Behavior (HashMap Int Object) <- B.accumB initialScene $ B.unions
    [ uncurry Map.insert <$> eAddObj
    , Map.union <$> eModifyObjects
    , Map.union <$> eDupeObjs
    , flip Map.difference <$> selectedObjects <@ eDeleteObjs
    , const <$> eReplaceObjects
    ]

  renInfo <- B.stepper undefined (eRender coreEvents)

  let eClick = (\(Size w h) (_, V2 x y, _) -> (x/ realToFrac w, y/ realToFrac h))
           <$> scrSizeB coreEvents
           <@> fmap head (B.filterE ((<0.3) . view _1 . head) $ eTouchesUp objectEditingMaskedEvents)
  eScreenPickedObjectID <- fmap (\x -> if x > 0 then Just x else Nothing) <$> B.execute (((\(r,fc) -> liftIO . pickObjectID fc r) <$> renInfo) <@> eClick)
  (eGUIPickedObjectID, guiPickObjectID) <- B.newEvent
  let ePickedObjectID = unionFirst
        [ eScreenPickedObjectID
        , Just <$> eGUIPickedObjectID
        ]
  let eSelectObject = B.filterJust $ flip Map.lookup <$> objects <@> B.filterJust ePickedObjectID

  selectedObjectIDs <- B.accumB [] $ B.unions
    [ const . pure . fst <$> eAddObj
    , const . Map.keys <$> eDupeObjs
    , const [] <$ eDeleteObjs
    , (\shift oid -> case oid of
          Just i -> if shift then (i:) else const [i]
          Nothing -> const []

    )  <$> keyHeldB coreEvents Key'LeftShift <@> ePickedObjectID
    , const [] <$ eReplaceObjects
    ]
  let selectedObjects = (\m ks -> Map.filterWithKey (\k _ -> k `elem` ks) m) <$> objects <*> selectedObjectIDs

  (manipMode, eManipObjects) <- objectManip coreEvents cameraState selectedObjects eDupeObjs
  let editingObject = isJust <$> manipMode

  cursorLoc <- mkCursorLoc coreEvents

  let
      worldRender :: (MonadReader Resources m, CommandMonad m) => B.Behavior (m ())
      worldRender = editorWorldView <$> cameraState <*> selectedObjects <*> objects <*> manipMode
      overlayRender :: (MonadReader Resources m, CommandMonad m) => B.Behavior (m ())
      overlayRender = editorOverlayView <$> scrSizeB coreEvents <*> cameraState <*> cursorLoc <*> selectedObjects <*> (fmap fst <$> manipMode)

  let bRenderSettings = renderSettings <$> scrSizeB coreEvents <*> graphicsParams <*> pure (V4 0.07 0.07 0.07 1)
        <*> (view #viewMat <$> cameraState)
        <*> (view #projMat <$> cameraState)
        <*> (cameraState <&> \CameraState{..} -> focusPos - angleVec)
        <*> selectedObjectIDs

  let runRender renSettings litF overlayF (renderer, frameContext)
        = renderToRenderer frameContext renderer renSettings litF overlayF
  B.reactimate $ runRender
    <$> bRenderSettings
    <*> (flip runReaderT <$> resources <*> worldRender)
    <*> (flip runReaderT <$> resources <*> overlayRender)
    <@> eRender coreEvents

  ePropertyEditedObjects :: B.Event (HashMap Int Object) <- mkObjectChangeEvent coreEvents resourcesStore editorState selectedObjects ePopulateEditorState

  B.reactimate $ drawMainEditorUI editorState <$> sceneFile <*> selectedObjects <*> objects <*> pure guiPickObjectID <@ eRender coreEvents
  B.reactimate $ drawObjectEditorUI editorState <$> B.filterE (not . Map.null) (selectedObjects <@ eRender coreEvents)

  pure $ Map.elems <$> objects

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

renderSettings :: Size Int -> GraphicsParams -> V4 Scalar -> Mat44 -> Mat44 -> V3 Scalar -> [Int] -> RenderSettings
renderSettings size@(Size w _h) GraphicsParams {..} clearColor viewMat projMat camPos selectedObjIds = RenderSettings
  { worldGlobals = worldGlobalDefaults
    { viewMat = viewMat
    , projMat = projMat
    , cameraPos      = camPos
    , lightTransform = identity
    , lightDirection = sunDirection
    , sunColor = sunLight ^* sunStrength
    , ambientColor = ambientLight ^* ambientStrength
    }
  , overlayGlobals = OverlayGlobals
    { viewMat = viewTarget zero (V3 0 0 1) (V3 0 (-1) 0)
    , projMat = shotMatrix (Ortho (realToFrac w) 0 1 False) (aspectRatio size)
    }
  , postSettings = H.PostConstants exposure colorShift saturation filmGrain
  , clearColor = clearColor
  , highlightObjs = selectedObjIds
  }
