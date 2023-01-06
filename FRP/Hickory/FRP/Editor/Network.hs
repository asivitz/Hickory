{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Hickory.FRP.Editor.Network where

import qualified Reactive.Banana as B
import Hickory.FRP.CoreEvents (CoreEvents (..), maskCoreEvents)
import qualified Reactive.Banana.Frameworks as B
import Reactive.Banana ((<@>), (<@), liftA2)
import Hickory.Color (white)
import Hickory.Math (mkScale, viewTarget, mkTranslation, glerp, Scalar)
import Hickory.Types (Size (..), aspectRatio)
import Hickory.Camera (shotMatrix, Projection (..), Camera (..), project, cameraFocusPlaneSize)
import Data.Maybe (fromMaybe, isJust)
import Hickory.FRP.Combinators (unionFirst)
import Hickory.Input (Key(..))
import Linear (axisAngle, identity, Quaternion (..), M44, translation, mkTransformationMat, fromQuaternion, m33_to_m44, unit, Epsilon(..), column, V3 (..), V2 (..), V4 (..), (!*!), normalize, (^*), _x, _y, _z, cross, norm, zero)
import qualified Data.HashMap.Strict as Map
import Hickory.Math.Vector (v2angle)
import Hickory.Vulkan.Forward.Renderer (pickObjectID)
import Control.Monad.IO.Class (liftIO)
import Hickory.FRP.DearImGUIHelpers (tripleToV3, v3ToTriple, v4ToImVec4, imVec4ToV4)
import Control.Lens (traversed, (^.), (&), (%~), (<&>), view, _1, _3, (.~), at, _Just, (^?), ix, (?~))
import Data.HashMap.Strict (HashMap)
import Hickory.FRP.Editor.Types
import Hickory.FRP.Editor.GUI (drawObjectEditorUI, drawMainEditorUI, mkEditorState)
import Hickory.FRP.Editor.View (editorWorldView, editorOverlayView)
import Hickory.FRP.Editor.General (mkCursorLoc, matEuler, matScale, refChangeEvent)
import Hickory.Vulkan.Types (FrameContext)
import Hickory.Vulkan.Forward.Types (Renderer, CommandMonad, RenderSettings (..), OverlayGlobals (..), WorldSettings (..), worldSettingsDefaults)
import Data.Text (unpack, pack)
import Vulkan (SamplerAddressMode (..), Filter (..))
import Control.Monad.Reader (MonadReader)
import qualified Data.Vector.Storable as SV
import qualified Hickory.Vulkan.Types as H
import qualified Hickory.Vulkan.Mesh as H
import Hickory.Resources (ResourcesStore (..), loadResource', loadResource, Resources (..))
import Safe (maximumMay, headMay)
import Data.Foldable (for_)
import Hickory.FRP.Editor.Post (GraphicsParams (..))
import Data.Functor.Const (Const(..))
import Data.Traversable (for)
import Data.Functor.Identity (Identity(..))
import Type.Reflection ((:~~:)(..))
import Hickory.FRP.Camera (omniscientCamera)
import Hickory.FRP.Game (Scene(..))

objectManip :: CoreEvents a -> B.Behavior Camera -> B.Behavior (HashMap Int Object) -> B.Event (HashMap Int Object) -> B.MomentIO (B.Behavior (Maybe (ObjectManipMode, V3 Scalar)), B.Event (HashMap Int Object))
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
          f :: Size Int -> Camera -> Maybe (HashMap Int Object, V2 Scalar) -> V3 Scalar -> V2 Scalar -> Maybe (HashMap Int Object)
          f _ _ Nothing _ _ = Nothing
          f size@(Size scrW scrH) cam@Camera {..} (Just (objects, start)) axes v =
              let yaxis = up
                  xaxis = cross (normalize angleVec) (normalize up)
                  (V2 vx vy) = v - start
                  Size focusW focusH = cameraFocusPlaneSize size cam
              in Just $ objects & traversed . #transform %~
                (mkTranslation (liftA2 (*) axes (xaxis ^* (vx / realToFrac scrW * focusW) - yaxis ^* (vy / realToFrac scrH * focusH))) !*!)
        in f <$> scrSizeB coreEvents <*> cameraState <*> captured <*> activeAxes <@> (fst . head <$> eTouchesLoc coreEvents)

      eScaleObject :: B.Event (HashMap Int Object) = B.filterJust $ B.whenE ((==Just OScale) <$> mode) $
        let
          f _ _ Nothing _ _ = Nothing
          f ss cs (Just (objects, start)) axes v =
              let objv = project ss cs (avgObjTranslation objects)
                  ratio = norm (v - objv) / norm (start - objv)
              in Just $ objects & traversed . #transform %~ (!*! mkScale ((\fr -> glerp fr 1 ratio) <$> axes))
        in f <$> scrSizeB coreEvents <*> cameraState <*> captured <*> activeAxes <@> (fst . head <$> eTouchesLoc coreEvents)

      eRotateObject :: B.Event (HashMap Int Object) = B.filterJust $ B.whenE ((==Just ORotate) <$> mode) $
        let
          f _ _ Nothing _ = Nothing
          f ss cs@Camera {..} (Just (objects, start)) v =
              let objv = project ss cs (avgObjTranslation objects)
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
  setVal posChange (transform ^. translation)
  setVal rotChange (matEuler transform)
  setVal scaChange (matScale transform)
  setVal colorChange color
  setVal modelChange model
  setVal textureChange texture
  setVal litChange lit
  setVal castsShadowChange castsShadow
  setVal blendChange blend
  setVal specularityChange specularity
  setVal componentsChange  (Map.keys components)

  for_ (Map.toList componentChanges) \((compName, attrName), SomeAttribute attr change) ->
    case components ^? ix compName . ix attrName of
      Just (SomeAttribute attr' (Identity v)) -> case eqAttr attr attr' of
        Just HRefl -> setVal change v
        Nothing -> error "Attributes don't match"
      _ -> setVal change (defaultAttrVal attr)

mkChangeEvents :: HashMap String Component -> CoreEvents a -> EditorState -> B.MomentIO EditorChangeEvents
mkChangeEvents componentDefs coreEvents EditorState {..} = do
  posChange   <- bimapEditorChange (fmap tripleToV3) (.v3ToTriple) <$> refChangeEvent coreEvents posRef
  scaChange   <- bimapEditorChange (fmap tripleToV3) (.v3ToTriple) <$> refChangeEvent coreEvents scaRef
  rotChange   <- bimapEditorChange (fmap tripleToV3) (.v3ToTriple) <$> refChangeEvent coreEvents rotRef
  colorChange <- bimapEditorChange (fmap imVec4ToV4) (.v4ToImVec4) <$> refChangeEvent coreEvents colorRef
  modelChange <- bimapEditorChange (fmap unpack)     (.pack)       <$> refChangeEvent coreEvents modelRef
  textureChange     <- bimapEditorChange (fmap unpack) (.pack)     <$> refChangeEvent coreEvents textureRef
  litChange         <- refChangeEvent coreEvents litRef
  castsShadowChange <- refChangeEvent coreEvents castsShadowRef
  blendChange       <- refChangeEvent coreEvents blendRef
  specularityChange <- refChangeEvent coreEvents specularityRef
  componentsChange <- refChangeEvent coreEvents componentsRef

  componentChanges <- Map.fromList . concat <$> for (Map.toList componentDefs) \(name, Component{..}) ->
    for attributes \(SomeAttribute attr (Const attrName)) ->
      case Map.lookup (name, attrName) componentData of
        Just (SomeAttributeRef attr' ref) -> case eqAttr attr attr' of
          Just HRefl -> case proveAttrClasses attr of
            AttrClasses -> ((name, attrName),) . SomeAttribute attr . bimapEditorChange (fmap fromAttrRefType) (.toAttrRefType) <$> refChangeEvent coreEvents ref
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
  :: HashMap String Component
  -> CoreEvents a
  -> ResourcesStore
  -> EditorState
  -> B.Behavior (HashMap Int Object)
  -> B.Event Object
  -> B.MomentIO (B.Event (HashMap Int Object))
mkObjectChangeEvent componentDefs coreEvents ResourcesStore {..} editorState selectedObjects ePopulateEditorState = do
  eca@EditorChangeEvents {..} <- mkChangeEvents componentDefs coreEvents editorState

  B.reactimate $ writeEditorState eca <$> ePopulateEditorState
  B.reactimate $ (\m -> loadResource meshes m ()) <$> ev modelChange
  B.reactimate $ (\t -> loadResource textures t (FILTER_NEAREST, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)) <$> ev textureChange

  let compEvs :: [B.Event (HashMap Int Object)] = Map.toList componentChanges <&> \((compName, attrName), SomeAttribute attr ch) ->
        (\os v -> os & traversed . #components . at compName . _Just . at attrName ?~ SomeAttribute attr (Identity v))
          <$> selectedObjects <@> ev ch

  pure $ unionFirst $
    [ (\os v -> os & traversed . #transform . translation .~ v) <$> selectedObjects <@> ev posChange
    , (\os v -> os & traversed . #transform %~ setScale v)      <$> selectedObjects <@> ev scaChange
    , (\os v -> os & traversed . #transform %~ setRotation v)   <$> selectedObjects <@> ev rotChange
    , (\os v -> os & traversed . #color .~ v)                   <$> selectedObjects <@> ev colorChange
    , (\os v -> os & traversed . #model .~ v)                   <$> selectedObjects <@> ev modelChange
    , (\os v -> os & traversed . #texture .~ v)                 <$> selectedObjects <@> ev textureChange
    , (\os v -> os & traversed . #lit .~ v)                     <$> selectedObjects <@> ev litChange
    , (\os v -> os & traversed . #castsShadow .~ v)             <$> selectedObjects <@> ev castsShadowChange
    , (\os v -> os & traversed . #blend .~ v)                   <$> selectedObjects <@> ev blendChange
    , (\os v -> os & traversed . #specularity .~ v)             <$> selectedObjects <@> ev specularityChange
    , (\os v -> os & traversed . #components %~ syncMap v) <$> selectedObjects <@> ev componentsChange
    ] ++ compEvs
  where
  syncMap ks m = let om = Map.fromList $ (,mempty) <$> ks
                 in Map.intersection (Map.union m om) om

editorNetwork
  :: forall m. (MonadReader Resources m, CommandMonad m)
  => H.VulkanResources
  -> ResourcesStore
  -> CoreEvents (Renderer, FrameContext)
  -> HashMap String Component
  -> B.Event FilePath
  -> ([Object] -> m ()) -- Extra, optional, global drawing function
  -> B.MomentIO (B.Behavior (Scene m), B.Behavior [Object])
editorNetwork vulkanResources resourcesStore coreEvents componentDefs eLoadScene xtraView = mdo
  editorState <- liftIO (mkEditorState componentDefs)
  sceneFile <- B.stepper (error "No scene file") eLoadScene

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

  let objectEditingMaskedEvents = maskCoreEvents (not <$> editingObject) coreEvents
  cameraState <- omniscientCamera objectEditingMaskedEvents

  let defaultObject Camera {..} = Object (mkTransformationMat identity focusPos) white "cube" "white" True True False 8 mempty
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
  objects :: B.Behavior (HashMap Int Object) <- B.accumB mempty $ B.unions
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
      worldRender :: B.Behavior (m ())
      worldRender = editorWorldView componentDefs <$> cameraState <*> selectedObjects <*> objects <*> manipMode
      overlayRender :: B.Behavior (m ())
      overlayRender = editorOverlayView <$> scrSizeB coreEvents <*> cameraState <*> cursorLoc <*> selectedObjects <*> (fmap fst <$> manipMode)

  ePropertyEditedObjects :: B.Event (HashMap Int Object) <- mkObjectChangeEvent componentDefs coreEvents resourcesStore editorState selectedObjects ePopulateEditorState

  B.reactimate $ drawMainEditorUI editorState <$> sceneFile <*> selectedObjects <*> objects <*> pure guiPickObjectID <@ eRender coreEvents
  B.reactimate $ drawObjectEditorUI componentDefs editorState <$> B.filterE (not . Map.null) (selectedObjects <@ eRender coreEvents)

  let scene = Scene <$> ((>>) <$> worldRender <*> (xtraView <$> (Map.elems <$> objects)))
                    <*> overlayRender
                    <*> cameraState
                    <*> selectedObjectIDs

  pure (scene, Map.elems <$> objects)

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
    { viewMat = viewTarget zero (V3 0 0 1) (V3 0 (-1) 0)
    , projMat = shotMatrix (Ortho (realToFrac w) 0 1 False) (aspectRatio size)
    }
  , postSettings = H.PostConstants exposure colorShift saturation filmGrain
  , clearColor = clearColor
  , highlightObjs = selectedObjIds
  }
