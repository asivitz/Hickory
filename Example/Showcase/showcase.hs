{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Time.Clock (NominalDiffTime)
import Hickory.Camera
import Hickory.Math (mkTranslation, mkScale, viewTarget, Scalar, mkRotation)
import Hickory.Types
import Linear ( V2(..), V3(..), V4(..), (!*!), transpose, _m33, inv33, identity, (^*), liftI2, (!*))
import Acquire (Acquire)
import Control.Lens ((^.), filtered, (^?), each, runIdentity, view)
import Foreign (poke)
import Control.Monad (void, when, join, mfilter)
import Hickory.FRP.Editor (drawPostUI, GraphicsParams (..))

import qualified Platforms.GLFW.Vulkan as GLFWV
import qualified Hickory.Vulkan.Types as H
import qualified Hickory.Vulkan.Renderer.Types as H
import qualified Hickory.Vulkan.Renderer.Renderer as H


import Hickory.Vulkan.Vulkan
import Hickory.Color (black, white)
import Hickory.Vulkan.Renderer.Types (OverlayGlobals(..), DrawCommand (..), Features (..))
import Platforms.GLFW.Vulkan (initGLFWVulkan)
import Hickory.Resources (ResourcesStore(..), withResourcesStore, getMesh, getTexture, getResourcesStoreResources, Resources, loadFontResource, runResources, loadResource', getTextureMay)
import Platforms.GLFW.GameLoop (glfwGameLoop)
import Hickory.GameLoop (newGameStateStack, stepGameState, queryGameState)
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import Data.Word (Word32)
import Hickory.Vulkan.Renderer.GBuffer (loadGBufTextures)
import Hickory.Vulkan.Types (Mesh(..), Attribute (..), DescriptorSpec (..), lutLoadOptions)
import qualified Text.GLTF.Loader as GLTF
import Text.GLTF.Loader (Gltf)
import qualified Data.Vector as V
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import qualified Data.Vector.Storable as SV
import Data.Maybe (fromMaybe)
import Linear.V (toV, toVector)
import Text.Printf (printf)
import Hickory.Vulkan.Mesh (withBufferedMesh)
import Data.Fixed (mod')
import Hickory.Vulkan.DescriptorSet (withDescriptorSet, withTextureDescriptorSet, TextureDescriptorSet(..))
import Vulkan (Extent2D(..))
import Hickory.Vulkan.Renderer.Environment (renderEnvironmentMap, renderIrradianceMap)

pullMeshesFromGltf :: HasCallStack => Gltf -> Text -> V.Vector Mesh
pullMeshesFromGltf gltf name = fromMaybe V.empty do
  gltfMesh :: GLTF.Mesh <- gltf ^? GLTF._meshes . each . filtered (\m -> m.meshName == Just name)
  pure $ V.map (primitiveToMesh (V.toList gltfMesh.meshTargetNames)) gltfMesh.meshPrimitives
  where
  primitiveToMesh :: [Text] -> GLTF.MeshPrimitive -> Mesh
  primitiveToMesh targetNames GLTF.MeshPrimitive {..} = Mesh vertices (Just (SV.convert . V.map fromIntegral $ meshPrimitiveIndices)) minPosition maxPosition morphTargets
    where
    vertices = filter (not . SV.null . snd)
               [ (Position, flattenAndConvert id meshPrimitivePositions)
               , (Normal, flattenAndConvert id meshPrimitiveNormals)
               , (TextureCoord, flattenAndConvert id meshPrimitiveTexCoords)
               , (Tangent, flattenAndConvert id meshPrimitiveTangents)
               , (JointIndices, flattenAndConvert realToFrac meshPrimitiveJoints)
               , (JointWeights, flattenAndConvert id meshPrimitiveWeights)
               ]
    flattenAndConvert conv = SV.convert . V.concatMap (toVector . toV . fmap conv)
    minPosition = V.foldl1' (liftI2 min) meshPrimitivePositions
    maxPosition = V.foldl1' (liftI2 max) meshPrimitivePositions
    morphTargets = zip targetNames (map morphTargetAttributes (V.toList meshPrimitiveTargets))
    morphTargetAttributes GLTF.MorphTarget {..} = filter (not . SV.null . snd)
      [ (Position, flattenAndConvert id morphTargetPositions)
      , (Normal, flattenAndConvert id morphTargetNormals)
      , (TextureCoord, flattenAndConvert id morphTargetTexCoords)
      , (Tangent, flattenAndConvert id morphTargetTangents)
      , (JointIndices, flattenAndConvert realToFrac morphTargetJoints)
      , (JointWeights, flattenAndConvert id morphTargetWeights)
      ]

pullMeshFromGltf :: HasCallStack => Gltf -> Text -> Int -> Mesh
pullMeshFromGltf gltf name i = fromMaybe (error $ printf "Can't find mesh %d in %s" i name) $ pullMeshesFromGltf gltf name V.!? i

type Model = Scalar

-- instance Interpolatable Model where
--   glerp fr a b = b

-- By default, our firingDirection is to the right
newModel :: Model
newModel = 0

-- Load meshes, textures, materials, fonts, etc.
loadResources :: String -> H.VulkanResources -> Acquire ResourcesStore
loadResources path vulkanResources = do
  resourcesStore <- withResourcesStore vulkanResources
  liftIO do
    Right glb <- GLTF.fromBinaryFile (path ++ "/bunny.glb")
    join $ loadResource' resourcesStore.meshes "bunny" do
      withBufferedMesh vulkanResources (Just "bunny") $ pullMeshFromGltf glb.unGltf "bunny" 0

    let loadUberTextures name alb nor = join $ loadResource' resourcesStore.textures name do
          loadGBufTextures vulkanResources (path ++ "/" ++ alb) (path ++ "/" ++ nor)
    loadUberTextures "blank" "white.png" "blue.png"
    -- loadTextureResource vulkanResources resourcesStore (path ++ "images/white.png") (FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, Nothing)

    -- gidolinya.json (font data) and gidolinya.png (font texture) were
    -- generated using https://github.com/Chlumsky/msdf-atlas-gen
    loadFontResource vulkanResources resourcesStore (path ++ "fonts/gidolinya.json") (path ++ "images/gidolinya.png") 2

  envDesSpec <- renderEnvironmentMap vulkanResources (Extent2D 512 512) (path ++ "sky/sky7.hdr")
  irrDesSpec <- renderIrradianceMap vulkanResources (Extent2D 32 32) envDesSpec

  liftIO do
    loadResource' resourcesStore.textures "envMap" do
      withDescriptorSet vulkanResources [envDesSpec, irrDesSpec]

  liftIO do
    loadResource' resourcesStore.textures "lut" do
      withDescriptorSet vulkanResources [ImageFileDescriptor (path ++ "lut11.png", lutLoadOptions)]

  pure resourcesStore

mkRenderSettings :: Size Int -> GraphicsParams -> V4 Scalar -> Maybe H.PointedDescriptorSet -> Maybe H.PointedDescriptorSet -> Camera -> [Word32] -> H.RenderSettings
mkRenderSettings size@(Size w _) GraphicsParams {..} clearColor envMap lut camera selectedObjIds = H.RenderSettings
  { worldSettings = H.WorldSettings
    { camera
    , lightTransform = identity
    , lightDirection = sunDirection
    , sunColor       = sunLight ^* sunStrength
    , ambientColor   = ambientLight ^* ambientStrength
    , envMap         = envMap
    , envMapStrength = envMapStrength
    , irradianceStrength = irradianceStrength
    }
  , overlayGlobals = OverlayGlobals
    { viewMat = overlayViewMat
    , projMat = overlayProjMat
    , viewProjMat = overlayViewProj
    }
  , postSettings = H.PostConstants exposure colorShift saturation filmGrain falseColor
  , clearColor = clearColor
  , highlightObjs = selectedObjIds
  , ssaoSettings = H.SSAOSettings (fromIntegral ssaoKernelSize) ssaoKernelRadius
  , shadowBiasSlope = shadowBiasSlope
  , features = features
  , lut = mfilter (const applyLut) lut
  }
  where
  overlayViewMat = viewTarget (V3 0 0 (-1)) (V3 0 0 1) (V3 0 (-1) 0)
  overlayProjMat = shotMatrix (Ortho (realToFrac w) 1 100 False) (aspectRatio size)
  overlayViewProj = overlayProjMat !*! overlayViewMat

-- Our render function
renderGame :: (MonadIO m) => GraphicsParams -> Resources -> Model -> Size Int -> (H.Renderer, H.FrameContext) -> m ()
renderGame graphicsParams res t scrSize@(Size _ _h) (renderer, frameContext)
  = void $ H.renderToRenderer frameContext renderer renderSettings litF overlayF
  where
  envMap = runIdentity . runResources res $ getTextureMay "envMap"
  lut = runIdentity . runResources res $ getTextureMay "lut"
  camera = Camera (V3 0 0 0) (view _m33 (mkRotation (V3 0 0 1) (t/2)) !* V3 7 0 0) (V3 0 0 1) (Perspective (pi/2) 1 100) "Main" Nothing
  renderSettings = mkRenderSettings scrSize graphicsParams black envMap lut camera []
  litF = runResources res do
    mesh <- getMesh "bunny"
    tex <- getTexture "blank"
    let itemScale = 3
        pos = V3 0 0 (-2)

    let mat = mkTranslation pos !*!  mkScale (V3 itemScale itemScale itemScale)
    H.addCommand $ DrawCommand
      { instances = [("", [(0,mat)])]
      , mesh = H.Buffered mesh
      , pokeData = \_ -> flip poke $ H.StaticConstants
          { modelMat    = mat
          , normalMat   = transpose . inv33 $ mat ^. _m33
          , color       = white
          , material    = V4 ((t `mod'` 5) / 5) 0 0 0
          , tiling      = V2 1 1
          }
      , cull = False
      , doCastShadow = False
      , doBlend = True
      , descriptorSet = Just tex
      , materialConfig = renderer.staticGBufferMaterialConfig
      }

  overlayF = runResources res do
    pure ()
    -- textRenderer <- getSomeFont
    -- H.drawText renderer.msdfMaterialConfig textRenderer (mkTranslation (topLeft 20 20 scrSize) !*! mkScale (V2 (12/12) (12/12)))
    --   white white 0 $ textcommand { text = "Arrow keys move, Space shoots", align = AlignLeft }

stepF :: a -> Model -> (Model, [()])
stepF _ t = (t + realToFrac physicsTimeStep, [])

physicsTimeStep :: NominalDiffTime
physicsTimeStep = 1/20

graphicsParamsDefaults :: GraphicsParams
graphicsParamsDefaults = GraphicsParams {..}
  where
  exposure = 1
  colorShift = V3 1 1 1
  saturation = 1
  filmGrain = 0
  ambientLight = V3 1 1 1
  ambientStrength = 0.1
  envMapStrength = 1
  irradianceStrength = 1
  sunLight = V3 1 1 1
  sunStrength = 1
  sunDirection = V3 0 0.7 (-1)
  ssaoKernelSize = 1
  ssaoKernelRadius = 1
  shadowBiasSlope = 0.003
  features = Features True True True True
  falseColor = False
  applyLut = True

main :: IO ()
main = GLFWV.withWindow 750 750 "Showcase" \win -> runAcquire do
  vulkanResources <- initGLFWVulkan win
  resStore <- loadResources "Example/Showcase/assets/" vulkanResources
  liftIO do
    res <- getResourcesStoreResources resStore
    let shouldShowPost = True

    graphicsParamsRef <- newIORef graphicsParamsDefaults

    let mkScene = do
          stack <- newIORef (newGameStateStack newModel)
          pure \inputFrame -> do
            _evs <- atomicModifyIORef' stack (stepGameState $ stepF inputFrame)
            pure $ \frac _ size (sr,fc) -> do

              when shouldShowPost do
                drawPostUI graphicsParamsRef (sr,fc)

              graphicsParams <- readIORef graphicsParamsRef
              model <- queryGameState <$> readIORef stack
              renderGame graphicsParams res (model (2 - frac)) size (sr, fc)

    glfwGameLoop win vulkanResources (H.withRenderer vulkanResources) physicsTimeStep $ const mkScene
