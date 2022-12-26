{-# LANGUAGE DataKinds, OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.Vulkan.Forward.Renderer where

import Hickory.Vulkan.Forward.Types (Renderer (..), castsShadow, DrawCommand (..), StaticConstants (..), MeshType (..), AnimatedMesh (..), AnimatedConstants (..), Command, MSDFMesh (..), RenderSettings (..), StaticMesh (..), DrawType (..), addCommand, CommandMonad, runCommand, highlightObjs, Globals(..))
import Hickory.Vulkan.Vulkan ( mkAcquire)
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.PostProcessing (withPostProcessMaterial)
import Linear (V4 (..), transpose, inv33, _m33, V2 (..), V3 (..), (!*!), inv44, (!*), _x, _y, _z, _w, (^/))
import Hickory.Vulkan.Monad (material, BufferedUniformMaterial (..), cmdDrawBufferedMesh, getMeshes, addMesh, askDynamicMesh, useDynamicMesh, DynamicMeshMonad, textMesh)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hickory.Vulkan.Types (RenderTarget (..), DescriptorSpec (..), PointedDescriptorSet, buf, hasPerDrawDescriptorSet, Material(..), DeviceContext (..), VulkanResources (..), Swapchain, FrameContext (..), BufferedMesh (..), vertices, indices)
import Hickory.Vulkan.Text (withMSDFMaterial, MSDFMatConstants (..), TextRenderer)
import Hickory.Vulkan.Forward.Lit (withStaticUnlitMaterial, withAnimatedLitMaterial, withLitRenderTarget, withStaticLitMaterial, withLineMaterial)
import Hickory.Vulkan.Forward.ShadowPass (withAnimatedShadowMaterial, withShadowRenderTarget, withStaticShadowMaterial, shadowMapSize)
import Hickory.Vulkan.RenderPass (withSwapchainRenderTarget, useRenderTarget)
import Hickory.Vulkan.Mesh (vsizeOf)
import Vulkan (ClearValue (..), ClearColorValue (..), cmdDraw, ClearDepthStencilValue (..), bindings, withDescriptorSetLayout, BufferUsageFlagBits (..))
import Foreign (Storable)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet, BufferDescriptorSet (..), descriptorSetBindings, withDataBuffer, uploadBufferDescriptor, uploadBufferDescriptorArray)
import Control.Lens (view, (^.), (.~), (&), _1, _2, _3, _4, ix, (^?!))
import Hickory.Vulkan.Framing (doubleResource, resourceForFrame, frameResource)
import Hickory.Vulkan.Material (cmdBindMaterial, cmdPushMaterialConstants, cmdBindDrawDescriptorSet)
import Data.List (partition, sortOn)
import Data.Foldable (for_)
import Hickory.Vulkan.DynamicMesh (DynamicBufferedMesh(..), withDynamicBufferedMesh)
import qualified Data.Vector as V
import Vulkan.Zero (zero)
import Hickory.Vulkan.Forward.ObjectPicking (withObjectIDMaterial, withObjectIDRenderTarget, ObjectIDConstants (..), withObjectHighlightMaterial)
import Linear.Matrix (M44)
import Hickory.Text.Types ( TextCommand(..) )
import Control.Monad (when, unless)
import Hickory.Types (aspectRatio)
import Hickory.Camera (shotMatrix, Projection (..))
import Hickory.Math.Matrix ( viewDirection , viewTarget, perspectiveProjection)
import Data.Word (Word32)
import Data.IORef (modifyIORef, newIORef, readIORef, IORef)
import Data.UUID (UUID)
import Control.Monad.State.Class ( MonadState, put, get )
import Control.Monad.State.Strict (evalStateT)
import qualified Data.UUID as UUID
import Data.Maybe (isJust)
import Hickory.Vulkan.RenderTarget (copyDescriptorImageToBuffer, withImageBuffer, readPixel)
import Hickory.Math (Scalar, orthographicProjection)

withRenderer :: VulkanResources -> Swapchain -> Acquire Renderer
withRenderer vulkanResources@VulkanResources {deviceContext = DeviceContext{..}} swapchain = do
  shadowRenderTarget       <- withShadowRenderTarget vulkanResources
  litRenderTarget          <- withLitRenderTarget vulkanResources swapchain
  swapchainRenderTarget    <- withSwapchainRenderTarget vulkanResources swapchain
  pickingRenderTarget          <- withObjectIDRenderTarget vulkanResources swapchain
  currentSelectionRenderTarget <- withObjectIDRenderTarget vulkanResources swapchain

  globalBuffer             <- withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT
  globalShadowPassBuffer   <- withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT
  globalWorldBuffer        <- withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT
  globalOverlayBuffer      <- withDataBuffer vulkanResources 1 BUFFER_USAGE_UNIFORM_BUFFER_BIT

  globalWorldDescriptorSet <- withDescriptorSet vulkanResources $
    [ BufferDescriptor (buf globalBuffer)
    , BufferDescriptor (buf globalWorldBuffer)
    ]
    ++
    descriptorSpecs shadowRenderTarget
  globalOverlayDescriptorSet <- withDescriptorSet vulkanResources
    [ BufferDescriptor (buf globalBuffer)
    , BufferDescriptor (buf globalOverlayBuffer)
    ]

  globalShadowPassDescriptorSet <- withDescriptorSet vulkanResources
    [ BufferDescriptor (buf globalBuffer)
    , BufferDescriptor (buf globalShadowPassBuffer)
    ]

  -- For debugging
  shadowMapDescriptorSet <- withDescriptorSet vulkanResources $
    descriptorSpecs shadowRenderTarget

  imageSetLayout <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"]]
    } Nothing mkAcquire

  pickingMaterial          <- withObjectIDMaterial vulkanResources pickingRenderTarget globalWorldDescriptorSet
  currentSelectionMaterial <- withObjectIDMaterial vulkanResources currentSelectionRenderTarget globalWorldDescriptorSet

  staticShadowMaterial     <- withStaticShadowMaterial vulkanResources shadowRenderTarget globalShadowPassDescriptorSet
  animatedShadowMaterial   <- withAnimatedShadowMaterial vulkanResources shadowRenderTarget globalShadowPassDescriptorSet
  staticLitWorldMaterial   <- withStaticLitMaterial vulkanResources litRenderTarget globalWorldDescriptorSet imageSetLayout
  staticUnlitWorldMaterial <- withStaticUnlitMaterial vulkanResources litRenderTarget globalWorldDescriptorSet imageSetLayout
  animatedLitWorldMaterial <- withAnimatedLitMaterial vulkanResources litRenderTarget globalWorldDescriptorSet imageSetLayout

  msdfWorldMaterial        <- withMSDFMaterial vulkanResources litRenderTarget globalWorldDescriptorSet imageSetLayout
  linesWorldMaterial       <- withLineMaterial vulkanResources litRenderTarget globalWorldDescriptorSet

  staticOverlayMaterial    <- withStaticUnlitMaterial vulkanResources swapchainRenderTarget globalOverlayDescriptorSet imageSetLayout
  msdfOverlayMaterial      <- withMSDFMaterial vulkanResources swapchainRenderTarget globalOverlayDescriptorSet imageSetLayout


  postMaterialDescriptorSet <- withDescriptorSet vulkanResources
    [ litRenderTarget ^?! (#descriptorSpecs . ix 0)
    ]

  objHighlightDescriptorSet <- withDescriptorSet vulkanResources
    [ currentSelectionRenderTarget ^?! (#descriptorSpecs . ix 0)
    ]

  postProcessMaterial <- withPostProcessMaterial vulkanResources swapchainRenderTarget (doubleResource globalWorldDescriptorSet) (doubleResource postMaterialDescriptorSet)
  objHighlightMaterial <- withObjectHighlightMaterial vulkanResources litRenderTarget (doubleResource globalWorldDescriptorSet) (doubleResource objHighlightDescriptorSet)

  dynamicMesh <- frameResource $ withDynamicBufferedMesh vulkanResources 1000

  objectPickingImageBuffer <- frameResource $ withImageBuffer vulkanResources pickingRenderTarget 0

  pure Renderer {..}

data RegisteredMaterial const extra
  = NullMat
  | Universal (BufferedUniformMaterial const) extra
  | LitAndUnlit (BufferedUniformMaterial const, extra) (BufferedUniformMaterial const, extra)

ndcBoundaryPoints :: Num a => [V4 a]
ndcBoundaryPoints =
  [ V4 (-1) (-1) 0 1
  , V4 (-1)   1  0 1
  , V4   1    1  0 1
  , V4   1  (-1) 0 1
  , V4 (-1) (-1) 1 1
  , V4 (-1)   1  1 1
  , V4   1    1  1 1
  , V4   1  (-1) 1 1
  ]

viewBoundaryFromInvProjection :: forall a. (Fractional a, Num a, Ord a) => M44 a -> (a, a, a, a)
viewBoundaryFromInvProjection m =  (l,r,b,t)
  where
  frustumPoints = (\v -> v ^/ (v ^. _w)) . (m !*) <$> ndcBoundaryPoints
  l = minimum . fmap (^. _x) $ frustumPoints
  r = maximum . fmap (^. _x) $ frustumPoints
  b = maximum . fmap (^. _y) $ frustumPoints
  t = minimum . fmap (^. _y) $ frustumPoints

renderToRenderer :: (MonadIO m) => FrameContext -> Renderer -> RenderSettings -> Command () -> Command () -> m ()
renderToRenderer frameContext@FrameContext{..} Renderer {..} RenderSettings {..} litF overlayF = do
  useDynamicMesh (resourceForFrame frameNumber dynamicMesh) do
    let lightView = viewDirection (V3 0 0 0) (worldGlobals ^. #lightDirection) (V3 0 0 1) -- Trying to get the whole scene in view of the sun
        -- lightProj = shotMatrix (Ortho 100 1.0 85 True) (aspectRatio shadowMapSize)
        invvp = inv44 $ (worldGlobals ^. #projMat) !*! (worldGlobals ^. #viewMat)
        (l,r,b,t) = viewBoundaryFromInvProjection (lightView !*! invvp)
        lightProj = orthographicProjection l r b t 0.1 40
    uploadBufferDescriptor globalBuffer
      $ Globals frameNumber
    uploadBufferDescriptor globalWorldBuffer
      $ worldGlobals
      & #lightTransform .~ (lightProj !*! lightView)
    uploadBufferDescriptor globalShadowPassBuffer
      $ worldGlobals
      & #viewMat .~ lightView
      & #projMat .~ lightProj
      -- TODO: Dynamic based on camera pos/target

    uploadBufferDescriptor globalOverlayBuffer $ worldGlobals
                                               & #viewMat .~ (overlayGlobals ^. #viewMat)
                                               & #projMat .~ (overlayGlobals ^. #projMat)

    let drawCommands = runCommand litF

    useRenderTarget pickingRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0), Color (Uint32 0 0 0 0) ] swapchainImageIndex do
      processIDRenderPass frameContext (Universal pickingMaterial ()) $ filter (isJust . ident) drawCommands

    useRenderTarget currentSelectionRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0), Color (Uint32 0 0 0 0) ] swapchainImageIndex do
      processIDRenderPass frameContext (Universal currentSelectionMaterial ()) $ filter ((\x -> x `elem` map Just highlightObjs) . ident) drawCommands

    useRenderTarget shadowRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0) ] swapchainImageIndex do
      processRenderPass frameContext
        ( Universal animatedShadowMaterial ()
        , Universal staticShadowMaterial ()
        , NullMat
        , NullMat
        ) $ filter castsShadow drawCommands

    let V4 r g b a = clearColor
    useRenderTarget litRenderTarget commandBuffer
      [ Color (Float32 r g b a)
      , DepthStencil (ClearDepthStencilValue 1 0)
      , Color (Float32 1 1 1 1)
      ] swapchainImageIndex do
      processRenderPass frameContext
        ( Universal animatedLitWorldMaterial ()
        , LitAndUnlit (staticLitWorldMaterial, ()) (staticUnlitWorldMaterial, ())
        , Universal msdfWorldMaterial ()
        , Universal linesWorldMaterial ()
        ) drawCommands

      case highlightObjs of
        (_:_) -> do
          cmdBindMaterial frameNumber commandBuffer objHighlightMaterial
          liftIO $ cmdDraw commandBuffer 3 1 0 0
        _ -> pure ()

    useRenderTarget swapchainRenderTarget commandBuffer [] swapchainImageIndex do
      cmdBindMaterial frameNumber commandBuffer postProcessMaterial
      liftIO $ cmdPushMaterialConstants commandBuffer postProcessMaterial postSettings
      liftIO $ cmdDraw commandBuffer 3 1 0 0

      processRenderPass frameContext
        ( NullMat
        , Universal staticOverlayMaterial ()
        , Universal msdfOverlayMaterial ()
        , NullMat
        ) $ runCommand overlayF
    liftIO $ copyDescriptorImageToBuffer commandBuffer (resourceForFrame frameNumber objectPickingImageBuffer)

type MaterialConfig c = RegisteredMaterial c (IORef [c])

regMatToConfig :: RegisteredMaterial a () -> IO (MaterialConfig a)
regMatToConfig = \case
  NullMat -> pure NullMat
  Universal mat () -> Universal mat <$> newIORef []
  LitAndUnlit (mat1,()) (mat2,()) -> LitAndUnlit <$> ((mat1,) <$> newIORef [])
                                                 <*> ((mat2,) <$> newIORef [])

uploadUniforms :: (Storable a, MonadIO m) => FrameContext -> MaterialConfig a -> m ()
uploadUniforms fc = \case
  Universal mat ref -> uploadConfig (mat,ref)
  LitAndUnlit c1 c2 -> uploadConfig c1 >> uploadConfig c2
  NullMat -> pure ()
  where
  uploadConfig (mat, ref) = do
    unis <- liftIO (readIORef ref)
    unless (null unis) do
      uploadUniformsBuffer fc mat $ reverse unis

submitCommand :: (MonadIO m, DynamicMeshMonad m, MonadState UUID m) => FrameContext -> DrawCommand -> MaterialConfig a -> (a, Maybe PointedDescriptorSet) -> m ()
submitCommand frameContext dc@DrawCommand{..} c (uniform, ds) = do
  let config = case c of
        Universal mat ref -> (mat, ref)
        LitAndUnlit c1 c2 -> if lit then c1 else c2
        NullMat -> error "No mat registered to handle this draw command"
  unis <- liftIO $ readIORef (snd config)
  renderCommand frameContext (fst config) dc ds (fromIntegral $ length unis)
  liftIO $ modifyIORef (snd config) (uniform:)

renderCommand
  :: (MonadIO m, DynamicMeshMonad m, MonadState UUID m)
  => FrameContext
  -> BufferedUniformMaterial uniform
  -> DrawCommand
  -> Maybe PointedDescriptorSet
  -> Word32
  -> m ()
renderCommand FrameContext {..} BufferedUniformMaterial {..} DrawCommand {..} drawDS uniformIdx = do
  curUUID <- get
  when (curUUID /= uuid material) do
    cmdBindMaterial frameNumber commandBuffer material
    put curUUID

  cmdPushMaterialConstants commandBuffer material uniformIdx
  when (hasPerDrawDescriptorSet material) do
    for_ drawDS $ cmdBindDrawDescriptorSet commandBuffer material
  case mesh of
    Buffered BufferedMesh {mesh = mesh', ..} -> cmdDrawBufferedMesh commandBuffer material mesh' 0 vertexBuffer 0 indexBuffer
    Dynamic dyn -> do
      meshes <- getMeshes
      addMesh dyn

      -- This is O(n)... Might want to cache this
      let vertexSizeThusFar = sum $ map (sum . map (vsizeOf . snd) . vertices) meshes
          indexSizeThusFar  = sum $ map (maybe 0 vsizeOf . indices) meshes

      DynamicBufferedMesh { vertexBufferPair = (vertexBuffer,_), indexBufferPair = (indexBuffer,_) } <- askDynamicMesh
      cmdDrawBufferedMesh commandBuffer material dyn vertexSizeThusFar vertexBuffer (fromIntegral indexSizeThusFar) (Just indexBuffer)

uploadUniformsBuffer :: (MonadIO m, Storable a) => FrameContext -> BufferedUniformMaterial a -> [a] -> m ()
uploadUniformsBuffer FrameContext {..} BufferedUniformMaterial {..} uniforms = do
  let BufferDescriptorSet { dataBuffer } = resourceForFrame frameNumber descriptor

  uploadBufferDescriptorArray dataBuffer uniforms

drawText
  :: CommandMonad m
  => TextRenderer
  -> M44 Float
  -> V4 Float
  -> V4 Float
  -> Float
  -> TextCommand
  -> m ()
drawText (font, fontTex, sdfPixelRange) mat color outlineColor outlineSize tc =
  addCommand $ DrawCommand
    { modelMat = mat
    , mesh = Dynamic (textMesh font tc)
    , color = color
    , drawType = MSDF $ MSDFMesh fontTex outlineColor outlineSize sdfPixelRange (V2 1 1)
    , lit = False
    , castsShadow = False
    , blend = True
    , ident = Nothing
    , specularity = 8
    }

-- Main Command Processor

type MaterialSet
  = ( RegisteredMaterial AnimatedConstants ()
    , RegisteredMaterial StaticConstants ()
    , RegisteredMaterial MSDFMatConstants ()
    , RegisteredMaterial StaticConstants ()
    )

type MaterialConfigSet
  = ( MaterialConfig AnimatedConstants
    , MaterialConfig StaticConstants
    , MaterialConfig MSDFMatConstants
    , MaterialConfig StaticConstants
    )

processRenderPass
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> MaterialSet
  -> [DrawCommand]
  -> m ()
processRenderPass fc mps commands = do
  config <- liftIO $
    (,,,) <$> regMatToConfig (view _1 mps)
          <*> regMatToConfig (view _2 mps)
          <*> regMatToConfig (view _3 mps)
          <*> regMatToConfig (view _4 mps)

  let (blended, opaque) = partition blend commands

  flip evalStateT UUID.nil do
    -- Sort opaque by material to minimize binding
    for_ (sortOn identifyDCMat opaque) $ processCommand fc config
    -- Render blended in order to maintain visual layering
    for_ blended $ processCommand fc config

  -- IORefs are now bursting full of uniform goodness. Here comes the airplane. Yum!
  uploadUniforms fc $ view _1 config
  uploadUniforms fc $ view _2 config
  uploadUniforms fc $ view _3 config
  uploadUniforms fc $ view _4 config
  where
  identifyDCMat :: DrawCommand -> Int
  identifyDCMat DrawCommand {..} = (if lit then 2 else 1) * case drawType of
    Animated _ -> 4
    Static   _ -> 3
    MSDF     _ -> 2
    Lines      -> 1 -- Draw lines first. Might need to be configurable

processCommand
  :: (MonadIO m, DynamicMeshMonad m, MonadState UUID m)
  => FrameContext
  -> MaterialConfigSet
  -> DrawCommand
  -> m ()
processCommand frameContext
  ( animatedConfig
  , staticConfig
  , msdfConfig
  , linesConfig
  )
  dc@DrawCommand {..} = case drawType of
  Animated AnimatedMesh {..} -> go animatedConfig $ (,Just albedo) AnimatedConstants
    { modelMat  = modelMat
    , normalMat = transpose . inv33 $ modelMat ^. _m33
    , color
    , boneMat
    , colors
    , specularity
    }
  Static StaticMesh {..} -> go staticConfig $ (, Just albedo) StaticConstants
    { modelMat  = modelMat
    , normalMat = transpose . inv33 $ modelMat ^. _m33
    , color = color
    , tiling = tiling
    , specularity
    }
  Lines -> go linesConfig $ (, Nothing) StaticConstants
    { modelMat  = modelMat
    , normalMat = transpose . inv33 $ modelMat ^. _m33
    , color = color
    , tiling = V2 1 1
    , specularity
    }
  MSDF MSDFMesh {..} -> go msdfConfig $ (, Just tex) MSDFMatConstants
    { modelMat  = modelMat
    , outlineColor = outlineColor
    , outlineSize = outlineSize
    , sdfPixelRange = sdfPixelRange
    , tiling = tiling
    , color = color
    }

  where
  go = submitCommand frameContext dc

-- Object ID Command Processor

processIDRenderPass
  :: (MonadIO m, DynamicMeshMonad m)
  => FrameContext
  -> RegisteredMaterial ObjectIDConstants ()
  -> [DrawCommand]
  -> m ()
processIDRenderPass fc mps commands = do
  config <- liftIO $ regMatToConfig mps

  flip evalStateT UUID.nil do
    for_ commands $ processIDCommand fc config

  -- IORefs are now bursting full of uniform goodness. Here comes the airplane. Yum!
  uploadUniforms fc config

processIDCommand
  :: (MonadIO m, DynamicMeshMonad m, MonadState UUID m)
  => FrameContext
  -> MaterialConfig ObjectIDConstants
  -> DrawCommand
  -> m ()
processIDCommand frameContext
  objectConfig
  dc@DrawCommand {..} = case ident of
    Just i ->
      go objectConfig $ (,Nothing) ObjectIDConstants
        { modelMat = modelMat
        , objectID = fromIntegral i
        }
    _ -> pure ()

  where
  go = submitCommand frameContext dc

pickObjectID :: FrameContext -> Renderer -> (Scalar,Scalar) -> IO Int
pickObjectID FrameContext {..} Renderer{..} = fmap fromIntegral . readPixel (resourceForFrame (frameNumber - 1) objectPickingImageBuffer)
