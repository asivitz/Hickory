{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.Vulkan.Forward.Renderer where

import Hickory.Vulkan.Forward.Types (Renderer (..), castsShadow, DrawCommand (..), StaticConstants (..), MeshType (..), AnimatedMesh (..), AnimatedConstants (..), Command, MSDFMesh (..), RenderSettings (..), StaticMesh (..), DrawType (..), addCommand, CommandMonad, runCommand)
import Hickory.Vulkan.Vulkan (VulkanResources(..), Swapchain (..), mkAcquire, DeviceContext (..))
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.ForwardRenderTarget (withPostProcessMaterial)
import Linear (V4 (..), transpose, inv33, _m33, V2 (..), V3 (..), (!*!))
import Hickory.Vulkan.Monad (FrameMonad, askFrameContext, material, BufferedUniformMaterial (..), cmdDrawBufferedMesh, getMeshes, addMesh, askDynamicMesh, useDynamicMesh, DynamicMeshMonad, textMesh)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hickory.Vulkan.Types (PostConstants(..), RenderTarget (..), DescriptorSpec (..), PointedDescriptorSet, buf, hasPerDrawDescriptorSet, Material(..))
import Hickory.Vulkan.Frame (FrameContext(..))
import Hickory.Vulkan.Text (withMSDFMaterial, MSDFMatConstants (..), TextRenderer)
import Hickory.Vulkan.Forward.Lit (withStaticUnlitMaterial, withAnimatedLitMaterial, withLitRenderTarget, withStaticLitMaterial, withLineMaterial)
import Hickory.Vulkan.Forward.ShadowPass (withAnimatedShadowMaterial, withShadowRenderTarget, withStaticShadowMaterial, shadowMapSize)
import Hickory.Vulkan.RenderPass (withSwapchainRenderTarget, useRenderTarget)
import Hickory.Vulkan.Mesh (BufferedMesh (..), vsizeOf, vertices, indices)
import Vulkan (ClearValue (..), ClearColorValue (..), cmdDraw, ClearDepthStencilValue (..), bindings, withDescriptorSetLayout)
import Foreign (Storable)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet, BufferDescriptorSet (..), descriptorSetBindings, withDataBuffer, uploadBufferDescriptor, uploadBufferDescriptorArray)
import Control.Lens (view, (^.), (.~), (&), _1, _2, _3, _4)
import Hickory.Vulkan.Framing (doubleResource, resourceForFrame, frameResource)
import Hickory.Vulkan.Material (cmdBindMaterial, cmdPushMaterialConstants, cmdBindDrawDescriptorSet)
import Data.List (partition, sortOn)
import Data.Foldable (for_)
import Hickory.Vulkan.DynamicMesh (DynamicBufferedMesh(..), withDynamicBufferedMesh)
import qualified Data.Vector as V
import Vulkan.Zero (zero)
import Hickory.Vulkan.Forward.ObjectPicking (withObjectIDMaterial, withObjectIDRenderTarget)
import Linear.Matrix (M44)
import Hickory.Text (TextCommand(..))
import Control.Monad (when, unless)
import Hickory.Types (aspectRatio)
import Hickory.Camera (shotMatrix, Projection (..))
import Hickory.Math (viewDirection)
import Data.Word (Word32)
import Data.IORef (modifyIORef, newIORef, readIORef, IORef)
import Data.UUID (UUID)
import Control.Monad.State.Class (MonadState, put)
import Control.Monad.State.Strict (evalStateT)
import qualified Data.UUID as UUID
import Control.Monad.State.Class (get)

withRenderer :: VulkanResources -> Swapchain -> Acquire Renderer
withRenderer vulkanResources@VulkanResources {deviceContext = DeviceContext{..}} swapchain = do
  shadowRenderTarget       <- withShadowRenderTarget vulkanResources
  litRenderTarget          <- withLitRenderTarget vulkanResources swapchain
  swapchainRenderTarget    <- withSwapchainRenderTarget vulkanResources swapchain
  objectIDRenderTarget     <- withObjectIDRenderTarget vulkanResources swapchain

  globalShadowPassBuffer   <- withDataBuffer vulkanResources 1
  globalWorldBuffer        <- withDataBuffer vulkanResources 1
  globalOverlayBuffer      <- withDataBuffer vulkanResources 1

  globalWorldDescriptorSet <- withDescriptorSet vulkanResources
    [ BufferDescriptor (buf globalWorldBuffer)
    , descriptorSpec shadowRenderTarget
    ]
  globalOverlayDescriptorSet <- withDescriptorSet vulkanResources
    [ BufferDescriptor (buf globalOverlayBuffer)
    ]

  globalShadowPassDescriptorSet <- withDescriptorSet vulkanResources
    [ BufferDescriptor (buf globalShadowPassBuffer)
    ]

  imageSetLayout <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"]]
    } Nothing mkAcquire

  objectIDMaterial         <- withObjectIDMaterial vulkanResources objectIDRenderTarget globalWorldDescriptorSet
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
    [ view #descriptorSpec litRenderTarget
    ]

  postProcessMaterial <- withPostProcessMaterial vulkanResources swapchainRenderTarget (doubleResource globalWorldDescriptorSet) (doubleResource postMaterialDescriptorSet)

  dynamicMesh <- frameResource $ withDynamicBufferedMesh vulkanResources 1000

  pure Renderer {..}

data RegisteredMaterial const extra
  = NullMat
  | Universal (BufferedUniformMaterial const) extra
  | LitAndUnlit (BufferedUniformMaterial const, extra) (BufferedUniformMaterial const, extra)

renderToRenderer :: (FrameMonad m, MonadIO m) => Renderer -> RenderSettings -> PostConstants -> Command () -> Command () -> m ()
renderToRenderer Renderer {..} RenderSettings {..} postConstants litF overlayF = do
  frameContext@FrameContext {..} <- askFrameContext
  useDynamicMesh (resourceForFrame frameNumber dynamicMesh) do
    let lightView = viewDirection (V3 0 10 20) (worldGlobals ^. #lightDirection) (V3 0 0 1) -- Trying to get the whole scene in view of the sun
        lightProj = shotMatrix (Ortho 100 0.1 45 True) (aspectRatio shadowMapSize)
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

    useRenderTarget swapchainRenderTarget commandBuffer [] swapchainImageIndex do
      cmdBindMaterial frameNumber commandBuffer postProcessMaterial
      liftIO $ cmdPushMaterialConstants commandBuffer postProcessMaterial postConstants
      liftIO $ cmdDraw commandBuffer 3 1 0 0

      processRenderPass frameContext
        ( NullMat
        , Universal staticOverlayMaterial ()
        , Universal msdfOverlayMaterial ()
        , NullMat
        ) $ runCommand overlayF

type MaterialSet
  = ( RegisteredMaterial AnimatedConstants ()
    , RegisteredMaterial StaticConstants ()
    , RegisteredMaterial MSDFMatConstants ()
    , RegisteredMaterial StaticConstants ()
    )

type MaterialConfig c = RegisteredMaterial c (IORef [c])

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
  let regMatToConfig :: RegisteredMaterial a () -> IO (MaterialConfig a)
      regMatToConfig = \case
        NullMat -> pure NullMat
        Universal mat () -> Universal mat <$> newIORef []
        LitAndUnlit (mat1,()) (mat2,()) -> LitAndUnlit <$> ((mat1,) <$> newIORef [])
                                                       <*> ((mat2,) <$> newIORef [])
  config <- liftIO $
    (,,,) <$> regMatToConfig (view _1 mps)
          <*> regMatToConfig (view _2 mps)
          <*> regMatToConfig (view _3 mps)
          <*> regMatToConfig (view _4 mps)

  let
      (blended, opaque) = partition blend commands

  flip evalStateT UUID.nil do
    -- Sort opaque by material to minimize binding
    for_ (sortOn identifyDCMat opaque) $ processCommand fc config
    -- Render blended in order to maintain visual layering
    for_ blended $ processCommand fc config

  -- IORefs are now bursting full of uniform goodness. Here comes the airplane. Yum!
  uploadUnis $ view _1 config
  uploadUnis $ view _2 config
  uploadUnis $ view _3 config
  uploadUnis $ view _4 config
  where
  uploadUnis :: (Storable a, MonadIO m) => MaterialConfig a -> m ()
  uploadUnis = \case
    Universal mat ref -> uploadConfig (mat,ref)
    LitAndUnlit c1 c2 -> uploadConfig c1 >> uploadConfig c2
    NullMat -> pure ()
  uploadConfig (mat, ref) = do
    unis <- liftIO (readIORef ref)
    unless (null unis) do
      uploadUniformsBuffer fc mat $ reverse unis
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
    , color = color
    , boneMat = boneMat
    , colors = colors
    }
  Static StaticMesh {..} -> go staticConfig $ (, Just albedo) StaticConstants
    { modelMat  = modelMat
    , normalMat = transpose . inv33 $ modelMat ^. _m33
    , color = color
    , tiling = tiling
    }
  Lines -> go linesConfig $ (, Nothing) StaticConstants
    { modelMat  = modelMat
    , normalMat = transpose . inv33 $ modelMat ^. _m33
    , color = color
    , tiling = V2 1 1
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
  go c (uniform, ds) = do
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
    }
