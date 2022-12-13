{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.Forward.Renderer where

import Hickory.Vulkan.Forward.Types (Renderer (..), castsShadow, DrawCommand (..), unCommandT, StaticConstants (..), MeshType (..), AnimatedMesh (..), AnimatedConstants (..), Command, MSDFMesh (..), RenderSettings (..), StaticMesh (..))
import Hickory.Vulkan.Vulkan (VulkanResources(..), Swapchain (..), mkAcquire, DeviceContext (..))
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.ForwardRenderTarget (withPostProcessMaterial)
import Linear (V4 (..), transpose, inv33, _m33, V2 (..))
import Hickory.Vulkan.Monad (FrameMonad, askFrameContext, material, BufferedUniformMaterial (..), cmdDrawBufferedMesh, getMeshes, addMesh, askDynamicMesh, useDynamicMesh, DynamicMeshMonad)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hickory.Vulkan.Types (PostConstants(..), RenderTarget (..), DescriptorSpec (..), PointedDescriptorSet, buf)
import Hickory.Vulkan.Frame (FrameContext(..))
import Hickory.Vulkan.Text (withMSDFMaterial, MSDFMatConstants (..))
import Hickory.Vulkan.Forward.Lit (withStaticUnlitMaterial, withAnimatedLitMaterial, withLitRenderTarget, withStaticLitMaterial, withLineMaterial)
import Hickory.Vulkan.Forward.ShadowPass (withAnimatedShadowMaterial, withShadowRenderTarget, withStaticShadowMaterial)
import Hickory.Vulkan.RenderPass (withSwapchainRenderTarget, useRenderTarget)
import Hickory.Vulkan.Forward.ObjectPicking (withObjectIDRenderTarget, withObjectIDMaterial)
import Hickory.Vulkan.Mesh (BufferedMesh (..), vsizeOf, vertices, indices)
import Vulkan (ClearValue (..), ClearColorValue (..), cmdDraw, ClearDepthStencilValue (..), bindings, withDescriptorSetLayout)
import Foreign (castPtr, copyArray, withArrayLen, Storable)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet, BufferDescriptorSet (..), descriptorSetBindings, withDataBuffer, uploadBufferDescriptor)
import Control.Lens (view, has, preview, to, (^.), (.~), (&))
import Hickory.Vulkan.Framing (doubleResource, resourceForFrame, frameResource)
import Hickory.Vulkan.Material (cmdBindMaterial, cmdPushMaterialConstants, cmdBindDrawDescriptorSet)
import VulkanMemoryAllocator (withMappedMemory)
import Control.Exception (bracket)
import Data.List (partition)
import Control.Monad.State.Strict (execStateT)
import Data.Traversable (for)
import Data.Maybe (mapMaybe)
import Data.Foldable (for_)
import Hickory.Vulkan.DynamicMesh (DynamicBufferedMesh(..), withDynamicBufferedMesh)
import Data.Functor.Identity (runIdentity)
import qualified Data.Vector as V
import Vulkan.Zero (zero)

withRenderer :: VulkanResources -> Swapchain -> Acquire Renderer
withRenderer vulkanResources@VulkanResources {deviceContext = DeviceContext{..}} swapchain = do
  shadowRenderTarget       <- withShadowRenderTarget vulkanResources
  litRenderTarget          <- withLitRenderTarget vulkanResources swapchain
  swapchainRenderTarget    <- withSwapchainRenderTarget vulkanResources swapchain
  -- objectIDRenderTarget     <- withObjectIDRenderTarget vulkanResources swapchain

  globalWorldBuffer   <- withDataBuffer vulkanResources 1
  globalOverlayBuffer <- withDataBuffer vulkanResources 1

  globalWorldDescriptorSet <- withDescriptorSet vulkanResources
    [ BufferDescriptor (buf globalWorldBuffer)
    , descriptorSpec shadowRenderTarget
    ]
  globalOverlayDescriptorSet <- withDescriptorSet vulkanResources
    [ BufferDescriptor (buf globalOverlayBuffer)
    ]

  imageSetLayout <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"]]
    } Nothing mkAcquire

  -- objectIDMaterial         <- withObjectIDMaterial vulkanResources objectIDRenderTarget globalWorldDescriptorSet
  staticShadowMaterial     <- withStaticShadowMaterial vulkanResources shadowRenderTarget globalWorldDescriptorSet
  animatedShadowMaterial   <- withAnimatedShadowMaterial vulkanResources shadowRenderTarget globalWorldDescriptorSet
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

renderToRenderer :: (FrameMonad m, MonadIO m) => Renderer -> RenderSettings -> PostConstants -> Command () -> Command () -> m ()
renderToRenderer Renderer {..} RenderSettings {..} postConstants litF overlayF = do
  frameContext@FrameContext {..} <- askFrameContext
  useDynamicMesh (resourceForFrame frameNumber dynamicMesh) do
    uploadBufferDescriptor globalWorldBuffer worldGlobals
    uploadBufferDescriptor globalOverlayBuffer $ worldGlobals
                                               & #viewMat .~ (overlayGlobals ^. #viewMat)
                                               & #projMat .~ (overlayGlobals ^. #projMat)

    let
      recordCommandBuffer :: Command () -> [DrawCommand]
      recordCommandBuffer = runIdentity . flip execStateT mempty . unCommandT

    let drawCommands = recordCommandBuffer litF

    useRenderTarget shadowRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0) ] swapchainImageIndex do
      let shadowCommands = filter castsShadow drawCommands
      renderMaterialCommands frameContext staticShadowMaterial   $ selectStaticCommands shadowCommands
      renderMaterialCommands frameContext animatedShadowMaterial $ selectAnimatedCommands shadowCommands

    let V4 r g b a = clearColor
    useRenderTarget litRenderTarget commandBuffer
      [ Color (Float32 r g b a)
      , DepthStencil (ClearDepthStencilValue 1 0)
      , Color (Float32 1 1 1 1)
      ] swapchainImageIndex do
      let (blended, opaque) = partition blend drawCommands
          (litOpaque, unlitOpaque)      = partition lit opaque
          (litBlended, unlitBlended)    = partition lit blended

      renderMaterialCommands frameContext linesWorldMaterial       $ selectLinesCommands opaque
      renderMaterialCommands frameContext staticLitWorldMaterial   $ selectStaticCommands litOpaque
      renderMaterialCommands frameContext staticUnlitWorldMaterial $ selectStaticCommands unlitOpaque
      renderMaterialCommands frameContext animatedLitWorldMaterial $ selectAnimatedCommands litOpaque
      -- renderMaterialCommands frameContext animatedUnlitWorldMaterial $ selectAnimatedCommands unlitOpaque
      renderMaterialCommands frameContext msdfWorldMaterial        $ selectMSDFCommands opaque

      renderMaterialCommands frameContext linesWorldMaterial       $ selectLinesCommands blended
      renderMaterialCommands frameContext staticLitWorldMaterial   $ selectStaticCommands litBlended
      renderMaterialCommands frameContext staticUnlitWorldMaterial $ selectStaticCommands unlitBlended
      renderMaterialCommands frameContext animatedLitWorldMaterial $ selectAnimatedCommands blended
      -- renderMaterialCommands frameContext animatedUnlitWorldMaterial $ selectAnimatedCommands unlitBlended
      renderMaterialCommands frameContext msdfWorldMaterial        $ selectMSDFCommands blended

    useRenderTarget swapchainRenderTarget commandBuffer [] swapchainImageIndex do
      cmdBindMaterial frameNumber commandBuffer postProcessMaterial
      liftIO $ cmdPushMaterialConstants commandBuffer postProcessMaterial postConstants
      liftIO $ cmdDraw commandBuffer 3 1 0 0

      let overlayCommands = recordCommandBuffer overlayF
      renderMaterialCommands frameContext staticOverlayMaterial $ selectStaticCommands overlayCommands
      renderMaterialCommands frameContext msdfOverlayMaterial   $ selectMSDFCommands overlayCommands
  where
  selectAnimatedCommands :: [DrawCommand] -> [(DrawCommand, (AnimatedConstants, Maybe PointedDescriptorSet))]
  selectAnimatedCommands = mapMaybe (\c -> (c,) <$> preview (#drawType . #_Animated . to (mkUniform c)) c)
    where
    mkUniform DrawCommand {..} AnimatedMesh {..} = (, Just albedo)
      AnimatedConstants
        { modelMat  = modelMat
        , normalMat = transpose . inv33 $ modelMat ^. _m33
        , color = color
        , boneMat = boneMat
        , colors = colors
        }
  selectStaticCommands :: [DrawCommand] -> [(DrawCommand, (StaticConstants, Maybe PointedDescriptorSet))]
  selectStaticCommands = mapMaybe (\c -> (c,) <$> preview (#drawType . #_Static . to (mkUniform c)) c)
    where
    mkUniform DrawCommand {..} StaticMesh {..} = (, Just albedo)
      StaticConstants
        { modelMat  = modelMat
        , normalMat = transpose . inv33 $ modelMat ^. _m33
        , color = color
        , tiling = tiling
        }
  selectLinesCommands :: [DrawCommand] -> [(DrawCommand, (StaticConstants, Maybe PointedDescriptorSet))]
  selectLinesCommands = mapMaybe (\c -> if has (#drawType . #_Lines) c then Just (c, (mkUniform c, Nothing)) else Nothing)
    where
    mkUniform DrawCommand {..} =
      StaticConstants
        { modelMat  = modelMat
        , normalMat = transpose . inv33 $ modelMat ^. _m33
        , color = color
        , tiling = V2 1 1
        }
  selectMSDFCommands :: [DrawCommand] -> [(DrawCommand, (MSDFMatConstants, Maybe PointedDescriptorSet))]
  selectMSDFCommands = mapMaybe (\c -> (c,) <$> preview (#drawType . #_MSDF . to (mkUniform c)) c)
    where
    mkUniform DrawCommand {..} MSDFMesh {..} = (, Just tex)
      MSDFMatConstants
        { modelMat  = modelMat
        , outlineColor = outlineColor
        , outlineSize = outlineSize
        , sdfPixelRange = sdfPixelRange
        , tiling = tiling
        , color = color
        }

renderMaterialCommands :: (MonadIO m, DynamicMeshMonad m, Storable uniform) => FrameContext -> BufferedUniformMaterial uniform -> [(DrawCommand, (uniform, Maybe PointedDescriptorSet))] -> m ()
renderMaterialCommands _ _ [] = pure ()
renderMaterialCommands FrameContext {..} BufferedUniformMaterial {..} commands = do
  cmdBindMaterial frameNumber commandBuffer material

  uniforms <- for (zip commands [0..]) \((DrawCommand {..}, (uniform, drawDS)), i) -> do
    cmdPushMaterialConstants commandBuffer material i
    for_ drawDS $ cmdBindDrawDescriptorSet commandBuffer material
    case mesh of
      Buffered BufferedMesh {..} -> cmdDrawBufferedMesh commandBuffer material mesh 0 vertexBuffer 0 indexBuffer
      Dynamic dyn -> do
        meshes <- getMeshes
        addMesh dyn

        -- This is O(n)... Might want to cache this
        let vertexSizeThusFar = sum $ map (sum . map (vsizeOf . snd) . vertices) meshes
            indexSizeThusFar  = sum $ map (maybe 0 vsizeOf . indices) meshes

        DynamicBufferedMesh { vertexBufferPair = (vertexBuffer,_), indexBufferPair = (indexBuffer,_) } <- askDynamicMesh
        cmdDrawBufferedMesh commandBuffer material dyn vertexSizeThusFar vertexBuffer (fromIntegral indexSizeThusFar) (Just indexBuffer)

    pure uniform

  let BufferDescriptorSet { bufferPair, allocator } = resourceForFrame frameNumber descriptor

  let (_, alloc) = bufferPair

  liftIO $ withMappedMemory allocator alloc bracket \bptr ->
    withArrayLen uniforms \len dptr -> copyArray (castPtr bptr) dptr len
