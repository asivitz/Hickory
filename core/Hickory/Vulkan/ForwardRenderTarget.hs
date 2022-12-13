{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.ForwardRenderTarget where

import Hickory.Vulkan.Vulkan (mkAcquire, ViewableImage(..), Swapchain (..), VulkanResources (..), DeviceContext (..), withDepthImage, with2DImageView)
import Vulkan
  ( Device
  , Extent2D (..)
  , Format (..)
  , withRenderPass
  , SampleCountFlagBits (..)
  , AttachmentDescription(..)
  , SubpassDescription(..)
  , SubpassDependency(..)
  , RenderPassCreateInfo(..)
  , AttachmentReference(..)
  , AttachmentLoadOp (..)
  , AttachmentStoreOp (..)
  , ImageLayout (..)
  , PipelineBindPoint (..)
  , pattern SUBPASS_EXTERNAL
  , PipelineStageFlagBits (..)
  , AccessFlagBits (..)
  , FramebufferCreateInfo(..)
  , ImageView
  , Framebuffer
  , withFramebuffer
  , SurfaceFormatKHR, ImageAspectFlagBits (..)
  , ImageUsageFlagBits(..)
  , ClearValue(..)
  , ClearColorValue(..)
  , cmdDraw, PrimitiveTopology(..), Rect2D (..), RenderPassBeginInfo(..)
  , cmdUseRenderPass
  , ClearDepthStencilValue(..)
  , pattern SUBPASS_CONTENTS_INLINE
  , cmdNextSubpass, RenderPass, Filter (..), SamplerAddressMode (..), Sampler, cmdBindDescriptorSets, cmdBindPipeline, BufferUsageFlagBits (..), MemoryPropertyFlagBits (..), Buffer, CullModeFlagBits (..)
  )
import Acquire.Acquire (Acquire)
import qualified Data.Vector as V
import Data.Generics.Labels ()
import Data.Bits ((.|.))
import Hickory.Vulkan.Monad (FrameMonad (askFrameContext), CommandT, recordCommandBuffer, shadowMap, meshOptions, DrawCommand (..), blend)
import Hickory.Vulkan.Material (withMaterial, cmdBindMaterial, cmdPushMaterialConstants, pipelineDefaults)
import Hickory.Vulkan.Framing (FramedResource(..), doubleResource)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hickory.Math (Scalar, Mat44)
import Hickory.Vulkan.Frame (FrameContext(..))
import Data.Proxy (Proxy)
import Hickory.Vulkan.Types
import Linear.V3 (V3)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Control.Lens (view)
import Hickory.Vulkan.Mesh (withBuffer')
import Foreign (sizeOf, poke)
import VulkanMemoryAllocator (withMappedMemory)
import Control.Exception (bracket)
import Foreign.Ptr (castPtr)
import Data.List (partition, sortOn)
import Control.Monad (when)
import Data.Foldable (for_)
import Linear ( V4(..), V3(..), identity, M44)
import Data.Maybe
-- import Hickory.Vulkan.RenderPass (withShadowRenderTarget, useRenderTarget, withLitRenderTarget, withSwapchainRenderTarget)

{-
withForwardRenderTarget :: VulkanResources -> Swapchain -> [DescriptorSpec] -> Acquire ForwardRenderTarget
withForwardRenderTarget vulkanResources@VulkanResources {allocator} swapchain extraGlobalDescriptors = do
  shadowRenderTarget    <- withShadowRenderTarget vulkanResources
  litRenderTarget       <- withLitRenderTarget vulkanResources swapchain
  swapchainRenderTarget <- withSwapchainRenderTarget vulkanResources swapchain

  (buffer, alloc, _) <- withBuffer' allocator
    BUFFER_USAGE_UNIFORM_BUFFER_BIT
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    (fromIntegral $ sizeOf (undefined :: Globals)) -- There's got to be a better way than hardcoding # of uniforms allowed
  let globalsBuffer = (buffer, alloc, allocator)

  globalDescriptorSet <- withDescriptorSet vulkanResources
    $ descriptorSpec shadowRenderTarget
    : BufferDescriptor buffer
    : extraGlobalDescriptors

  materialDescriptorSet <- withDescriptorSet vulkanResources
    [ view #descriptorSpec litRenderTarget
    ]

  postProcessMaterial <- withPostProcessMaterial vulkanResources [shadowRenderTarget, litRenderTarget, swapchainRenderTarget] (doubleResource globalDescriptorSet) (doubleResource materialDescriptorSet)

  pure ForwardRenderTarget {..}

renderToForwardTarget :: (FrameMonad m, MonadIO m) => ForwardRenderTarget -> V4 Scalar -> Globals -> PostConstants -> CommandT m () -> CommandT m () -> m ()
renderToForwardTarget ForwardRenderTarget {..} (V4 r g b a) globals postConstants litF overlayF = do
  FrameContext {..} <- askFrameContext

  let (_,alloc,allocator) = globalsBuffer
  liftIO do
    withMappedMemory allocator alloc bracket \bufptr ->
      poke (castPtr bufptr) globals

  drawCommands <- recordCommandBuffer litF
  let (shadowCommands, litCommands) = partition (shadowMap . meshOptions) drawCommands

  useRenderTarget shadowRenderTarget commandBuffer [ DepthStencil (ClearDepthStencilValue 1 0) ] swapchainImageIndex do
    liftIO $ renderCommands commandBuffer frameNumber (selectPipeline 0) shadowCommands

  useRenderTarget litRenderTarget commandBuffer
    [ Color (Float32 r g b a)
    , DepthStencil (ClearDepthStencilValue 1 0)
    , Color (Float32 1 1 1 1)
    ] swapchainImageIndex do
      liftIO $ sortBlendedAndRenderCommands commandBuffer frameNumber (selectPipeline 1) litCommands

  useRenderTarget swapchainRenderTarget commandBuffer [] swapchainImageIndex do
    cmdBindMaterial frameNumber (selectPipeline 2) commandBuffer postProcessMaterial
    cmdPushMaterialConstants commandBuffer postProcessMaterial postConstants
    cmdDraw commandBuffer 3 1 0 0

    recordCommandBuffer overlayF >>= liftIO . sortBlendedAndRenderCommands commandBuffer frameNumber (selectPipeline 2)

  where
  selectPipeline i = fromMaybe (error $ "Error accessing pipeline: " ++ show i) . (V.!? i) . pipelines
  renderCommands commandBuffer frameNumber selector commands =
    for_ commands \DrawCommand{..} -> do
      cmdBindMaterial frameNumber selector commandBuffer material
      io commandBuffer
  sortBlendedAndRenderCommands commandBuffer frameNumber selector commands = do
    forState_ opaque Nothing \curMatId DrawCommand{..} -> do
      let newMatId = view #uuid material
      when (Just newMatId /= curMatId) do
        cmdBindMaterial frameNumber selector commandBuffer material
      io commandBuffer
      pure (Just newMatId)
    renderCommands commandBuffer frameNumber selector (reverse blended)
    where
    (blended, sortOn (view #uuid . material) . reverse -> opaque) = partition (blend . meshOptions) commands
    -}

withPostProcessMaterial :: VulkanResources -> RenderTarget -> FramedResource PointedDescriptorSet -> FramedResource PointedDescriptorSet -> Acquire (Material PostConstants)
withPostProcessMaterial vulkanResources renderTarget globalDescriptorSet materialDescriptorSet =
  withMaterial vulkanResources renderTarget (undefined :: Proxy PostConstants)
    [] pipelineDefaults vertShader fragShader [globalDescriptorSet, materialDescriptorSet] Nothing
  where
  vertShader = [vert|
#version 450

layout (location = 0) out vec2 texCoordsVarying;

void main()
{
    texCoordsVarying = vec2(gl_VertexIndex & 2, (gl_VertexIndex << 1) & 2 );
    gl_Position = vec4(texCoordsVarying * 2.0f + -1.0f, 1.0f, 1.0f);
}

|]
  fragShader = [frag|
#version 450
#extension GL_EXT_scalar_block_layout : require

layout (location = 0) in vec2 texCoordsVarying;
layout (location = 0) out vec4 outColor;

layout( push_constant, scalar ) uniform constants
{
  float exposure;
  vec3 colorShift;
  float saturation;
  float filmGrain;
  int frameNumber;
} PushConstants;

layout (set = 1, binding = 0) uniform sampler2D textureSampler;

// Bring hdr color into ldr range with an artistic curve
// Narkowicz 2015, "ACES Filmic Tone Mapping Curve"

vec3 aces_tonemapping(vec3 x) {
  const float a = 2.51;
  const float b = 0.03;
  const float c = 2.43;
  const float d = 0.59;
  const float e = 0.14;
  return clamp((x * (a * x + b)) / (x * (c * x + d) + e), 0.0, 1.0);
}

void main()
{
  lowp vec4 origColor = texture(textureSampler, texCoordsVarying);

  // Exposure
  vec3 exposureFilter = exp2(PushConstants.exposure) * PushConstants.colorShift;
  vec3 color = origColor.rgb * exposureFilter;

  // Saturation
  vec3 lumaWeights = vec3(0.25,0.50,0.25);
  float luminance = dot(lumaWeights, color.rgb);
  vec3 grey = vec3(luminance, luminance, luminance);
  vec3 saturated = grey + PushConstants.saturation * (color.rgb - grey);

  // Tonemapping
  color = aces_tonemapping(saturated);

  // Film grain
  float grainIntensity =
    fract( 10000
         * sin( (3.14 / 180)
              * ( texCoordsVarying.x * 360
                + texCoordsVarying.y * 36
                * PushConstants.frameNumber
                )
              )
         );

  color += PushConstants.filmGrain * grainIntensity;

  outColor = vec4(color, 1.0);
}
|]

forState_ :: Monad m => [t] -> a -> (a -> t -> m a) -> m ()
forState_ (x:xs) initialVal f = f initialVal x >>= flip (forState_ xs) f
forState_ [] _ _ = pure ()
