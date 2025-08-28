{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE DataKinds, OverloadedLists, OverloadedRecordDot #-}

module Hickory.Vulkan.Renderer.Direct where

import Vulkan
  ( Format (..)
  , SampleCountFlagBits (..)
  , SurfaceFormatKHR(..)
  , PrimitiveTopology (..), PipelineRenderingCreateInfo (..)
  )
import Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Types
import Hickory.Vulkan.Material (PipelineOptions (..), defaultBlend, pipelineDefaults)
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.String.QM (qm)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Hickory.Vulkan.Renderer.GBuffer (depthFormat)
import Hickory.Vulkan.Framing (FramedResource)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import Data.Word (Word32)
import Hickory.Vulkan.Renderer.Types (StaticConstants)

hdrFormat :: Format
hdrFormat = FORMAT_R16G16B16A16_SFLOAT

-- withDirectFrameBuffer :: VulkanResources -> RenderConfig -> ViewableImage -> ViewableImage -> Acquire (Framebuffer, DescriptorSpec)
-- withDirectFrameBuffer vulkanResources@VulkanResources { deviceContext = DeviceContext{..} } rc@RenderConfig {..} colorImage depthImage = do
--   sampler <- withImageSampler vulkanResources FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE SAMPLER_MIPMAP_MODE_LINEAR
--
--   let ViewableImage _ colorImageView _ = colorImage
--       ViewableImage _ depthImageView _ = depthImage
--   let descriptorSpec = ImageDescriptor
--         [ (colorImage,sampler)
--         , (depthImage,sampler)
--         ]
--   (,descriptorSpec) <$> createFramebuffer device (renderConfigRenderPass rc) extent [colorImageView, depthImageView]

withDirectRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withDirectRenderConfig _vulkanResources Swapchain {..} = do
  let samples = SAMPLE_COUNT_1_BIT
  pure RenderConfig {..}
  where
  renderPassInfo = Right PipelineRenderingCreateInfo
    { colorAttachmentFormats = [imageFormat.format]
    , depthAttachmentFormat = depthFormat
    , stencilAttachmentFormat = FORMAT_UNDEFINED
    , viewMask = 0
    }

withOverlayRenderConfig :: VulkanResources -> Swapchain -> Acquire RenderConfig
withOverlayRenderConfig _vulkanResources Swapchain {..} = do
  let samples = SAMPLE_COUNT_1_BIT
  pure RenderConfig {..}
  where
  renderPassInfo = Right PipelineRenderingCreateInfo
    { colorAttachmentFormats = [imageFormat.format]
    , depthAttachmentFormat = FORMAT_UNDEFINED
    , stencilAttachmentFormat = FORMAT_UNDEFINED
    , viewMask = 0
    }

staticDirectVertShader :: String
staticDirectVertShader = [qm|
$pushConstantsDef
$staticUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 3) in vec2 inTexCoord;

layout(location = 0) out vec2 texCoord;
layout(location = 2) out vec4 color;

void main() {
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);

  gl_Position = globals.viewProjMat
              * worldPosition;

  texCoord = inTexCoord;
  color = uniforms.color;
}

|]

staticDirectFragShader :: ByteString
staticDirectFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef
$pushConstantsDef

layout(location = 0) in vec2 inTexCoord;
layout(location = 2) in vec4 inColor;
layout (set = 2, binding = 0) uniform sampler2D texSampler;

layout(location = 0) out vec4 outColor;

void main() {
  vec4 texColor = texture(texSampler, inTexCoord);
  outColor   = vec4(texColor * inColor);
}
|])

lineVertShader :: String
lineVertShader = [qm|
$pushConstantsDef
$staticUniformsDef


layout(location = 0) in vec3 inPosition;
layout(location = 2) out vec4 color;

void main() {
    color = uniforms.color;
    gl_Position = globals.viewProjMat
                * uniforms.modelMat
                * vec4(inPosition, 1.0);
}
|]

pointVertShader :: String
pointVertShader = [qm|
$pushConstantsDef
$staticUniformsDef


layout(location = 0) in vec3 inPosition;
layout(location = 2) out vec4 color;

void main() {
    color = uniforms.color;
    gl_Position = globals.viewProjMat
                * uniforms.modelMat
                * vec4(inPosition, 1.0);
    gl_PointSize = 20;
}
|]


simpleFragShader :: ByteString
simpleFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$fragHeader
$pushConstantsDef
layout(location = 2) in vec4 inColor;

void main() {
  outColor = inColor;
}

|])

withPointMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> Acquire (BufferedUniformMaterial Word32 StaticConstants)
withPointMaterial vulkanResources renderConfig globalPDS = withBufferedUniformMaterial vulkanResources "Point" renderConfig [Position] pipelineOptions vertShader simpleFragShader globalPDS Nothing
  where
  pipelineOptions = (pipelineDefaults [defaultBlend]) { primitiveTopology = PRIMITIVE_TOPOLOGY_POINT_LIST, depthTestEnable = False }
  vertShader :: ByteString
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
  $header
  $worldGlobalsDef
  $pushConstantsDef
  $staticUniformsDef

  layout(location = 0) in vec3 inPosition;

  void main() {
      gl_PointSize = 20;
      gl_Position = globals.viewProjMat
                  * uniforms.modelMat
                  * vec4(inPosition, 1.0);
  }

  |])
