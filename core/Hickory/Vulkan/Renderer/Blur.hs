{-# LANGUAGE OverloadedLists, OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Renderer.Blur where

import Hickory.Vulkan.Types (VulkanResources (..), RenderConfig (..), Material(..), PointedDescriptorSet(..))
import Vulkan (Extent2D (..), SampleCountFlagBits (..), Format (..), PipelineRenderingCreateInfo (..), CullModeFlagBits (..))
import Acquire (Acquire (..))
import Hickory.Vulkan.Material (withMaterial, defaultBlend, pipelineDefaults)
import Data.Word (Word32)
import Data.String.QM (qm)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.ByteString (ByteString)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Hickory.Vulkan.Framing (FramedResource)

data BlurConstants = BlurConstants
  { size   :: Word32
  , spread :: Float
  } deriving Generic
    deriving anyclass GStorable

data DepthOfFieldConstants = DepthOfFieldConstants
  { focalDist :: Float
  , dofMin    :: Float
  , dofMax    :: Float
  } deriving (Generic, Show, Read)
    deriving anyclass GStorable

withBlurRenderConfig :: Format -> Extent2D -> Acquire RenderConfig
withBlurRenderConfig format extent = do
  let samples = SAMPLE_COUNT_1_BIT
      renderPassInfo = Right PipelineRenderingCreateInfo
        { colorAttachmentFormats = [format]
        , depthAttachmentFormat = FORMAT_UNDEFINED
        , stencilAttachmentFormat = FORMAT_UNDEFINED
        , viewMask = 0
        }
  pure RenderConfig {..}

blurVertShader :: ByteString
blurVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header

layout (location = 0) out vec2 texCoords;

void main()
{
  texCoords = vec2(gl_VertexIndex & 2, (gl_VertexIndex << 1) & 2 );
  gl_Position = vec4(texCoords * 2.0f + -1.0f, 1.0f, 1.0f);
}
|])

blurFragShader :: ByteString
blurFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header

layout (location = 0) in vec2 texCoords;
layout (location = 0) out vec4 outColor;

layout (set = 0, binding = 0) uniform sampler2D tex;

layout( push_constant, scalar, row_major ) uniform constants
{ int size;
  float spread;
} PushConstants;

void main()
{
  int samplesWidth = PushConstants.size * 2 + 1;
  int totalSamples = samplesWidth * samplesWidth;
  float spread = max(PushConstants.spread, 1);
  vec2 texSize = textureSize(tex, 0).xy;
  vec4 color = vec4(0,0,0,0);

  for (int i = 0; i < samplesWidth; i++)
  {
    for (int j = 0; j < samplesWidth; j++)
    {
      vec2 offset = vec2(i - PushConstants.size, j - PushConstants.size) * spread / texSize;
      color += texture(tex, texCoords + offset);
    }
  }

  outColor = color / totalSamples;
}
|])


dofVertShader :: ByteString
dofVertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$worldGlobalsDef

layout (location = 0) out vec2 texCoords;
layout (location = 1) out vec3 viewRay;
layout (location = 2) out vec3 worldRay;

void main()
{
  texCoords = vec2(gl_VertexIndex & 2, (gl_VertexIndex << 1) & 2 );
  gl_Position = vec4(texCoords * 2.0f + -1.0f, 1.0f, 1.0f);

  vec4 view = globals.invProjMat * vec4(gl_Position.x,gl_Position.y,1,1);
  viewRay = view.xyz / view.w; // From camera toward fragment
  worldRay = mat3(globals.invViewMat) * viewRay;
}
|])

dofFragShader :: ByteString
dofFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef

layout (location = 0) in vec2 texCoords;
layout (location = 1) in vec3 viewRay;
layout (location = 2) in vec3 worldRay;

layout (location = 0) out vec4 outColor;

layout (set = 1, binding = 0) uniform sampler2D origTex;
layout (set = 1, binding = 1) uniform sampler2D blurredTex;
layout (set = 1, binding = 2) uniform sampler2D depthTex;

layout( push_constant, scalar, row_major ) uniform constants
{ float focalDist;
  float dofMin;
  float dofMax;
} PushConstants;

float linearizeDepth(float depth, float nearPlane, float farPlane)
{
    return nearPlane * farPlane / (farPlane - depth * (farPlane - nearPlane));
}

void main()
{
  vec3 orig = texture(origTex, texCoords).rgb;
  vec3 color = vec3(0,0,0);
  if (PushConstants.dofMax > 0) {
    float depth = texture(depthTex, texCoords).r;
    depth = linearizeDepth(depth, globals.nearPlane, globals.farPlane);

    vec3 viewPos  = viewRay * (depth / globals.farPlane);

    vec3 blurred = texture(blurredTex, texCoords).rgb;

    float amt = smoothstep(PushConstants.dofMin, PushConstants.dofMax, abs(PushConstants.focalDist - viewPos.z));
    color = mix(orig, blurred, amt);
  } else {
    color = orig;
  }
  outColor = vec4(color, 1);
}
|])

withBlurMaterial :: VulkanResources -> Format -> Extent2D -> Vector PointedDescriptorSet -> Acquire (Material BlurConstants)
withBlurMaterial vulkanResources format extent source = do
  renderConfig <- withBlurRenderConfig format extent
  material <- withMaterial vulkanResources renderConfig
    [] (pipelineDefaults [defaultBlend]) CULL_MODE_BACK_BIT blurVertShader blurFragShader [source] Nothing
  pure material


withDepthOfFieldRenderConfig :: Format -> Extent2D -> Acquire RenderConfig
withDepthOfFieldRenderConfig = withBlurRenderConfig

withDepthOfFieldMaterial
  :: VulkanResources
  -> Format
  -> Extent2D
  -> FramedResource PointedDescriptorSet
  -> FramedResource PointedDescriptorSet
  -> Acquire (Material DepthOfFieldConstants)
withDepthOfFieldMaterial vulkanResources format extent globalSet matSet = do
  renderConfig <- withDepthOfFieldRenderConfig format extent
  material <- withMaterial vulkanResources renderConfig
    [] (pipelineDefaults [defaultBlend]) CULL_MODE_BACK_BIT dofVertShader dofFragShader [globalSet, matSet] Nothing
  pure material
