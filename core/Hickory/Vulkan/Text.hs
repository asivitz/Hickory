{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Hickory.Vulkan.Text where

import Vulkan (RenderPass)
import Hickory.Vulkan.Vulkan (VulkanResources (..), Swapchain)
import Hickory.Vulkan.Mesh (Attribute (..))
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Linear (M44, V2)
import Linear.V4 (V4)
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import Hickory.Vulkan.Types (PointedDescriptorSet, RenderTarget)

data MSDFMatConstants = MSDFMatConstants
  { modelViewMat  :: M44 Float
  , color         :: V4 Float
  , outlineColor  :: V4 Float
  , outlineSize   :: Float -- In pixels
  , sdfPixelRange :: Float -- Should match parameter used to generate MSDF
  , tiling        :: V2 Float
  } deriving Generic
    deriving anyclass GStorable

withMSDFMaterial :: VulkanResources -> Swapchain -> RenderTarget -> PointedDescriptorSet -> Acquire (BufferedUniformMaterial MSDFMatConstants)
withMSDFMaterial vulkanResources swapchain renderTarget pds = withBufferedUniformMaterial vulkanResources swapchain renderTarget [Position, TextureCoord] vertShader fragShader (Just pds)
  where
  vertShader :: ByteString
  vertShader = [vert|
  #version 450
  #extension GL_EXT_scalar_block_layout : require

  layout(location = 0) in vec3 inPosition;
  layout(location = 3) in vec2 inTexCoord;

  layout(location = 1) out vec2 texCoord;

  struct Uniforms
  {
    mat4 modelViewProjMatrix;
    vec4 color;
    vec4 outlineColor;
    float outlineSize;
    float sdfPixelRange;
    vec2 tiling;
  };

  layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
  layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;

  void main() {
      Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
      gl_Position = uniforms.modelViewProjMatrix * vec4(inPosition, 1.0);
      texCoord = uniforms.tiling * inTexCoord;
  }

|]

  fragShader :: ByteString
  fragShader = [frag|
#version 450
#extension GL_EXT_scalar_block_layout : require

layout(location = 1) in vec2 texCoord;

layout(location = 0) out vec4 outColor;

struct Uniforms
{
  mat4 modelViewProjMatrix;
  vec4 color;
  vec4 outlineColor;
  float outlineSize;
  float sdfPixelRange;
  vec2 tiling;
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
layout (set = 2, binding = 0) uniform sampler2D texSampler;


float median(vec3 v) {
  return max(min(v.r, v.g), min(max(v.r, v.g), v.b));
}

// From https://github.com/Chlumsky/msdfgen
float screenPixelRange(float sdfPixelRange) {
  vec2 unitRange = vec2(sdfPixelRange)/vec2(textureSize(texSampler, 0));
  vec2 screenTexSize = vec2(1.0)/fwidth(texCoord);
  return max(0.5*dot(unitRange, screenTexSize), 1.0);
}

void main() {
  Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];

  float distance = median(texture(texSampler, texCoord).rgb);

  float range = screenPixelRange(uniforms.sdfPixelRange);
  float screenPixelDistance = range * (distance - 0.5);

  if (uniforms.outlineSize > 0)
  {
    vec4 background = vec4(uniforms.outlineColor.rgb, 0);

    vec4 outerColor = mix( background
                         , uniforms.outlineColor
                         , clamp( screenPixelDistance + 1 + uniforms.outlineSize
                           , 0
                           , 2
                           ) / 2);
    outColor = mix( outerColor
                  , uniforms.color
                  , clamp( screenPixelDistance + 1
                         , 0
                         , 2
                         ) / 2);
  }
  else
  {
    vec4 background = vec4(uniforms.color.rgb, 0);
    outColor = mix( background
                  , uniforms.color
                  , clamp( screenPixelDistance + 1
                         , 0
                         , 2
                         ) / 2);
  }
}

|]
