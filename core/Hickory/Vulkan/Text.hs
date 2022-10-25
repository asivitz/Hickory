{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Hickory.Vulkan.Text where

import Vulkan (RenderPass)
import Hickory.Vulkan.Vulkan (VulkanResources (..), Swapchain)
import Hickory.Vulkan.Mesh (Attribute (..))
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.DescriptorSet (PointedDescriptorSet)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Linear (M44)
import Linear.V4 (V4)
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)

data MSDFMatConstants = MSDFMatConstants
  { modelViewMat  :: M44 Float
  , color         :: V4 Float
  , outlineColor  :: V4 Float
  , outlineSize   :: Float -- In pixels
  , sdfPixelRange :: Float -- Should match parameter used to generate MSDF
  } deriving Generic
    deriving anyclass GStorable

withMSDFMaterial :: Bool -> VulkanResources -> Swapchain -> RenderPass -> PointedDescriptorSet -> Acquire (BufferedUniformMaterial MSDFMatConstants)
withMSDFMaterial lit vulkanResources swapchain renderPass pds = withBufferedUniformMaterial vulkanResources swapchain renderPass lit [Position, TextureCoord] vertShader fragShader Nothing (Just pds)
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
  };

  layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
  layout (row_major, scalar, set = 0, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;

  void main() {
      Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
      gl_Position = uniforms.modelViewProjMatrix * vec4(inPosition, 1.0);
      texCoord = inTexCoord;
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
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 0, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
layout (set = 1, binding = 0) uniform sampler2D texSampler;


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
  float screenPixelDistance = range*(distance - 0.5);

  float alpha = clamp( screenPixelDistance + 1 + uniforms.outlineSize
                      , 0
                      , 2
                      ) / 2;

  vec3 rgb = mix( uniforms.outlineColor.rgb
                , uniforms.color.rgb
                , clamp( screenPixelDistance + 1
                        , 0
                        , 2
                        ) / 2
                );

  outColor = vec4(rgb, alpha);
}

|]
