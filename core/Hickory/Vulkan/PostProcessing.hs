{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.PostProcessing where

import Acquire.Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Material (withMaterial, pipelineDefaults)
import Hickory.Vulkan.Framing (FramedResource(..))
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Data.Proxy (Proxy)
import Hickory.Vulkan.Types

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

layout (row_major, scalar, set = 0, binding = 0) uniform Globals
  { int frameNumber;
  } globals;

layout( push_constant, scalar ) uniform constants
{
  float exposure;
  vec3 colorShift;
  float saturation;
  float filmGrain;
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
                * globals.frameNumber
                )
              )
         );

  color += PushConstants.filmGrain * grainIntensity;

  outColor = vec4(color, 1.0);
}
|]
