{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.PostProcessing where

import Acquire.Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Material (withMaterial, pipelineDefaults, defaultBlend)
import Hickory.Vulkan.Framing (FramedResource)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Hickory.Vulkan.Types
import Data.String.QM (qm)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Vulkan (CullModeFlagBits(..))
import Hickory.Vulkan.Renderer.Types (PostConstants)

withPostProcessMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> FramedResource PointedDescriptorSet -> Acquire (Material PostConstants)
withPostProcessMaterial vulkanResources renderConfig globalDescriptorSet materialDescriptorSet =
  withMaterial vulkanResources renderConfig
    [] (pipelineDefaults [defaultBlend]) CULL_MODE_BACK_BIT vertShader fragShader [globalDescriptorSet, materialDescriptorSet] Nothing
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
  fragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header
$worldGlobalsDef

layout (location = 0) in vec2 texCoordsVarying;
layout (location = 0) out vec4 outColor;

layout (row_major, scalar, set = 0, binding = 0) uniform PostGlobals
  { int frameNumber;
  } postGlobals;

layout( push_constant, scalar ) uniform constants
{
  float exposure;
  vec3 colorShift;
  float saturation;
  float filmGrain;
  float shadowBiasSlope;
} PushConstants;

layout (set = 1, binding = 0) uniform sampler2D textures[2];

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

vec3 filmic_tonemapping(vec3 x) {
    const float A = 0.22; // shoulder strength
    const float B = 0.30; // linear strength
    const float C = 0.10; // linear angle
    const float D = 0.20; // toe strength
    const float E = 0.01; // toe numerator
    const float F = 0.30; // toe denominator
    const float W = 11.2; // white point

    vec3 num = (x * (A * x + B * C)) + D * E;
    vec3 den = (x * (A * x + B)) + D * F;
    return clamp(num / den - E / F, 0.0, 1.0);
}

void main()
{
  lowp vec4 origColor = texture(textures[0], texCoordsVarying);

  // Exposure
  vec3 exposureFilter = exp2(PushConstants.exposure) * PushConstants.colorShift;
  vec3 color = origColor.rgb * exposureFilter;

  // Saturation
  vec3 lumaWeights = vec3(0.25,0.50,0.25);
  float luminance = dot(lumaWeights, color.rgb);
  vec3 grey = vec3(luminance, luminance, luminance);
  vec3 saturated = grey + PushConstants.saturation * (color.rgb - grey);

  // Tonemapping
  //color = aces_tonemapping(saturated);
  color = filmic_tonemapping(saturated);

  // Film grain
  float grainIntensity =
    fract( 10000
         * sin( (3.14 / 180)
              * ( texCoordsVarying.x * 360
                + texCoordsVarying.y * 36
                * postGlobals.frameNumber
                )
              )
         );

  color += PushConstants.filmGrain * grainIntensity;

  outColor = vec4(color, 1.0);
}
|])
