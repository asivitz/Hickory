{-# LANGUAGE PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, DerivingStrategies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, OverloadedLists, OverloadedLabels #-}

module Hickory.Vulkan.PostProcessing where

import Acquire (Acquire)
import Data.Generics.Labels ()
import Hickory.Vulkan.Material (withMaterial, pipelineDefaults, defaultBlend)
import Hickory.Vulkan.Framing (FramedResource)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Hickory.Vulkan.Types
import Data.String.QM (qm)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Vulkan (CullModeFlagBits(..), bindings, withDescriptorSetLayout)
import Hickory.Vulkan.Renderer.Types (PostConstants)
import Vulkan.Zero (Zero(..))
import qualified Data.Vector as V
import Hickory.Vulkan.DescriptorSet (descriptorSetBindings)
import Hickory.Vulkan.Vulkan (mkAcquire)

withPostProcessMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> FramedResource PointedDescriptorSet -> Acquire (Material PostConstants)
withPostProcessMaterial vulkanResources renderConfig globalDescriptorSet materialDescriptorSet = do
  postPDS <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ descriptorSetBindings [ImageDescriptor [error "Dummy image"]]
    } Nothing mkAcquire
  withMaterial vulkanResources "PostProcessing" renderConfig
    [] (pipelineDefaults [defaultBlend]) CULL_MODE_BACK_BIT vertShader fragShader [globalDescriptorSet, materialDescriptorSet] (Just postPDS)
  where
  VulkanResources {..} = vulkanResources
  DeviceContext {..} = deviceContext
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
  bool falseColor;
} PushConstants;

layout (set = 1, binding = 0) uniform sampler2D textures[2];
layout (set = 2, binding = 0) uniform sampler3D lut;

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

float luminance(vec3 color) {
  return dot(color, vec3(0.2126, 0.7152, 0.0722));  // Rec. 709
}

vec3 falseColor(float lum) {
  if (lum < 0.0005) return vec3(0.0); // Clipping
  else if (lum < 0.005) return vec3(0.0, 0.0, 1.0);
  else if (lum < 0.05) return vec3(0.0, 0.5, 1.0);
  else if (lum < 0.16) return vec3(0.0, 1.0, 1.0);
  else if (lum < 0.22) return vec3(0.0, 1.0, 0.5);
  else if (lum < 0.35) return vec3(0.5, 0.5, 0.5);
  else if (lum < 0.55) return vec3(0.68, 1.0, 0.18);
  else if (lum < 0.80) return vec3(1.0, 1.0, 0.0);
  else if (lum < 0.97) return vec3(1.0, 0.5, 0.0);
  else                 return vec3(1.0, 0.0, 0.0);
}

void main()
{
  lowp vec4 origColor = texture(textures[0], texCoordsVarying);

  vec3 color = origColor.rgb * exp2(PushConstants.exposure);

  //color = aces_tonemapping(saturated);
  color = filmic_tonemapping(color);
  color = texture(lut, color).rgb;

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

  if (PushConstants.falseColor) {
    color = falseColor(luminance(color));
  }

  outColor = vec4(color, 1.0);
}
|])
