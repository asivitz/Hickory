{-# LANGUAGE QuasiQuotes #-}

module Hickory.Vulkan.Forward.ShaderDefinitions where

import Data.String.QM (qt)

worldGlobalsDef :: String
worldGlobalsDef = [qt|
layout (row_major, scalar, set = 0, binding = 1) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    vec3 cameraPos;
    mat4 lightTransform;
    vec3 lightDirection;
    vec3 sunColor;
    vec3 ambientColor;
    float multiSampleCount;
    float nearPlane;
    float farPlane;
  } globals;
  |]

overlayGlobalsDef :: String
overlayGlobalsDef = [qt|
layout (row_major, scalar, set = 0, binding = 2) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
  } globals;
  |]

shadowPassGlobalsDef :: String
shadowPassGlobalsDef = [qt|
layout (row_major, scalar, set = 0, binding = 3) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
  } globals;
  |]

header :: String
header = [qt|
#version 450
#extension GL_EXT_scalar_block_layout : require
  |]

staticUniformsDef :: String
staticUniformsDef = [qt|
struct Uniforms
{
  mat4 modelMat;
  mat3 normalMat;
  vec4 color;
  float specularity;
  vec2 tiling;
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
  |]

animatedUniformsDef :: String
animatedUniformsDef = [qt|
struct Uniforms {
  mat4 modelMat;
  mat3 normalMat;
  vec4 color;
  float specularity;
  mat4 boneMat[66];
  vec4 colors[6];
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
  |]

msdfUniformsDef :: String
msdfUniformsDef = [qt|
struct Uniforms
{
  mat4 modelMat;
  vec4 color;
  vec4 outlineColor;
  float outlineSize;
  float sdfPixelRange;
  vec2 tiling;
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
  |]
