{-# LANGUAGE QuasiQuotes #-}

module Hickory.Vulkan.Forward.ShaderDefinitions where

import Data.String.QM (qt)

globalsDef :: String
globalsDef = [qt|
layout (row_major, scalar, set = 0, binding = 1) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    vec3 cameraPos;
    mat4 lightTransform;
    vec3 lightDirection;
    vec3 sunColor;
    vec3 ambientColor;
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
  mat4 boneMat[32];
  vec4 colors[6];
};

layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
  |]

