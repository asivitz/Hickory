{-# LANGUAGE QuasiQuotes #-}

module Hickory.Vulkan.Forward.ShaderDefinitions where

import Data.String.QM (qm, qt)

worldGlobalsDef :: String
worldGlobalsDef = [qm|
layout (row_major, scalar, set = 0, binding = 1) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    mat4 viewProjMat;
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
overlayGlobalsDef = [qm|
layout (row_major, scalar, set = 0, binding = 2) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    mat4 viewProjMat;
  } globals;
  |]

shadowPassGlobalsDef :: String
shadowPassGlobalsDef = [qm|
layout (row_major, scalar, set = 0, binding = 3) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    mat4 viewProjMat;
  } globals;
  |]

header :: String
header = [qm|
#version 450
#extension GL_EXT_scalar_block_layout : require
  |]

vertHeader :: String
vertHeader = [qm|
$header
layout(location = 0) in vec3 inPosition;
  |]

fragHeader :: String
fragHeader = [qm|
$header
layout(location = 0) out vec4 outColor;
  |]

staticUniformsDef :: String
staticUniformsDef = [qm|
struct Uniforms
{
  mat4 modelMat;
  mat3 normalMat;
  vec4 color;
  float specularity;
  vec2 tiling;
  uint objectID;
};

$uniformDef
  |]

animatedUniformsDef :: String
animatedUniformsDef = [qm|
struct Uniforms {
  mat4 modelMat;
  mat3 normalMat;
  vec4 color;
  float specularity;
  mat4 boneMat[66];
  vec4 colors[6];
  uint objectID;
};

$uniformDef
  |]

msdfUniformsDef :: String
msdfUniformsDef = [qm|
struct Uniforms
{
  mat4 modelMat;
  vec4 color;
  vec4 outlineColor;
  float outlineSize;
  float sdfPixelRange;
  vec2 tiling;
};

$uniformDef
  |]

objectIDUniformsDef :: String
objectIDUniformsDef = [qm|
struct Uniforms
{
  mat4 modelMat;
  uint objectID;
};

$uniformDef
  |]

uniformDef :: String
uniformDef = [qm|
layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
  |]


litVertexCalc :: String
litVertexCalc = [qt|
vec3 lightDirection = normalize(globals.lightDirection);
vec3 directionToLight = -lightDirection;
vec3 viewDirection = normalize(globals.cameraPos - worldPosition.xyz);

float diffuseIntensity = max(0.0, dot(worldNormal, directionToLight));


vec3 halfAngle = normalize(directionToLight + viewDirection);
float specularIntensity = pow(max(0.0, dot(worldNormal, halfAngle)), uniforms.specularity);

light = (vec3(diffuseIntensity) + vec3(specularIntensity)) * globals.sunColor;

shadowCoord = biasMat * globals.lightTransform * worldPosition;
  |]

litVertexDef :: String
litVertexDef = [qt|
layout(location = 0) out vec3 light;
layout(location = 2) out vec4 shadowCoord;

const mat4 biasMat = mat4(
  0.5, 0.0, 0.0, 0.0,
  0.0, 0.5, 0.0, 0.0,
  0.0, 0.0, 1.0, 0.0,
  0.5, 0.5, 0.0, 1.0 );
  |]

litFragDef :: String
litFragDef = [qt|
layout(location = 0) in vec3 light;
layout(location = 2) in vec4 shadowCoord;
layout (set = 0, binding = 4) uniform sampler2DShadow shadowMap;
  |]

litFragCalc :: String
litFragCalc = [qt|
  float shadow = 0.0;
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2(-1,  1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 1,  1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2(-1, -1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 1, -1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 0, 0));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 1, 0));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( -1, 0));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 0, 1));
  shadow += textureProjOffset(shadowMap, shadowCoord, ivec2( 0, -1));
  shadow = shadow / 9.0;

  vec4 litColor = vec4((shadow * light + globals.ambientColor) * surfaceColor.rgb, surfaceColor.a);
  |]

staticVertCalc :: String
staticVertCalc = [qt|
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);

  gl_Position = globals.viewProjMat
              * worldPosition;
|]

buildWorldVertShader :: String -> String
buildWorldVertShader vertShader = [qm|
$vertHeader
$worldGlobalsDef
$vertShader
  |]

buildShadowVertShader :: String -> String
buildShadowVertShader vertShader = [qm|
$vertHeader
$shadowPassGlobalsDef
$vertShader
  |]

buildWorldFragShader :: String -> String
buildWorldFragShader fragShader = [qm|
$fragHeader
$worldGlobalsDef
$fragShader
  |]
