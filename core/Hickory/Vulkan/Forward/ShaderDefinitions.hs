{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Hickory.Vulkan.Forward.ShaderDefinitions where

import Data.String.QM (qm, qt)
import GHC.TypeLits (Nat, natVal)
import Data.Word (Word32)
import Data.Proxy (Proxy(..))

type MaxShadowCascadesNat :: Nat
type MaxShadowCascadesNat = 3

maxShadowCascades :: Word32
maxShadowCascades = fromIntegral $ natVal (Proxy @MaxShadowCascadesNat)

worldGlobalsDef :: String
worldGlobalsDef = [qm|
layout (row_major, scalar, set = 0, binding = 1) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    mat4 viewProjMat;
    mat4 invViewMat;
    mat4 invProjMat;
    vec3 cameraPos;
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

maxShadowCascadesString :: String
maxShadowCascadesString = show maxShadowCascades

shadowPassGlobalsDef :: String
shadowPassGlobalsDef = [qm|
layout (row_major, scalar, set = 0, binding = 3) uniform ShadowGlobalUniform
  {
    mat4 viewProjMat[$maxShadowCascadesString];
    float splitDepths[$maxShadowCascadesString];
  } shadowGlobals;
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

$pushConstantsDef
$uniformDef
  |]

objectIDUniformsDef :: String
objectIDUniformsDef = [qm|
struct Uniforms
{
  mat4 modelMat;
  uint objectID;
};

$pushConstantsDef
$uniformDef
  |]

pushConstantsDef :: String
pushConstantsDef = [qm|
layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
  |]

gbufferPushConstantsDef :: String
gbufferPushConstantsDef = [qm|
layout (push_constant) uniform constants { uint uniformIdx; uint objectID; } PushConstants;
  |]

shadowPushConstantsDef :: String
shadowPushConstantsDef = [qm|
layout (push_constant) uniform constants { uint uniformIdx; uint cascadeIndex; } PushConstants;
  |]

uniformDef :: String
uniformDef = [qm|
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
  |]

litVertexCalc :: String
litVertexCalc = [qm|
pos = worldPosition.xyz;
viewPos = (globals.viewMat * vec4(worldPosition.xyz, 1.0)).xyz;

vec3 lightDirection = normalize(globals.lightDirection);
vec3 directionToLight = -lightDirection;
vec3 viewDirection = normalize(globals.cameraPos - worldPosition.xyz);

float diffuseIntensity = max(0.0, dot(worldNormal, directionToLight));


vec3 halfAngle = normalize(directionToLight + viewDirection);
float specularIntensity = pow(max(0.0, dot(worldNormal, halfAngle)), uniforms.specularity);

light = (vec3(diffuseIntensity) + vec3(specularIntensity)) * globals.sunColor;

  |]

litVertexDef :: String
litVertexDef = [qt|
layout(location = 0) out vec3 light;
layout(location = 2) out vec3 pos;
layout(location = 3) out vec3 viewPos;

  |]

litFragDef :: String
litFragDef = [qm|
$shadowPassGlobalsDef
layout(location = 0) in vec3 light;
layout(location = 2) in vec3 inPos;
layout(location = 3) in vec3 inViewPos;
layout (set = 0, binding = 4) uniform sampler2DArrayShadow shadowMap;

const mat4 biasMat = mat4(
  0.5, 0.0, 0.0, 0.0,
  0.0, 0.5, 0.0, 0.0,
  0.0, 0.0, 1.0, 0.0,
  0.5, 0.5, 0.0, 1.0 );
  |]

litFragCalc :: String
litFragCalc = [qm|
uint cascadeIndex = 0;

for (uint i = 0; i < $maxShadowCascadesString; i++) {
  if (inViewPos.z < shadowGlobals.splitDepths[i]) {
    cascadeIndex = i; break;
  }
}

vec4 shadowCoord = biasMat * shadowGlobals.viewProjMat[cascadeIndex] * vec4(inPos, 1);

vec4 texcoord;
texcoord.xyw = shadowCoord.xyz / shadowCoord.w;
texcoord.z = cascadeIndex;

float shadow = 0.0;
shadow += textureOffset(shadowMap, texcoord, ivec2(-1,  1));
shadow += textureOffset(shadowMap, texcoord, ivec2( 1,  1));
shadow += textureOffset(shadowMap, texcoord, ivec2(-1, -1));
shadow += textureOffset(shadowMap, texcoord, ivec2( 1, -1));
shadow += textureOffset(shadowMap, texcoord, ivec2( 0, 0));
shadow += textureOffset(shadowMap, texcoord, ivec2( 1, 0));
shadow += textureOffset(shadowMap, texcoord, ivec2( -1, 0));
shadow += textureOffset(shadowMap, texcoord, ivec2( 0, 1));
shadow += textureOffset(shadowMap, texcoord, ivec2( 0, -1));
shadow = shadow / 9.0;

vec4 litColor = vec4((shadow * light + globals.ambientColor) * surfaceColor.rgb, surfaceColor.a);
  |]

staticVertCalc :: String
staticVertCalc = [qt|
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);
  vec4 screenPosition
    = globals.viewProjMat
    * worldPosition;

  gl_Position = screenPosition;
|]

staticShadowVertCalc :: String
staticShadowVertCalc = [qt|
  vec4 worldPosition = uniforms.modelMat * vec4(inPosition, 1.0);
  vec4 screenPosition
    = shadowGlobals.viewProjMat[PushConstants.cascadeIndex]
    * worldPosition;

  gl_Position = screenPosition;
|]

buildWorldVertShader :: String -> String
buildWorldVertShader vertShader = [qm|
$vertHeader
$worldGlobalsDef
$shadowPassGlobalsDef
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
