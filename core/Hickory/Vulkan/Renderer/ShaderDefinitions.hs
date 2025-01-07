{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Hickory.Vulkan.Renderer.ShaderDefinitions where

import Data.String.QM (qm, qt)
import GHC.TypeLits (Nat, natVal)
import Data.Word (Word32)
import Data.Proxy (Proxy(..))

type MaxShadowCascadesNat :: Nat
type MaxShadowCascadesNat = 4

maxShadowCascades :: Word32
maxShadowCascades = fromIntegral $ natVal (Proxy @MaxShadowCascadesNat)
maxShadowCascadesString :: String
maxShadowCascadesString = show maxShadowCascades


type MaxSSAOKernelSizeNat :: Nat
type MaxSSAOKernelSizeNat = 64

maxSSAOKernelSize :: Word32
maxSSAOKernelSize = fromIntegral $ natVal (Proxy @MaxSSAOKernelSizeNat)
maxSSAOKernelSizeString :: String
maxSSAOKernelSizeString = show maxSSAOKernelSize

cascadeOverlapThreshold :: Float
cascadeOverlapThreshold = 10.0

worldGlobalsDef :: String
worldGlobalsDef = [qm|
layout (row_major, scalar, set = 0, binding = 1) uniform GlobalUniform
  { mat4 viewMat;
    mat4 projMat;
    mat4 viewProjMat;
    mat4 invViewMat;
    mat4 invProjMat;
    vec3 cameraPos;
    float envMapStrength;
    vec3 lightDirection;
    float padding2;
    vec3 sunColor;
    float padding3;
    vec3 ambientColor;
    float padding4;
    vec2 gbufferSize;
    float multiSampleCount;
    float nearPlane;
    float farPlane;
    float diffuseMask;
    float specularMask;
    float ssaoMask;
    float shadowsMask;
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

cascadeOverlapThresholdString :: String
cascadeOverlapThresholdString = show cascadeOverlapThreshold

shadowPassGlobalsDef :: String
shadowPassGlobalsDef = [qm|
layout (row_major, scalar, set = 0, binding = 3) uniform ShadowGlobalUniform
  {
    mat4 viewProjMat[$maxShadowCascadesString];
    float splitDepths[$maxShadowCascadesString];
    float shadowBiasSlope;
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
  vec4 material;
  vec2 tiling;
};

$instancedUniformDef
  |]
--
-- nonInstancedStaticUniformsDef :: String
-- nonInstancedStaticUniformsDef = [qm|
-- struct Uniforms
-- {
--   mat4 modelMat;
--   mat3 normalMat;
--   vec4 color;
--   float specularity;
--   vec2 tiling;
-- };
--
-- $uniformDef
--   |]

animatedUniformsDef :: String
animatedUniformsDef = [qm|
struct Uniforms {
  mat4 modelMat;
  mat3 normalMat;
  vec4 color;
  vec4 material;
  uint skinIdx;
  vec4 colors[6];
};

$instancedUniformDef
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
$instancedUniformDef
  |]

decalUniformsDef :: String
decalUniformsDef = [qm|
struct Uniforms
{
  mat4 modelMat;
  mat3 normalMat;
  mat4 invModelViewProjMat;
  vec4 color;
  uint receiverId;
};
  |]

pushConstantsDef :: String
pushConstantsDef = [qm|
//layout (push_constant) uniform constants { uint instanceIdx; } PushConstants;
  |]

gbufferPushConstantsDef :: String
gbufferPushConstantsDef = [qm|
//layout (push_constant) uniform constants { uint instanceIdx; } PushConstants;
  |]

shadowPushConstantsDef :: String
shadowPushConstantsDef = [qm|
layout (push_constant) uniform constants { uint cascadeIndex; } PushConstants;
  |]

-- uniformDef :: String
-- uniformDef = [qm|
-- layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
-- Uniforms uniforms = uniformBlock.uniforms[gl_InstanceIndex];
--   |]

instancedUniformDef :: String
instancedUniformDef = [qm|
layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
layout (scalar, set = 1, binding = 1) uniform ObjectIds { uint objectIds [128]; };
layout (scalar, set = 1, binding = 2) uniform InstanceIndices { uint instanceIndices[128]; };
uint uniformIdx = instanceIndices[gl_InstanceIndex];
Uniforms uniforms = uniformBlock.uniforms[uniformIdx];
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

buildDirectVertShader :: String -> String
buildDirectVertShader vertShader = [qm|
$header
$worldGlobalsDef
$vertShader
  |]

buildOverlayVertShader :: String -> String
buildOverlayVertShader vertShader = [qm|
$header
$overlayGlobalsDef
$vertShader
  |]
