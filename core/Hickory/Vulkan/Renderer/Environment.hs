{-# LANGUAGE OverloadedLists, OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Hickory.Vulkan.Renderer.Environment where

import Hickory.Vulkan.Mesh (withSingleTimeCommands)
import Hickory.Vulkan.Types (TextureLoadOptions(..), ImageType (..), VulkanResources (..), RenderConfig (..), Material(..), DescriptorSpec (..), PointedDescriptorSet(..), ViewableImage (..), ConversionTo3D (..))
import Vulkan (Filter(..), SamplerAddressMode (..), RenderingInfo(..), RenderingAttachmentInfo(..), Rect2D (..), Offset2D (..), Extent2D (..), AttachmentStoreOp (..), AttachmentLoadOp (..), ImageLayout (..), ClearValue (..), ClearColorValue (..), cmdDraw, cmdUseRendering, ImageCreateInfo(..), SharingMode (..), ImageType (..), Extent3D (..), ImageTiling (..), SampleCountFlagBits (..), ImageUsageFlagBits (..), ImageCreateFlagBits (..), Format (..), MemoryPropertyFlagBits (..), ImageAspectFlagBits (..), ImageViewType (..), PipelineRenderingCreateInfo (..), CullModeFlagBits (..), PipelineBindPoint (..), cmdBindPipeline, cmdBindDescriptorSets, SamplerMipmapMode (..), DescriptorSet, Sampler)
import Vulkan.Zero (zero)
import Acquire (Acquire (..))
import VulkanMemoryAllocator (withImage, AllocationCreateInfo(..))
import Data.Bits ((.|.))
import Hickory.Vulkan.Vulkan (mkAcquire, with2DImageViewMips)
import Data.Traversable (for)
import Data.Foldable (for_)
import Hickory.Vulkan.Material (withMaterial, defaultBlend, pipelineDefaults, cmdPushMaterialConstants)
import Data.Word (Word32)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet)
import Data.String.QM (qm)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.ByteString (ByteString)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Linear (V3 (..), (!*!), inv44)
import Hickory.Math (Mat44, viewDirection)
import Hickory.Camera (shotMatrix, Projection (..))
import Hickory.Vulkan.Framing (unframedResource)
import Hickory.Vulkan.Textures (transitionImageLayoutMips, withImageSamplerMips, cubeFormat)
import Data.Vector (Vector)

withEnvMapRenderConfig :: Extent2D -> Acquire RenderConfig
withEnvMapRenderConfig extent = do
  let samples = SAMPLE_COUNT_1_BIT
      renderPassInfo = Right PipelineRenderingCreateInfo
        { colorAttachmentFormats = [cubeFormat]
        , depthAttachmentFormat = FORMAT_UNDEFINED
        , stencilAttachmentFormat = FORMAT_UNDEFINED
        , viewMask = 0
        }
  pure RenderConfig {..}

vertShader :: ByteString
vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header

layout (location = 0) out vec3 worldRay; // From camera toward fragment

layout( push_constant, scalar, row_major ) uniform constants
{ mat4 invViewProj;
} PushConstants;

void main()
{
    vec2 texCoords = vec2(gl_VertexIndex & 2, (gl_VertexIndex << 1) & 2 );
    gl_Position = vec4(texCoords * 2.0f + -1.0f, 1.0f, 1.0f);

    vec4 world = PushConstants.invViewProj * vec4(gl_Position.x,gl_Position.y,1,1);
    worldRay = world.xyz / world.w;
}
|])

cubeMapFragShader :: ByteString
cubeMapFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header

layout (location = 0) in vec3 worldRay;
layout (location = 0) out vec4 outColor;

layout (set = 0, binding = 0) uniform sampler2D equirectangularMap;

#define PI 3.1415926535

vec2 directionToEquirectangularUV(vec3 dir) {
  float phi   = -atan(dir.y, dir.x);
  float theta = acos(clamp(dir.z, -1.0, 1.0));

  float u = 1 - (phi - PI/2) / (2.0 * PI);
  float v = theta / PI;

  return vec2(u, v);
}

void main()
{
  vec2 uv = directionToEquirectangularUV(normalize(worldRay));
  vec3 color = texture(equirectangularMap, uv).rgb;

  outColor = vec4(color, 1.0);
}
|])

irradianceFragShader :: ByteString
irradianceFragShader = $(compileShaderQ Nothing "frag" Nothing [qm|
$header

layout (location = 0) in vec3 worldRay;
layout (location = 0) out vec4 outColor;

layout (set = 0, binding = 0) uniform samplerCube environmentMap;

#define PI 3.1415926535

// Jarzynski and Olano
uint pcg_hash(uint v)
{
    uint state = v * 747796405u + 2891336453u;
    uint word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

float uintToFloat01(uint value) {
    return float(value) / 4294967295.0; // uint max
}

void main()
{
  vec3 normal = normalize(worldRay);
  vec3 upIsh  = vec3(0, 1, 0);
  vec3 right  = normalize(cross(upIsh, normal));
  vec3 up     = normalize(cross(normal, right));

  // monte carlo sampling
  const uint totalSamples = 512;
  vec3 irradiance = vec3(0);
  for (uint i = 0; i < totalSamples; i++)
  {
    uint a = pcg_hash(i);
    uint b = pcg_hash(a);

    float theta = acos(sqrt(1 - uintToFloat01(a))); // importance sampling
    float phi = 2 * PI * uintToFloat01(b);
    vec3 tangentSpace = vec3(sin(theta) * cos(phi), sin(theta) * sin(phi), cos(theta));
    vec3 sampleWorld  = tangentSpace.x * right + tangentSpace.y * up + tangentSpace.z * normal;

    irradiance += texture(environmentMap, sampleWorld).rgb * max(dot(sampleWorld, normal), 0.0);
  }
  irradiance /= float(totalSamples);

  outColor = vec4(irradiance, 1);
}
|])

-- TODO: Can we garbage collect? E.g. the render material, the hdr image, etc
renderEnvironmentMap :: VulkanResources -> Extent2D -> FilePath -> Acquire (ViewableImage, Sampler)
renderEnvironmentMap vulkanResources faceExtent pathToHDR = do
  let options = TextureLoadOptions
        { filter = FILTER_LINEAR
        , samplerAddressMode = SAMPLER_ADDRESS_MODE_REPEAT
        , samplerMipmapMode = Nothing
        , isCubemap = False
        , fileType = HDR
        , conversionTo3D = Simply2D
        , shouldFlipVertically = False
        }

  hdrDescSet <- withDescriptorSet vulkanResources [ImageFileDescriptor (pathToHDR, options)]

  renderConfig <- withEnvMapRenderConfig faceExtent
  material :: Material Mat44 <- withMaterial vulkanResources "EnvironmentMap" renderConfig
    [] (pipelineDefaults [defaultBlend]) CULL_MODE_BACK_BIT vertShader cubeMapFragShader [unframedResource hdrDescSet] Nothing
  renderCubeMap vulkanResources faceExtent material [hdrDescSet.descriptorSet]

-- Face size of 32x32 should be good enough
renderIrradianceMap :: VulkanResources -> Extent2D -> DescriptorSpec -> Acquire (ViewableImage, Sampler)
renderIrradianceMap vulkanResources faceExtent envMapDescSpec = do
  renderConfig <- withEnvMapRenderConfig faceExtent
  descSet <- withDescriptorSet vulkanResources [envMapDescSpec]
  material :: Material Mat44 <- withMaterial vulkanResources "Irradiance" renderConfig
    [] (pipelineDefaults [defaultBlend]) CULL_MODE_BACK_BIT vertShader irradianceFragShader [unframedResource descSet] Nothing
  renderCubeMap vulkanResources faceExtent material [descSet.descriptorSet]

renderCubeMap :: VulkanResources -> Extent2D -> Material Mat44 -> Vector DescriptorSet -> Acquire (ViewableImage, Sampler)
renderCubeMap vulkanResources@VulkanResources {..} faceExtent material descriptorSets = do
  let cubeImageCreateInfo :: ImageCreateInfo '[]
      cubeImageCreateInfo = zero
        { imageType     = IMAGE_TYPE_2D
        , extent        = Extent3D (fromIntegral faceExtent.width) (fromIntegral faceExtent.height) 1
        , format        = cubeFormat
        , mipLevels     = 1
        , arrayLayers   = 6
        , tiling        = IMAGE_TILING_OPTIMAL
        , samples       = SAMPLE_COUNT_1_BIT
        , usage         = IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. IMAGE_USAGE_SAMPLED_BIT
        , sharingMode   = SHARING_MODE_EXCLUSIVE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        , flags         = IMAGE_CREATE_CUBE_COMPATIBLE_BIT
        }
      allocationCreateInfo :: AllocationCreateInfo
      allocationCreateInfo = zero { requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }

  (cubeImage, _, _) <- withImage allocator cubeImageCreateInfo allocationCreateInfo mkAcquire
  cubeSampler       <- withImageSamplerMips vulkanResources 1 FILTER_LINEAR SAMPLER_ADDRESS_MODE_REPEAT SAMPLER_MIPMAP_MODE_LINEAR
  cubeImageView     <- with2DImageViewMips deviceContext cubeFormat IMAGE_ASPECT_COLOR_BIT cubeImage 1 IMAGE_VIEW_TYPE_CUBE 0 6
  let cubeViewableImage = ViewableImage cubeImage cubeImageView cubeFormat
      cubeDescriptorSpec = ImageDescriptor [(cubeViewableImage, cubeSampler)]

  faceViews <- for ([0..5] :: [Word32]) \i -> do
    fv <- with2DImageViewMips deviceContext cubeFormat IMAGE_ASPECT_COLOR_BIT cubeImage 1 IMAGE_VIEW_TYPE_2D i 1
    let viewMat = shotMatrix (Perspective (pi/2) 0.1 1) 1
        mat = case i of
          0 -> inv44 $ viewMat !*! viewDirection (V3 0 0 0) (V3 (-1) 0 0) (V3 0 (1) 0)
          1 -> inv44 $ viewMat !*! viewDirection (V3 0 0 0) (V3 1 0 0)    (V3 0 (1) 0)
          2 -> inv44 $ viewMat !*! viewDirection (V3 0 0 0) (V3 0 1 0)    (V3 0 0 (-1))
          3 -> inv44 $ viewMat !*! viewDirection (V3 0 0 0) (V3 0 (-1) 0) (V3 0 0 1)
          4 -> inv44 $ viewMat !*! viewDirection (V3 0 0 0) (V3 0 0 1)    (V3 0 (1) 0)
          5 -> inv44 $ viewMat !*! viewDirection (V3 0 0 0) (V3 0 0 (-1)) (V3 0 (1) 0)
          _ -> error "Cube map face index out of bounds"

    pure (fv, mat)

  withSingleTimeCommands vulkanResources \commandBuffer -> for_ faceViews \(faceView, mat) -> do
    transitionImageLayoutMips cubeImage 1 6 IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL commandBuffer
    cmdUseRendering commandBuffer (zero
      { layerCount = 1
      , renderArea = Rect2D { offset = Offset2D 0 0, extent = faceExtent }
      , colorAttachments =
        [ zero
          { imageView = faceView
          , imageLayout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , loadOp = ATTACHMENT_LOAD_OP_CLEAR
          , storeOp = ATTACHMENT_STORE_OP_STORE
          , clearValue = Color (Float32 0 0 0 1)
          }
        ]
      }) do

      cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS material.pipeline
      cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS material.pipelineLayout 0 descriptorSets []
      cmdPushMaterialConstants commandBuffer material mat
      cmdDraw commandBuffer 3 1 0 0

    transitionImageLayoutMips cubeImage 1 6 IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL commandBuffer
  pure (cubeViewableImage, cubeSampler)
