{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels, OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hickory.Vulkan.Text where

import Acquire (Acquire)
import Hickory.Vulkan.Monad (BufferedUniformMaterial, withBufferedUniformMaterial)
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Linear (M44, V2)
import Linear.V4 (V4)
import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag)
import Hickory.Vulkan.Types (PointedDescriptorSet, RenderConfig, VulkanResources, Attribute (..), TextureLoadOptions (..), pngLoadOptions)
import Hickory.Vulkan.Material (pipelineDefaults, defaultBlend)
import Vulkan (DescriptorSetLayout, SamplerAddressMode (..), Filter (..), SamplerMipmapMode (..))
import Hickory.Text (Font(..), makeFont)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS
import Control.Lens (view)
import Hickory.Vulkan.DescriptorSet (withTextureDescriptorSet)
import Hickory.Vulkan.Framing (FramedResource)
import Data.String.QM (qm)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Hickory.Vulkan.Renderer.ShaderDefinitions
import Data.Word (Word32)

type TextRenderer = (Font, PointedDescriptorSet, Float)

readFont :: FilePath -> IO Font
readFont fontPath = do
  text <- BS.readFile fontPath
  pure case makeFont text of
    Left s -> error s
    Right f -> f

withTextRenderer :: VulkanResources -> FilePath -> FilePath -> Float -> Acquire TextRenderer
withTextRenderer vulkanResources fontPath imagePath sdfPixelRange = do
  let opts = pngLoadOptions { samplerMipmapMode = Just SAMPLER_MIPMAP_MODE_LINEAR }
  fontTex   <- view #descriptorSet <$> withTextureDescriptorSet vulkanResources [(imagePath, opts)]
  font <- liftIO $ readFont fontPath
  pure (font, fontTex, sdfPixelRange)

data MSDFMatConstants = MSDFMatConstants
  { modelMat      :: M44 Float
  , color         :: V4 Float
  , outlineColor  :: V4 Float
  , outlineSize   :: Float -- In pixels
  , sdfPixelRange :: Float -- Should match parameter used to generate MSDF
  , tiling        :: V2 Float
  } deriving Generic
    deriving anyclass GStorable

withOverlayMSDFMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial Word32 MSDFMatConstants)
withOverlayMSDFMaterial vulkanResources renderConfig globalPds perDrawLayout = withBufferedUniformMaterial vulkanResources renderConfig [Position, TextureCoord] (pipelineDefaults [defaultBlend]) vertShader msdfFragShader globalPds (Just perDrawLayout )
  where
  vertShader :: ByteString
  vertShader = $(compileShaderQ Nothing "vert" Nothing [qm|
$header
$overlayGlobalsDef
$msdfUniformsDef

layout(location = 0) in vec3 inPosition;
layout(location = 3) in vec2 inTexCoord;

layout(location = 1) out vec2 texCoord;

void main() {
    gl_Position = globals.viewProjMat
                * uniforms.modelMat
                * vec4(inPosition, 1.0);
    texCoord = uniforms.tiling * inTexCoord;
}

|])

-- withMSDFMaterial :: VulkanResources -> RenderConfig -> FramedResource PointedDescriptorSet -> DescriptorSetLayout -> Acquire (BufferedUniformMaterial Word32 MSDFMatConstants)
-- withMSDFMaterial vulkanResources renderTarget globalPds perDrawLayout = withBufferedUniformMaterial vulkanResources renderTarget [Position, TextureCoord] (pipelineDefaults [defaultBlend]) msdfVertShader msdfFragShader globalPds (Just perDrawLayout )

msdfVertShader :: String
msdfVertShader = [qm|
  layout(location = 0) in vec3 inPosition;
  layout(location = 3) in vec2 inTexCoord;

  layout(location = 1) out vec2 texCoord;
  layout(location = 2) out flat uint outUniformIdx;

  $msdfUniformsDef

  void main() {
      gl_Position = globals.viewProjMat
                  * uniforms.modelMat
                  * vec4(inPosition, 1.0);
      texCoord = uniforms.tiling * inTexCoord;
      outUniformIdx = uniformIdx;
  }
|]

msdfFragShader :: ByteString
msdfFragShader = [frag|
#version 450
#extension GL_EXT_scalar_block_layout : require

layout(location = 1) in vec2 texCoord;
layout(location = 2) in flat uint uniformIdx;

layout(location = 0) out vec4 outColor;

struct Uniforms
{
  mat4 modelMat;
  vec4 color;
  vec4 outlineColor;
  float outlineSize;
  float sdfPixelRange;
  vec2 tiling;
};

layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;
layout (set = 2, binding = 0) uniform sampler2D texSampler;


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
  Uniforms uniforms = uniformBlock.uniforms[uniformIdx];

  float distance = median(texture(texSampler, texCoord).rgb);

  float range = screenPixelRange(uniforms.sdfPixelRange);
  float screenPixelDistance = range * (distance - 0.5);

  if (uniforms.outlineSize > 0)
  {
    vec4 background = vec4(uniforms.outlineColor.rgb, 0);

    vec4 outerColor = mix( background
                         , uniforms.outlineColor
                         , clamp( screenPixelDistance + 1 + uniforms.outlineSize
                           , 0
                           , 2
                           ) / 2);
    outColor = mix( outerColor
                  , uniforms.color
                  , clamp( screenPixelDistance + 1
                         , 0
                         , 2
                         ) / 2);
  }
  else
  {
    vec4 background = vec4(uniforms.color.rgb, 0);
    outColor = mix( background
                  , uniforms.color
                  , clamp( screenPixelDistance + 1
                         , 0
                         , 2
                         ) / 2);
  }
  if (outColor.a < 0.0001) discard;
}

|]
