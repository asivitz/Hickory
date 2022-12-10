{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, QuasiQuotes, TypeApplications, DerivingStrategies, DeriveGeneric, DeriveAnyClass, OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main where

import Vulkan
  ( pattern FILTER_LINEAR, Instance
  , SamplerAddressMode(..)
  , PrimitiveTopology(..)
  )

import qualified Data.ByteString as B
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)

import Platforms.GLFW.Vulkan
import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.Mesh as H
import qualified Hickory.Vulkan.DescriptorSet as H
import qualified Hickory.Vulkan.Monad as H
import qualified Hickory.Vulkan.RenderPass as H
import qualified Hickory.Vulkan.Types as H
import Linear.Matrix ((!*!))
import Linear ( M44, V2 (..), V4(..), V3(..), identity)
import Hickory.Math (perspectiveProjection, mkTranslation)
import Hickory.Math.Matrix ( orthographicProjection, mkScale )
import Hickory.Vulkan.Frame (FrameContext(..))
import Hickory.Vulkan.Material (pipelineDefaults)

import Hickory.Vulkan.Monad (drawMesh)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Hickory.Types (Size)
import Control.Lens (view)
import Acquire.Acquire (Acquire)

data Resources = Resources
  { target              :: H.ForwardRenderTarget
  , square              :: H.BufferedMesh
  , solidColorMaterial  :: H.BufferedUniformMaterial Uniform
  , texturedMaterial    :: H.BufferedUniformMaterial Uniform
  , starTex             :: H.PointedDescriptorSet
  , xTex                :: H.PointedDescriptorSet
  }

data Uniform = Uniform
  { modelView :: M44 Float
  } deriving Generic
    deriving anyclass GStorable

acquireResources :: Size Int -> Instance -> VulkanResources -> Swapchain -> Acquire Resources
acquireResources _ _ vulkanResources swapchain = do
  target@H.ForwardRenderTarget {..} <- H.withForwardRenderTarget vulkanResources swapchain []
  square <- H.withBufferedMesh vulkanResources $ H.Mesh
    { vertices =
          [ (H.Position, [ -0.5, -0.5, 1.0
                         ,  0.5, -0.5, 1.0
                         ,  0.5,  0.5, 1.0
                         , -0.5,  0.5, 1.0
                         ])
          , (H.Color, [ 1.0, 0.0, 0.0, 0.0
                      , 0.0, 1.0, 0.0, 0.0
                      , 0.0, 0.0, 1.0, 0.0
                      , 1.0, 1.0, 1.0, 0.0
                      ])
          , (H.TextureCoord, [ 0.0, 0.0
                             , 1.0, 0.0
                             , 1.0, 1.0
                             , 0.0, 1.0
                             ])
          ]
    , indices = Just [0, 2, 1, 2, 0, 3]
    }

  starTex <- view #descriptorSet <$> H.withTextureDescriptorSet vulkanResources [("star.png", FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)]
  xTex    <- view #descriptorSet <$> H.withTextureDescriptorSet vulkanResources [("x.png", FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)]

  solidColorMaterial <- H.withBufferedUniformMaterial vulkanResources target [H.Position, H.Color, H.TextureCoord] pipelineDefaults vertShader fragShader Nothing
  texturedMaterial   <- H.withBufferedUniformMaterial vulkanResources target [H.Position, H.Color, H.TextureCoord] pipelineDefaults vertShader texFragShader (Just xTex)
  pure Resources {..}

main :: IO ()
main = withWindow 800 800 "Vulkan Test" \win ->
  runFrames win acquireResources \Resources {..} frameContext@FrameContext {..} -> H.runFrame frameContext
    . H.runBatchIO
    $ do
      let
        overlayF = do
          pure ()
        litF = do
          let
            screenRatio = 1

            mat :: M44 Float
            mat = perspectiveProjection screenRatio (pi / 2) 0.1 10
              -- !*! mkRotation (V3 0 1 0) (realToFrac frameNumber * pi / 90 / 10)

          drawMesh solidColorMaterial (Uniform mat) square Nothing id

          drawMesh texturedMaterial (Uniform (orthographicProjection 0 100 100 0 0 100 !*! mkTranslation (V2 25 25) !*! mkScale (V2 20 20) :: M44 Float)) square (Just starTex) H.doBlend
          drawMesh texturedMaterial (Uniform (orthographicProjection 0 100 100 0 0 100 !*! mkTranslation (V2 75 25) !*! mkScale (V2 20 20) :: M44 Float)) square (Just xTex) H.doBlend

      H.renderToForwardTarget target (V4 0 0 0 1) H.globalDefaults (H.PostConstants 0 (V3 1 1 1) 1 0 frameNumber) litF overlayF

{-- SHADERS --}

vertShader :: B.ByteString
vertShader = [vert|
  #version 450
  #extension GL_EXT_scalar_block_layout : require

  layout(location = 0) in vec3 inPosition;
  layout(location = 1) in vec3 inColor;
  layout(location = 3) in vec2 inTexCoord;

  layout(location = 0) out vec3 fragColor;
  layout(location = 1) out vec2 texCoord;

  struct Uniforms
  {
    mat4 modelViewMatrix;
  };

  layout (push_constant) uniform constants { uint uniformIdx; } PushConstants;
  layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock { Uniforms uniforms [128]; } uniformBlock;

  void main() {
      Uniforms uniforms = uniformBlock.uniforms[PushConstants.uniformIdx];
      gl_Position = uniforms.modelViewMatrix * vec4(inPosition, 1.0);
      fragColor = inColor;
      texCoord = inTexCoord;
  }

|]

fragShader :: B.ByteString
fragShader = [frag|
  #version 450

  layout(location = 0) in vec3 fragColor;
  layout(location = 1) in vec2 texCoord;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = vec4(fragColor, 1.0);
  }

|]

texFragShader :: B.ByteString
texFragShader = [frag|
  #version 450
  #extension GL_EXT_scalar_block_layout : require
  #extension GL_EXT_nonuniform_qualifier : require

  layout(location = 0) in vec3 fragColor;
  layout(location = 1) in vec2 texCoord;

  layout(location = 0) out vec4 outColor;

  struct Uniforms
  {
    mat4 modelViewMatrix;
  };

  layout (row_major, scalar, set = 1, binding = 0) uniform UniformBlock {
    Uniforms uniforms [128];
  } ub;

  layout(set = 2, binding = 0) uniform sampler2D texSampler;

  layout( push_constant ) uniform constants
  {
    int uniformIdx;
  } PushConstants;

  void main() {
    Uniforms uniforms = ub.uniforms[PushConstants.uniformIdx];
    outColor = vec4(fragColor, 1.0) * texture(texSampler, texCoord);
  }

|]
