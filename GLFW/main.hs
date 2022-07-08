{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, QuasiQuotes, TypeApplications, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main where

import Control.Monad.Managed (Managed)
import Vulkan
  ( pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
  )

import qualified Data.ByteString as B
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)

import Platforms.GLFW.Vulkan
import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.Mesh as H
import qualified Hickory.Vulkan.Material as H
import qualified Hickory.Vulkan.DescriptorSet as H
import Linear.Matrix ((!*!))
import Linear ( M44, transpose, V2 (..) )
import Hickory.Math (perspectiveProjection, mkTranslation)
import Hickory.Math.Matrix ( orthographicProjection, mkScale )

import Hickory.Vulkan.Monad (recordCommandBuffer, useGlobalDecriptorSet, getTexIdx, drawMesh)
import Data.Word (Word32)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Hickory.Types (Size)

data Resources = Resources
  { square              :: H.BufferedMesh
  , solidColorMaterial  :: H.Material Uniform
  , texturedMaterial    :: H.Material Uniform
  , globalDescriptorSet :: H.TextureDescriptorSet
  }

data Uniform = Uniform
  { modelView :: M44 Float
  , texIdx    :: Word32
  , unused1 :: Word32
  , unused2 :: Word32
  , unused3 :: Word32
  } deriving Generic
    deriving anyclass GStorable

acquireResources :: Size Int -> VulkanResources -> SwapchainContext -> Managed Resources
acquireResources _ vulkanResources swapchainContext = do
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
    , indices = Just [0, 1, 2, 2, 3, 0]
    }

  globalDescriptorSet <- H.withTextureDescriptorSet vulkanResources ["star.png", "x.png"]
  solidColorMaterial <- H.withMaterial vulkanResources swapchainContext [H.Position, H.Color, H.TextureCoord] PRIMITIVE_TOPOLOGY_TRIANGLE_LIST vertShader fragShader (Just globalDescriptorSet)
  texturedMaterial   <- H.withMaterial vulkanResources swapchainContext [H.Position, H.Color, H.TextureCoord] PRIMITIVE_TOPOLOGY_TRIANGLE_LIST vertShader texFragShader (Just globalDescriptorSet)
  pure Resources {..}

main :: IO ()
main = withWindow 800 800 "Vulkan Test" \win ->
  runFrames win acquireResources \Resources {..} commandInfo -> do
    recordCommandBuffer commandInfo . useGlobalDecriptorSet globalDescriptorSet texturedMaterial $ do
      let
        screenRatio = 1

        mat :: M44 Float
        mat = perspectiveProjection screenRatio (pi / 2) 0.1 10
          -- !*! mkRotation (V3 0 1 0) (realToFrac frameNumber * pi / 90 / 10)

      drawMesh False solidColorMaterial (Uniform (transpose mat) 0 0 0 0) square

      texidx0 <- getTexIdx "star"
      drawMesh True texturedMaterial (Uniform (transpose $ orthographicProjection 0 100 100 0 0 100 !*! mkTranslation (V2 25 25) !*! mkScale (V2 20 20) :: M44 Float) texidx0 0 0 0) square

      texidx1 <- getTexIdx "x"
      drawMesh True texturedMaterial (Uniform (transpose $ orthographicProjection 0 100 100 0 0 100 !*! mkTranslation (V2 75 25) !*! mkScale (V2 20 20) :: M44 Float) texidx1 0 0 0) square

{-- SHADERS --}

vertShader :: B.ByteString
vertShader = [vert|
  #version 450

  layout(location = 0) in vec3 inPosition;
  layout(location = 1) in vec3 inColor;
  layout(location = 3) in vec2 inTexCoord;

  layout(location = 0) out vec3 fragColor;
  layout(location = 1) out vec2 texCoord;

  struct Uniforms
  {
    mat4 modelViewMatrix;
    int texIdx;
    int unused1;
    int unused2;
    int unused3;
  };

  layout (std140, set = 1, binding = 0) uniform UniformBlock {
    Uniforms uniforms [128];
  } ub;

  layout( push_constant ) uniform constants
  {
    int uniformIdx;
  } PushConstants;

  void main() {
      Uniforms uniforms = ub.uniforms[PushConstants.uniformIdx];
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

  layout(location = 0) in vec3 fragColor;
  layout(location = 1) in vec2 texCoord;

  layout(set = 0, binding = 0) uniform sampler2D texSampler[2];

  layout(location = 0) out vec4 outColor;

  struct Uniforms
  {
    mat4 modelViewMatrix;
    int texIdx;
    int unused1;
    int unused2;
    int unused3;
  };

  layout (std140, set = 1, binding = 0) uniform UniformBlock {
    Uniforms uniforms [128];
  } ub;

  layout( push_constant ) uniform constants
  {
    int uniformIdx;
  } PushConstants;

  void main() {
    Uniforms uniforms = ub.uniforms[PushConstants.uniformIdx];
    outColor = vec4(fragColor, 1.0) * texture(texSampler[ub.uniforms[PushConstants.uniformIdx].texIdx], texCoord);
  }

|]
