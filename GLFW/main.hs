{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, QuasiQuotes, TypeApplications #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad.Managed (runManaged)
import Vulkan
  ( ShaderStageFlagBits (..)
  , deviceWaitIdle
  )

import qualified Data.ByteString as B
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Control.Monad.Extra (whenM)

import Platforms.GLFW.Vulkan
import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.Mesh as H
import qualified Hickory.Vulkan.Material as H
import Linear.Matrix ((!*!))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Linear ( M44, V3(..), transpose )
import Data.Proxy (Proxy(..))
import Hickory.Math (perspectiveProjection)
import Hickory.Math.Matrix (mkRotation)

data Resources = Resources
  { square             :: H.BufferedMesh
  , solidColorMaterial :: H.Material (M44 Float)
  , texturedMaterial   :: H.Material (M44 Float)
  }

main :: IO ()
main = withWindow 800 800 "Vulkan Test" $ \win bag@Bag {..} -> do
  let DeviceContext {..} = deviceContext
  runManaged do
    square <- H.withBufferedMesh bag $ H.Mesh
      { vertices =
            [ (H.Position, [ -0.5, -0.5, 1.0
                           ,  0.5, -0.5, 1.0
                           ,  0.5,  0.5, 1.0
                           , -0.5,  0.5, 1.0
                           ])
            , (H.Color, [ 1.0, 0.0, 0.0
                        , 0.0, 1.0, 0.0
                        , 0.0, 0.0, 1.0
                        , 1.0, 1.0, 1.0
                        ])
            , (H.TextureCoord, [ 0.0, 0.0
                               , 1.0, 0.0
                               , 1.0, 1.0
                               , 0.0, 1.0
                               ])
            ]
      , indices = Just [0, 1, 2, 2, 3, 0]
      }
    solidColorMaterial <- H.withMaterial @(M44 Float) bag [H.Position, H.Color, H.TextureCoord] vertShader fragShader []
    texturedMaterial   <- H.withMaterial @(M44 Float) bag [H.Position, H.Color, H.TextureCoord] vertShader texFragShader ["star.png"]

    let loop frameNumber = do
          liftIO GLFW.pollEvents
          drawFrame frameNumber bag \commandBuffer -> do

            let
              screenRatio = 1

              mat :: M44 Float
              mat = perspectiveProjection screenRatio (pi / 2) 10 0.1
                !*! mkRotation (V3 0 1 0) (realToFrac frameNumber * pi / 90 / 10)


            H.cmdBindMaterial commandBuffer solidColorMaterial
            H.cmdPushMaterialConstants commandBuffer solidColorMaterial (transpose mat)
            H.cmdDrawBufferedMesh commandBuffer square

            -- H.cmdBindMaterial commandBuffer texturedMaterial
            -- H.cmdPushMaterialConstants commandBuffer texturedMaterial SHADER_STAGE_VERTEX_BIT (mkTranslation (V3 1.0 0.0 0.0) :: M44 Float)
            -- H.cmdDrawBufferedMesh commandBuffer square

          whenM (not <$> liftIO (GLFW.windowShouldClose win)) $ loop (frameNumber + 1)
    loop 0

    deviceWaitIdle device

{-- SHADERS --}

vertShader :: B.ByteString
vertShader = [vert|
  #version 450

  layout(location = 0) in vec3 inPosition;
  layout(location = 1) in vec3 inColor;
  layout(location = 3) in vec2 inTexCoord;

  layout(location = 0) out vec3 fragColor;
  layout(location = 1) out vec2 texCoord;

  layout( push_constant ) uniform constants
  {
    mat4 modelViewMatrix;
  } PushConstants;

  void main() {
      gl_Position = PushConstants.modelViewMatrix * vec4(inPosition, 1.0);
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

  layout(binding = 0) uniform sampler2D texSampler;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = vec4(fragColor, 1.0) * texture(texSampler, texCoord);
  }

|]
