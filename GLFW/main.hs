{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, QuasiQuotes, TypeApplications, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad.Managed (runManaged)
import Vulkan
  ( deviceWaitIdle
  , pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
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
import Linear ( M44, V3(..), transpose, V2 (..) )
import Hickory.Math (perspectiveProjection, mkTranslation)
import Hickory.Math.Matrix ( mkRotation, orthographicProjection, mkScale )

import Hickory.Vulkan.Monad (targetCommandBuffer, useMaterial, pushConstant, draw)
import Data.Word (Word32)
import Foreign (Storable)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)

data Resources = Resources
  { square             :: H.BufferedMesh
  , solidColorMaterial :: H.Material (M44 Float)
  , texturedMaterial   :: H.Material (M44 Float)
  }

data PushConstants = PushConstants
  { modelView :: M44 Float
  , texIdx    :: Word32
  } deriving Generic
    deriving anyclass GStorable

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
    solidColorMaterial <- H.withMaterial @PushConstants bag [H.Position, H.Color, H.TextureCoord] PRIMITIVE_TOPOLOGY_TRIANGLE_LIST vertShader fragShader []
    texturedMaterial   <- H.withMaterial @PushConstants bag [H.Position, H.Color, H.TextureCoord] PRIMITIVE_TOPOLOGY_TRIANGLE_LIST vertShader texFragShader ["star.png", "x.png"]

    let loop frameNumber = do
          liftIO GLFW.pollEvents
          drawFrame frameNumber bag . flip targetCommandBuffer $ do

            let
              screenRatio = 1

              mat :: M44 Float
              mat = perspectiveProjection screenRatio (pi / 2) 0.1 10
                !*! mkRotation (V3 0 1 0) (realToFrac frameNumber * pi / 90 / 10)

            useMaterial solidColorMaterial do
              pushConstant (PushConstants (transpose mat) 0)
              draw square

            useMaterial texturedMaterial do
              pushConstant (PushConstants (transpose $ orthographicProjection 0 100 100 0 0 100 !*! mkTranslation (V2 25 25) !*! mkScale (V2 20 20) :: M44 Float) 0)
              draw square

              pushConstant (PushConstants (transpose $ orthographicProjection 0 100 100 0 0 100 !*! mkTranslation (V2 75 25) !*! mkScale (V2 20 20) :: M44 Float) 1)
              draw square

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
    int texIdx;
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

  layout(binding = 0) uniform sampler2D texSampler[2];

  layout(location = 0) out vec4 outColor;

  layout( push_constant ) uniform constants
  {
    mat4 modelViewMatrix;
    int texIdx;
  } PushConstants;

  void main() {
    outColor = vec4(fragColor, 1.0) * texture(texSampler[PushConstants.texIdx], texCoord);
  }

|]
