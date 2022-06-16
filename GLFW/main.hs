{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, QuasiQuotes, TypeApplications #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Control.Monad.Managed (runManaged)
import Vulkan
  ( ShaderStageFlagBits (..)
  , Rect2D (..)
  , PipelineStageFlagBits (..)
  , RenderPassBeginInfo(..), useCommandBuffer, ClearValue (..), ClearColorValue (..), cmdUseRenderPass, SubpassContents (..), waitForFences, resetFences
  , acquireNextImageKHR, CommandBuffer(..), resetCommandBuffer
  , SubmitInfo(..)
  , PresentInfoKHR(..), queueSubmit, queuePresentKHR, deviceWaitIdle
  )
import Vulkan.Zero
import qualified Data.Vector as V

import qualified Data.ByteString as B
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Control.Monad.Extra (whenM)

import Platforms.GLFW.Vulkan
import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.Mesh as H
import Linear.Matrix (identity)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Linear (M44)
import Data.Proxy (Proxy(..))

data Resources = Resources
  { square             :: H.BufferedMesh
  , solidColorMaterial :: H.Material
  , texturedMaterial   :: H.Material
  }

main :: IO ()
main = withWindow 800 800 "Vulkan Test" $ \win bag@Bag {..} -> do
  let DeviceContext {..} = deviceContext
  runManaged do
    square <- H.withBufferedMesh bag $ H.Mesh
      { vertices =
            [ (H.Position, [ -0.5, -0.5, 0.0
                          ,  0.5, -0.5, 0.0
                          ,  0.5,  0.5, 0.0
                          , -0.5,  0.5, 0.0
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
    solidColorMaterial <- H.withMaterial bag [(Proxy @(M44 Float), SHADER_STAGE_VERTEX_BIT)] (H.meshAttributes . H.mesh $ square) vertShader fragShader
    texturedMaterial   <- H.withMaterial bag [(Proxy @(M44 Float), SHADER_STAGE_VERTEX_BIT)] (H.meshAttributes . H.mesh $ square) vertShader fragShader

    let loop frameNumber = do
          liftIO GLFW.pollEvents
          drawFrame frameNumber bag Resources {..}

          whenM (not <$> liftIO (GLFW.windowShouldClose win)) $ loop (frameNumber + 1)
    loop 0

    deviceWaitIdle device

drawFrame :: MonadIO m => Int -> Bag -> Resources -> m ()
drawFrame frameNumber Bag {..} Resources {..} = do
  let Swapchain {..} = swapchain
      DeviceContext {..} = deviceContext
      Frame {..} = frames V.! (frameNumber `mod` V.length frames)

  _ <- waitForFences device [ inFlightFence ] True maxBound
  resetFences device [ inFlightFence ]

  (_, imageIndex) <- acquireNextImageKHR device swapchainHandle maxBound imageAvailableSemaphore zero

  let framebuffer = framebuffers V.! fromIntegral imageIndex

  resetCommandBuffer commandBuffer zero

  useCommandBuffer commandBuffer zero do
    let renderPassBeginInfo = zero
          { renderPass  = renderpass
          , framebuffer = framebuffer
          , renderArea  = Rect2D { offset = zero , extent = extent }
          , clearValues = [ Color (Float32 0.0 0.0 0.0 1.0) ]
          }
    cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE do
      H.cmdBindMaterial commandBuffer solidColorMaterial
      H.cmdPushMaterialConstants commandBuffer solidColorMaterial SHADER_STAGE_VERTEX_BIT (identity :: M44 Float)
      H.cmdDrawBufferedMesh commandBuffer square

  let submitInfo = zero
        { waitSemaphores   = [imageAvailableSemaphore]
        , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
        , commandBuffers   = [commandBufferHandle  commandBuffer]
        , signalSemaphores = [renderFinishedSemaphore]
        }
  queueSubmit graphicsQueue [SomeStruct submitInfo] inFlightFence
  void $ queuePresentKHR presentQueue $ zero
    { waitSemaphores = [renderFinishedSemaphore]
    , swapchains     = [swapchainHandle]
    , imageIndices   = [imageIndex]
    }

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
