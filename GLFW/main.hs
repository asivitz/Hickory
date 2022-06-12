{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, QuasiQuotes #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Control.Monad.Managed
import Vulkan
  ( Device
  , Extent2D (..)
  , GraphicsPipelineCreateInfo(..)
  , Pipeline
  , PipelineInputAssemblyStateCreateInfo(..)
  , PipelineShaderStageCreateInfo(..)
  , RenderPass
  , ShaderModuleCreateInfo(..)
  , ShaderStageFlagBits (..)
  , withShaderModule
  , Viewport (..)
  , PipelineViewportStateCreateInfo(..), Rect2D (..)
  , PipelineRasterizationStateCreateInfo(..)
  , PipelineMultisampleStateCreateInfo(..)
  , PipelineColorBlendAttachmentState(..)
  , PipelineColorBlendStateCreateInfo(..), PrimitiveTopology (..), Offset2D (..), withPipelineLayout, PolygonMode (..), CullModeFlagBits (..), FrontFace (..), SampleCountFlagBits (..), ColorComponentFlagBits (..), withGraphicsPipelines
  , PipelineBindPoint (..)
  , PipelineStageFlagBits (..)
  , RenderPassBeginInfo(..), useCommandBuffer, ClearValue (..), ClearColorValue (..), cmdUseRenderPass, SubpassContents (..), cmdBindPipeline, cmdDraw, waitForFences, resetFences
  , acquireNextImageKHR, CommandBuffer(..), resetCommandBuffer
  , SubmitInfo(..)
  , PresentInfoKHR(..), queueSubmit, queuePresentKHR, deviceWaitIdle
  )
import Foreign (Bits ((.|.)))
import Vulkan.Zero
import qualified Data.Vector as V

import qualified Data.ByteString as B
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Control.Monad.Extra (whenM)

import Platforms.GLFW.Vulkan
import Hickory.Vulkan.Vulkan

main :: IO ()
main = withWindow 800 800 "Vulkan Test" $ \win bag@Bag {..} -> do
  let Swapchain {..} = swapchain
      DeviceContext {..} = deviceContext
  runManaged do
    pipeline <- withGraphicsPipeline device renderpass extent

    let loop frameNumber = do
          liftIO GLFW.pollEvents
          drawFrame frameNumber bag pipeline

          whenM (not <$> liftIO (GLFW.windowShouldClose win)) $ loop (frameNumber + 1)
    loop 0

    deviceWaitIdle device

drawFrame :: MonadIO m => Int -> Bag -> Pipeline -> m ()
drawFrame frameNumber Bag {..} pipeline = do
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
      cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
      cmdDraw commandBuffer 3 1 0 0

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


{- GRAPHICS PIPELINE -}

withGraphicsPipeline :: Device -> RenderPass -> Extent2D -> Managed Pipeline
withGraphicsPipeline dev renderPass swapchainExtent = do
  shaderStages   <- sequence [ createVertShader dev vertShader, createFragShader dev fragShader ]
  pipelineLayout <- withPipelineLayout dev zero Nothing allocate

  let
    pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo = zero
      { stages             = shaderStages
      , vertexInputState   = Just zero
      , inputAssemblyState = Just zero
          { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
          , primitiveRestartEnable = False
          }
      , viewportState = Just . SomeStruct $ zero
        { viewports =
          [ Viewport
              { x        = 0
              , y        = 0
              , width    = realToFrac $ width  (swapchainExtent :: Extent2D)
              , height   = realToFrac $ height (swapchainExtent :: Extent2D)
              , minDepth = 0
              , maxDepth = 1
              }
          ]
        , scissors  = [ Rect2D { offset = Offset2D 0 0, extent = swapchainExtent } ]
        }
      , rasterizationState = Just . SomeStruct $ zero
          { depthClampEnable        = False
          , rasterizerDiscardEnable = False
          , polygonMode             = POLYGON_MODE_FILL
          , lineWidth               = 1
          , cullMode                = CULL_MODE_BACK_BIT
          , frontFace               = FRONT_FACE_CLOCKWISE
          , depthBiasEnable         = False
          }
      , multisampleState = Just . SomeStruct $ zero
          { sampleShadingEnable  = False
          , rasterizationSamples = SAMPLE_COUNT_1_BIT
          }
      , depthStencilState = Nothing
      , colorBlendState = Just . SomeStruct $ zero
          { logicOpEnable = False
          , attachments =
            [ zero
              { colorWriteMask
                =   COLOR_COMPONENT_R_BIT
                .|. COLOR_COMPONENT_G_BIT
                .|. COLOR_COMPONENT_B_BIT
                .|. COLOR_COMPONENT_A_BIT
              , blendEnable = False
              }
            ]
          }
      , dynamicState       = Nothing
      , layout             = pipelineLayout
      , renderPass         = renderPass
      , subpass            = 0
      , basePipelineHandle = zero
      }
  V.head . snd
    <$> withGraphicsPipelines dev zero [SomeStruct pipelineCreateInfo] Nothing allocate

{-- SHADERS --}

createVertShader :: Device -> B.ByteString -> Managed (SomeStruct PipelineShaderStageCreateInfo)
createVertShader = createShader SHADER_STAGE_VERTEX_BIT

createFragShader :: Device -> B.ByteString -> Managed (SomeStruct PipelineShaderStageCreateInfo)
createFragShader = createShader SHADER_STAGE_FRAGMENT_BIT

createShader :: ShaderStageFlagBits -> Device -> B.ByteString -> Managed (SomeStruct PipelineShaderStageCreateInfo)
createShader stage dev source = do
  shaderModule <- withShaderModule dev zero { code = source } Nothing allocate
  pure . SomeStruct $ zero
    { stage = stage
    , module' = shaderModule
    , name = "main"
    }

vertShader :: B.ByteString
vertShader = [vert|
  #version 450

  layout(location = 0) out vec3 fragColor;

  vec3 colors[3] = vec3[](
      vec3(1.0, 0.0, 0.0),
      vec3(0.0, 1.0, 0.0),
      vec3(0.0, 0.0, 1.0)
  );

  vec2 positions[3] = vec2[](
      vec2(0.0, -0.5),
      vec2(0.5, 0.5),
      vec2(-0.5, 0.5)
  );

  void main() {
      gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
      fragColor = colors[gl_VertexIndex];
  }

|]

fragShader :: B.ByteString
fragShader = [frag|
  #version 450

  layout(location = 0) in vec3 fragColor;
  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = vec4(fragColor, 1.0);
  }

|]
