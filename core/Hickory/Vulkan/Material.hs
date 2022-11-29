{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists, DeriveGeneric #-}
{-# LANGUAGE DataKinds, PatternSynonyms, OverloadedLabels  #-}

module Hickory.Vulkan.Material where

import Vulkan.Zero (zero)
import Hickory.Vulkan.Mesh (Attribute(..), bindingDescriptions, attributeDescriptions, attrLocation)
import qualified Data.ByteString as B
import Foreign (sizeOf, castPtr, with, (.|.), Storable)
import Vulkan
  ( CommandBuffer
  , PipelineLayoutCreateInfo(..)
  , PushConstantRange(..), PipelineLayout, ShaderStageFlagBits (..)
  , withPipelineLayout
  , cmdPushConstants
  , cmdBindPipeline
  , pattern PIPELINE_BIND_POINT_GRAPHICS, cmdBindDescriptorSets
  , Extent2D (..)
  , SampleCountFlagBits (..)
  , GraphicsPipelineCreateInfo(..)
  , Pipeline
  , PipelineInputAssemblyStateCreateInfo(..)
  , Viewport (..)
  , PipelineViewportStateCreateInfo(..), Rect2D (..)
  , PipelineRasterizationStateCreateInfo(..)
  , PipelineMultisampleStateCreateInfo(..)
  , PipelineColorBlendAttachmentState(..)
  , PipelineColorBlendStateCreateInfo(..), PrimitiveTopology (..), Offset2D (..), PolygonMode (..), CullModeFlagBits (..), FrontFace (..), ColorComponentFlagBits (..), withGraphicsPipelines
  , PipelineVertexInputStateCreateInfo(..), VertexInputBindingDescription, VertexInputAttributeDescription
  , BlendOp (..), BlendFactor (..)
  , PipelineDepthStencilStateCreateInfo(..)
  , CompareOp (..), RenderPass
  )
import Hickory.Vulkan.Vulkan (VulkanResources(..), Swapchain(..), DeviceContext (..), mkAcquire, mkAcquire, createVertShader, createFragShader)
import Data.Vector as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Generics.Labels ()
import Control.Lens (view)
import Data.Word (Word32)
import Hickory.Vulkan.Framing (FramedResource, resourceForFrame, doubleResource)
import Data.List (sortOn)
import Acquire.Acquire (Acquire)
import Data.Maybe (catMaybes, maybeToList)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Hickory.Vulkan.Types (PointedDescriptorSet, Material (..), RenderTarget (..))

withMaterial
  :: forall f a. Storable a
  => VulkanResources
  -> Swapchain
  -> RenderTarget
  -> f a -- Push Const proxy
  -> [Attribute]
  -> PrimitiveTopology
  -> B.ByteString
  -> B.ByteString
  -> FramedResource PointedDescriptorSet -- Descriptor sets bound along with material
  -> Maybe (FramedResource PointedDescriptorSet) -- Descriptor set bound per draw
  -> Acquire (Material a)
withMaterial
  bag@VulkanResources {..}
  swapchain
  RenderTarget {..}
  _pushConstProxy
  (sortOn attrLocation -> attributes)
  topology vertShader fragShader
  materialDescriptorSet
  drawDescriptorSet
  = do
  let
    DeviceContext {..} = deviceContext
    pipelineLayoutCreateInfo = zero
      { pushConstantRanges = [ PushConstantRange
          { size = fromIntegral $ sizeOf (undefined :: a)
          , offset = 0
          , stageFlags = SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT
          }]
      , setLayouts = V.fromList $ view #descriptorSetLayout . resourceForFrame (0 :: Int)
                               <$> [ doubleResource globalDescriptorSet
                                   , materialDescriptorSet
                                   ] Prelude.++ maybeToList drawDescriptorSet
      }

  pipelineLayout <- withPipelineLayout device pipelineLayoutCreateInfo Nothing mkAcquire
  shadowPipeline       <- withGraphicsPipeline bag swapchain renderPass 0 False topology vertShader fragShader pipelineLayout (bindingDescriptions attributes) (attributeDescriptions attributes)
  multiSamplePipeline  <- withGraphicsPipeline bag swapchain renderPass 1 True  topology vertShader fragShader pipelineLayout (bindingDescriptions attributes) (attributeDescriptions attributes)
  singleSamplePipeline <- withGraphicsPipeline bag swapchain renderPass 2 False topology vertShader fragShader pipelineLayout (bindingDescriptions attributes) (attributeDescriptions attributes)
  uuid <- liftIO nextRandom

  pure Material {..}

cmdPushMaterialConstants :: (MonadIO m, Storable a) => CommandBuffer -> Material a -> a -> m ()
cmdPushMaterialConstants commandBuffer Material {..} a =
  liftIO $ with a $ cmdPushConstants commandBuffer pipelineLayout (SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT) 0 (fromIntegral $ sizeOf a) . castPtr

cmdBindDrawDescriptorSet :: MonadIO m => CommandBuffer -> Material a -> PointedDescriptorSet -> m ()
cmdBindDrawDescriptorSet commandBuffer Material {..} pds =
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 2 [view #descriptorSet pds] []

cmdBindMaterial :: MonadIO m => Int -> Word32 -> CommandBuffer -> Material a -> m ()
cmdBindMaterial frameNumber subpassIdx commandBuffer Material {..} = do
  let pipeline = case subpassIdx of
        0 -> shadowPipeline
        1 -> multiSamplePipeline
        2 -> singleSamplePipeline
        _ -> error "Invalid subpass index"

  cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 1 sets []
  where
  sets = [view #descriptorSet . resourceForFrame frameNumber $ materialDescriptorSet]

{- GRAPHICS PIPELINE -}

withGraphicsPipeline
  :: VulkanResources
  -> Swapchain
  -> RenderPass
  -> Word32
  -> Bool
  -> PrimitiveTopology
  -> B.ByteString
  -> B.ByteString
  -> PipelineLayout
  -> V.Vector VertexInputBindingDescription
  -> V.Vector VertexInputAttributeDescription
  -> Acquire Pipeline
withGraphicsPipeline
  VulkanResources {..} swapchain renderPass subpassIndex multisample
  topology vertShader fragShader pipelineLayout vertexBindingDescriptions vertexAttributeDescriptions
  = do
  let DeviceContext {..} = deviceContext
  let Swapchain {..} = swapchain
  shaderStages   <- V.sequence [ createVertShader device vertShader, createFragShader device fragShader ]

  let
    pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo = zero
      { stages             = shaderStages
      , vertexInputState   = Just . SomeStruct $ zero
        { vertexBindingDescriptions   = vertexBindingDescriptions
        , vertexAttributeDescriptions = vertexAttributeDescriptions
        }
      , inputAssemblyState = Just zero
          { topology = topology
          , primitiveRestartEnable = False
          }
      , viewportState = Just . SomeStruct $ zero
        { viewports =
          [ Viewport
              { x        = 0
              , y        = 0
              , width    = realToFrac $ width  (extent :: Extent2D)
              , height   = realToFrac $ height (extent :: Extent2D)
              , minDepth = 0
              , maxDepth = 1
              }
          ]
        , scissors  = [ Rect2D { offset = Offset2D 0 0, extent = extent } ]
        }
      , rasterizationState = Just . SomeStruct $ zero
          { depthClampEnable        = False
          , rasterizerDiscardEnable = False
          , polygonMode             = POLYGON_MODE_FILL
          , lineWidth               = 1
          , cullMode                = CULL_MODE_BACK_BIT
          , frontFace               = FRONT_FACE_COUNTER_CLOCKWISE
          , depthBiasEnable         = False
          }
      , multisampleState = Just . SomeStruct $ zero
          { sampleShadingEnable  = False
          , rasterizationSamples = if multisample then maxSampleCount else SAMPLE_COUNT_1_BIT
          }
      , depthStencilState = Just $ zero
        { depthTestEnable       = True
        , depthWriteEnable      = True
        , depthCompareOp        = COMPARE_OP_LESS
        , depthBoundsTestEnable = False
        , stencilTestEnable     = False
        }
      , colorBlendState = Just . SomeStruct $ zero
          { logicOpEnable = False
          , attachments =
            [ zero
              { colorWriteMask
                =   COLOR_COMPONENT_R_BIT
                .|. COLOR_COMPONENT_G_BIT
                .|. COLOR_COMPONENT_B_BIT
                .|. COLOR_COMPONENT_A_BIT
              , blendEnable = True
              , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
              , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
              , colorBlendOp = BLEND_OP_ADD
              , srcAlphaBlendFactor = BLEND_FACTOR_ONE
              , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
              , alphaBlendOp = BLEND_OP_ADD
              }
            ]
          }
      , dynamicState       = Nothing
      , layout             = pipelineLayout
      , subpass            = subpassIndex
      , basePipelineHandle = zero
      , renderPass
      }
  V.head . snd
    <$> withGraphicsPipelines device zero [SomeStruct pipelineCreateInfo] Nothing mkAcquire
