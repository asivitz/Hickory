{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists, DeriveGeneric #-}
{-# LANGUAGE DataKinds, PatternSynonyms, OverloadedLabels  #-}

module Hickory.Vulkan.Material where

import Vulkan.Zero (zero)
import Hickory.Vulkan.Mesh (Attribute(..), bindingDescriptions, attributeDescriptions, attrLocation)
import qualified Data.ByteString as B
import Foreign (sizeOf, castPtr, with, (.|.))
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
import Hickory.Vulkan.DescriptorSet (PointedDescriptorSet)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Generics.Labels ()
import Control.Lens (view)
import Data.Word (Word32)
import Hickory.Vulkan.Framing (FramedResource, resourceForFrame)
import Data.List (sortOn)
import Acquire.Acquire (Acquire)
import GHC.Generics (Generic)
import Data.Maybe (maybeToList)
import Vulkan.CStruct.Extends (SomeStruct(..))

data Material = Material
  { pipeline               :: Pipeline
  , pipelineLayout         :: PipelineLayout
  , materialDescriptorSets :: [FramedResource PointedDescriptorSet] -- Bound along with the material
  , uuid                   :: UUID
  , attributes             :: [Attribute]
  } deriving Generic

withMaterial
  :: VulkanResources
  -> Swapchain
  -> RenderPass
  -> Bool                                        -- Lit? If True, use first subpass and multisample FIXME: Too tied to renderpass structure?
  -> [Attribute]
  -> PrimitiveTopology
  -> B.ByteString
  -> B.ByteString
  -> [FramedResource PointedDescriptorSet]       -- Descriptor sets bound along with material
  -> Maybe (FramedResource PointedDescriptorSet) -- Descriptor set bound per draw
  -> Acquire Material
withMaterial
  bag@VulkanResources {..}
  swapchain
  renderPass
  lit
  (sortOn attrLocation -> attributes)
  topology vertShader fragShader
  materialDescriptorSets
  drawDescriptorSet
  = do
  let
    DeviceContext {..} = deviceContext
    pipelineLayoutCreateInfo = zero
      { pushConstantRanges = [ PushConstantRange
          { size = fromIntegral $ sizeOf (undefined :: Word32)
          , offset = 0
          , stageFlags = SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT
          }]
      , setLayouts = V.fromList $ view #descriptorSetLayout . resourceForFrame (0 :: Int) <$> (materialDescriptorSets Prelude.++ maybeToList drawDescriptorSet)
      }

  pipelineLayout <- withPipelineLayout device pipelineLayoutCreateInfo Nothing mkAcquire
  pipeline <- withGraphicsPipeline bag swapchain renderPass lit topology vertShader fragShader pipelineLayout (bindingDescriptions attributes) (attributeDescriptions attributes)
  uuid <- liftIO nextRandom

  pure Material {..}

cmdBindMaterial :: MonadIO m => Int -> CommandBuffer -> Material -> m ()
cmdBindMaterial frameNumber commandBuffer Material {..} = do
  cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 sets []
  where
  sets = V.fromList $ view #descriptorSet . resourceForFrame frameNumber <$> materialDescriptorSets

cmdPushMaterialConstants :: (MonadIO m) => CommandBuffer -> Material -> Word32 -> m ()
cmdPushMaterialConstants commandBuffer Material {..} a =
  liftIO $ with a $ cmdPushConstants commandBuffer pipelineLayout (SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT) 0 (fromIntegral $ sizeOf a) . castPtr

cmdBindDrawDescriptorSet :: MonadIO m => CommandBuffer -> Material -> PointedDescriptorSet -> m ()
cmdBindDrawDescriptorSet commandBuffer Material {..} pds =
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout (fromIntegral $ Prelude.length materialDescriptorSets) [view #descriptorSet pds] []

{- GRAPHICS PIPELINE -}

withGraphicsPipeline
  :: VulkanResources
  -> Swapchain
  -> RenderPass
  -> Bool
  -> PrimitiveTopology
  -> B.ByteString
  -> B.ByteString
  -> PipelineLayout
  -> V.Vector VertexInputBindingDescription
  -> V.Vector VertexInputAttributeDescription
  -> Acquire Pipeline
withGraphicsPipeline
  VulkanResources {..} swapchain renderPass lit
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
          , rasterizationSamples = if lit then maxSampleCount else SAMPLE_COUNT_1_BIT
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
      , subpass            = if lit then 0 else 1
      , basePipelineHandle = zero
      , renderPass
      }
  V.head . snd
    <$> withGraphicsPipelines device zero [SomeStruct pipelineCreateInfo] Nothing mkAcquire
