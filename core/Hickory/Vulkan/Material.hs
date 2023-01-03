{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists, DeriveGeneric #-}
{-# LANGUAGE DataKinds, PatternSynonyms, OverloadedLabels, QuasiQuotes  #-}

module Hickory.Vulkan.Material where

import Vulkan.Zero (zero)
import Hickory.Vulkan.Mesh (bindingDescriptions, attributeDescriptions, attrLocation)
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
  , CompareOp (..), RenderPass, DescriptorSetLayout
  )
import Hickory.Vulkan.Vulkan (mkAcquire, mkAcquire, createVertShader, createFragShader)
import Data.Vector as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Generics.Labels ()
import Control.Lens (view)
import Data.Word (Word32)
import Hickory.Vulkan.Framing (FramedResource, resourceForFrame)
import Data.List (sortOn)
import Acquire.Acquire (Acquire)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Hickory.Vulkan.Types (PointedDescriptorSet, Material (..), RenderTarget (..), VulkanResources (..), Attribute, DeviceContext (..), FrameContext (..))
import Data.Maybe (isJust)

withMaterial
  :: forall f a. Storable a
  => VulkanResources
  -> RenderTarget
  -> f a -- Push Const proxy
  -> [Attribute]
  -> PipelineOptions
  -> B.ByteString
  -> B.ByteString
  -> [FramedResource PointedDescriptorSet]
  -> Maybe DescriptorSetLayout
  -> Acquire (Material a)
withMaterial
  bag@VulkanResources {..}
  RenderTarget {..}
  _pushConstProxy
  (sortOn attrLocation -> attributes)
  pipelineOptions vertShader fragShader
  descriptorSets
  perDrawDescriptorSetLayout
  = do
  let
    DeviceContext {..} = deviceContext
    hasPerDrawDescriptorSet = isJust perDrawDescriptorSetLayout
    pipelineLayoutCreateInfo = zero
      { pushConstantRanges = [ PushConstantRange
          { size = fromIntegral $ sizeOf (undefined :: a)
          , offset = 0
          , stageFlags = SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT
          }]
      , setLayouts = V.fromList $ (view #descriptorSetLayout . resourceForFrame (0 :: Word32)
                               <$> descriptorSets) Prelude.++ maybe [] pure perDrawDescriptorSetLayout
      }
    (globalDescriptorSet : materialDescriptorSet : _) = descriptorSets

  pipelineLayout <- withPipelineLayout device pipelineLayoutCreateInfo Nothing mkAcquire
  pipeline <-
    withGraphicsPipeline bag renderPass 0 samples cullMode extent pipelineOptions vertShader fragShader pipelineLayout (bindingDescriptions attributes) (attributeDescriptions attributes)
  uuid <- liftIO nextRandom

  pure Material {..}

cmdPushMaterialConstants :: (MonadIO m, Storable a) => CommandBuffer -> Material a -> a -> m ()
cmdPushMaterialConstants commandBuffer Material {..} a =
  liftIO $ with a $ cmdPushConstants commandBuffer pipelineLayout (SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT) 0 (fromIntegral $ sizeOf a) . castPtr

cmdBindDrawDescriptorSet :: MonadIO m => CommandBuffer -> Material a -> PointedDescriptorSet -> m ()
cmdBindDrawDescriptorSet commandBuffer Material {..} pds =
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 2 [view #descriptorSet pds] []

cmdBindMaterial :: MonadIO m => FrameContext -> Material a -> m ()
cmdBindMaterial FrameContext {..} Material {..} = do
  cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 sets []
  where
  sets = fmap (view #descriptorSet . resourceForFrame swapchainImageIndex) [globalDescriptorSet, materialDescriptorSet]

{- GRAPHICS PIPELINE -}

data PipelineOptions = PipelineOptions
  { primitiveTopology :: PrimitiveTopology
  , depthTestEnable   :: Bool
  , blendEnable       :: Bool
  }

pipelineDefaults :: PipelineOptions
pipelineDefaults = PipelineOptions {..}
  where
  primitiveTopology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
  depthTestEnable = True
  blendEnable     = True

withGraphicsPipeline
  :: VulkanResources
  -> RenderPass
  -> Word32
  -> SampleCountFlagBits
  -> CullModeFlagBits
  -> Extent2D
  -> PipelineOptions
  -> B.ByteString
  -> B.ByteString
  -> PipelineLayout
  -> V.Vector VertexInputBindingDescription
  -> V.Vector VertexInputAttributeDescription
  -> Acquire Pipeline
withGraphicsPipeline
  VulkanResources {..} renderPass subpassIndex samples cullMode extent
  PipelineOptions {..} vertShader fragShader pipelineLayout vertexBindingDescriptions vertexAttributeDescriptions
  = do
  let DeviceContext {..} = deviceContext
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
          { topology = primitiveTopology
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
          , cullMode                = cullMode
          , frontFace               = FRONT_FACE_COUNTER_CLOCKWISE
          , depthBiasEnable         = False
          }
      , multisampleState = Just . SomeStruct $ zero
          { sampleShadingEnable  = False
          , rasterizationSamples = samples
          }
      , depthStencilState = Just $ zero
        { depthTestEnable       = True
        , depthWriteEnable      = True
        , depthCompareOp        = if depthTestEnable then COMPARE_OP_LESS else COMPARE_OP_ALWAYS
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
              , blendEnable = blendEnable
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
