{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds, PatternSynonyms, OverloadedLabels #-}

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
  , CompareOp (..), RenderPass, DescriptorSetLayout, PipelineRenderingCreateInfo
  )
import Hickory.Vulkan.Vulkan (mkAcquire, mkAcquire, createVertShader, createFragShader)
import Data.Vector as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Generics.Labels ()
import Control.Lens (view, set)
import Data.Word (Word32)
import Hickory.Vulkan.Framing (FramedResource, resourceForFrame)
import Data.List (sortOn)
import Acquire (Acquire)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Hickory.Vulkan.Types (PointedDescriptorSet, Material (..), RenderConfig (..), VulkanResources (..), Attribute, DeviceContext (..), FrameContext (..))
import Data.Maybe (isJust, fromMaybe)
import GHC.Generics (Generic)

withMaterial
  :: forall a. Storable a
  => VulkanResources
  -> RenderConfig
  -> [Attribute]
  -> PipelineOptions
  -> CullModeFlagBits
  -> B.ByteString
  -> B.ByteString
  -> [FramedResource PointedDescriptorSet]
  -> Maybe DescriptorSetLayout
  -> Acquire (Material a)
withMaterial
  bag@VulkanResources {..}
  RenderConfig {..}
  (sortOn attrLocation -> attributes)
  pipelineOptions cullMode vertShader fragShader
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

  pipelineLayout <- withPipelineLayout device pipelineLayoutCreateInfo Nothing mkAcquire
  pipeline <-
    withGraphicsPipeline bag renderPassInfo 0 samples extent pipelineOptions cullMode vertShader fragShader pipelineLayout (bindingDescriptions attributes) (attributeDescriptions attributes)
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
  sets = V.fromList $ fmap (view #descriptorSet . resourceForFrame swapchainImageIndex) descriptorSets

{- GRAPHICS PIPELINE -}

data PipelineOptions = PipelineOptions
  { primitiveTopology :: PrimitiveTopology
  , depthTestEnable   :: Bool
  , depthClampEnable  :: Bool
  , colorBlends       :: V.Vector PipelineColorBlendAttachmentState
  , cullMode          :: CullModeFlagBits
  , shadowCullMode    :: CullModeFlagBits
  } deriving Generic

pipelineDefaults :: V.Vector PipelineColorBlendAttachmentState -> PipelineOptions
pipelineDefaults colorBlends = PipelineOptions {..}
  where
  primitiveTopology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
  depthTestEnable = True
  depthClampEnable = False
  cullMode = CULL_MODE_BACK_BIT
  shadowCullMode = CULL_MODE_FRONT_BIT

withGraphicsPipeline
  :: VulkanResources
  -- Left for traditional renderpass, right for dynamic rendering
  -> Either RenderPass PipelineRenderingCreateInfo
  -> Word32
  -> SampleCountFlagBits
  -> Extent2D
  -> PipelineOptions
  -> CullModeFlagBits
  -> B.ByteString
  -> B.ByteString
  -> PipelineLayout
  -> V.Vector VertexInputBindingDescription
  -> V.Vector VertexInputAttributeDescription
  -> Acquire Pipeline
withGraphicsPipeline
  VulkanResources {..} renderPassInfo subpassIndex samples extent
  pipelineOptions cullMode vertShader fragShader pipelineLayout vertexBindingDescriptions vertexAttributeDescriptions
  = do
  let DeviceContext {..} = deviceContext
  shaderStages   <- V.sequence [ createVertShader device vertShader, createFragShader device fragShader ]

  let
    -- pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
    pipelineCreateInfoBase = zero
      { stages             = shaderStages
      , vertexInputState   = Just . SomeStruct $ zero
        { vertexBindingDescriptions   = vertexBindingDescriptions
        , vertexAttributeDescriptions = vertexAttributeDescriptions
        }
      , inputAssemblyState = Just zero
          { topology = pipelineOptions.primitiveTopology
          , primitiveRestartEnable = False
          }
      , viewportState = Just . SomeStruct $ zero
        { viewports =
          [ Viewport
              { x        = 0
              , y        = 0
              , width    = realToFrac extent.width
              , height   = realToFrac extent.height
              , minDepth = 0
              , maxDepth = 1
              }
          ]
        , scissors  = [ Rect2D { offset = Offset2D 0 0, extent = extent } ]
        }
      , rasterizationState = Just . SomeStruct $ zero
          { depthClampEnable        = pipelineOptions.depthClampEnable
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
        , depthCompareOp        = if pipelineOptions.depthTestEnable then COMPARE_OP_LESS else COMPARE_OP_ALWAYS
        , depthBoundsTestEnable = False
        , stencilTestEnable     = False
        }
      , colorBlendState = Just . SomeStruct $ zero
          { logicOpEnable = False
          , attachments = pipelineOptions.colorBlends
          }
      , dynamicState       = Nothing
      , layout             = pipelineLayout
      , subpass            = subpassIndex
      , basePipelineHandle = zero
      }

    createInfo = case renderPassInfo of
      Left renderPass -> SomeStruct $ pipelineCreateInfoBase { renderPass = renderPass }
      Right prci      -> SomeStruct $ (pipelineCreateInfoBase { next = (prci, ()) } :: GraphicsPipelineCreateInfo '[PipelineRenderingCreateInfo])

  V.head . snd
    <$> withGraphicsPipelines device zero [createInfo] Nothing mkAcquire

defaultBlend :: PipelineColorBlendAttachmentState
defaultBlend = zero
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

noBlend :: PipelineColorBlendAttachmentState
noBlend = zero
  { blendEnable = False
  , colorWriteMask
    =   COLOR_COMPONENT_R_BIT
    .|. COLOR_COMPONENT_G_BIT
    .|. COLOR_COMPONENT_B_BIT
    .|. COLOR_COMPONENT_A_BIT
  , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
  , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
  , colorBlendOp = BLEND_OP_ADD
  , srcAlphaBlendFactor = BLEND_FACTOR_ONE
  , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
  , alphaBlendOp = BLEND_OP_ADD
  }

colorBlendAddAlpha :: PipelineColorBlendAttachmentState
colorBlendAddAlpha = zero
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
  , dstAlphaBlendFactor = BLEND_FACTOR_ONE
  , alphaBlendOp = BLEND_OP_ADD
  }

colorBlendLeaveAlpha :: PipelineColorBlendAttachmentState
colorBlendLeaveAlpha = zero
  { colorWriteMask
    =   COLOR_COMPONENT_R_BIT
    .|. COLOR_COMPONENT_G_BIT
    .|. COLOR_COMPONENT_B_BIT
    .|. COLOR_COMPONENT_A_BIT
  , blendEnable = True
  , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
  , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
  , colorBlendOp = BLEND_OP_ADD
  , srcAlphaBlendFactor = BLEND_FACTOR_ZERO
  , dstAlphaBlendFactor = BLEND_FACTOR_ONE
  , alphaBlendOp = BLEND_OP_ADD
  }
