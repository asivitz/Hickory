{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds, PatternSynonyms, OverloadedLabels  #-}

module Hickory.Vulkan.Material where

import Vulkan.Zero (zero)
import Hickory.Vulkan.Mesh (Attribute(..), bindingDescriptions, attributeDescriptions, attrLocation)
import qualified Data.ByteString as B
import Foreign (sizeOf, castPtr, with, (.|.))
import Vulkan
  ( CommandBuffer
  , PipelineLayoutCreateInfo(..)
  , Pipeline
  , PipelineLayout
  , PushConstantRange(..), PipelineLayout, ShaderStageFlagBits (..)
  , withPipelineLayout
  , cmdPushConstants
  , cmdBindPipeline
  , pattern PIPELINE_BIND_POINT_GRAPHICS, cmdBindDescriptorSets, DescriptorSet
  )
import Hickory.Vulkan.Vulkan (VulkanResources(..), Swapchain(..), DeviceContext (..), mkAcquire, withGraphicsPipeline, mkAcquire)
import Data.Vector as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Vulkan.Core10 (PrimitiveTopology)
import Hickory.Vulkan.DescriptorSet (PointedDescriptorSet, descriptorSetBinding)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Generics.Labels ()
import Control.Lens (view)
import Data.Word (Word32)
import Hickory.Vulkan.Framing (FramedResource, resourceForFrame)
import Data.List (sortOn)
import Acquire.Acquire (Acquire)

data Material = Material
  { pipeline            :: Pipeline
  , pipelineLayout      :: PipelineLayout
  , materialDescriptor  :: FramedResource (Vector DescriptorSet)
  , uuid                :: UUID
  , attributes          :: [Attribute]
  }

withMaterial
  :: VulkanResources
  -> Swapchain
  -> [Attribute]
  -> PrimitiveTopology
  -> B.ByteString
  -> B.ByteString
  -> FramedResource PointedDescriptorSet
  -> Maybe PointedDescriptorSet
  -> Acquire Material
withMaterial
  bag@VulkanResources {..}
  swapchainContext
  (sortOn attrLocation -> attributes)
  topology vertShader fragShader
  materialDescriptorSet
  globalDescriptorSet = do
  let
    (materialDescriptorLayout, materialDescriptor) = descriptorSetBinding materialDescriptorSet
    DeviceContext {..} = deviceContext
    pipelineLayoutCreateInfo = zero
      { pushConstantRanges = [ PushConstantRange
          { size = fromIntegral $ sizeOf (undefined :: Word32)
          , offset = 0
          , stageFlags = SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT
          }]
      , setLayouts = V.fromList $ maybe id (:) (view #descriptorSetLayout <$> globalDescriptorSet)
                                               [materialDescriptorLayout]
      }

  pipelineLayout <- withPipelineLayout device pipelineLayoutCreateInfo Nothing mkAcquire
  pipeline <- withGraphicsPipeline bag swapchainContext topology vertShader fragShader pipelineLayout (bindingDescriptions attributes) (attributeDescriptions attributes)
  uuid <- liftIO nextRandom

  pure Material {..}

cmdBindMaterial :: MonadIO m => Int -> CommandBuffer -> Material -> m ()
cmdBindMaterial frameNumber commandBuffer Material {..} = do
  cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 1 (resourceForFrame frameNumber materialDescriptor) []

cmdPushMaterialConstants :: (MonadIO m) => CommandBuffer -> Material -> Word32 -> m ()
cmdPushMaterialConstants commandBuffer Material {..} a =
  liftIO $ with a $ cmdPushConstants commandBuffer pipelineLayout (SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT) 0 (fromIntegral $ sizeOf a) . castPtr
