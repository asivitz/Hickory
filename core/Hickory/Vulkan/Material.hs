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
  , Pipeline
  , PipelineLayout
  , PushConstantRange(..), PipelineLayout, ShaderStageFlagBits (..)
  , withPipelineLayout
  , cmdPushConstants
  , cmdBindPipeline
  , pattern PIPELINE_BIND_POINT_GRAPHICS, cmdBindDescriptorSets
  )
import Hickory.Vulkan.Vulkan (VulkanResources(..), Swapchain(..), DeviceContext (..), mkAcquire, withGraphicsPipeline, mkAcquire)
import Data.Vector as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Vulkan.Core10 (PrimitiveTopology)
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
  -> [Attribute]
  -> PrimitiveTopology
  -> B.ByteString
  -> B.ByteString
  -> [FramedResource PointedDescriptorSet]       -- Descriptor sets bound along with material
  -> Maybe (FramedResource PointedDescriptorSet) -- Descriptor set bound per draw
  -> Acquire Material
withMaterial
  bag@VulkanResources {..}
  swapchainContext
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
  pipeline <- withGraphicsPipeline bag swapchainContext topology vertShader fragShader pipelineLayout (bindingDescriptions attributes) (attributeDescriptions attributes)
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
