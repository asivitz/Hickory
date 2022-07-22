{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds, PatternSynonyms, OverloadedLabels  #-}

module Hickory.Vulkan.Material where

import Vulkan.Zero (zero)
import Hickory.Vulkan.Mesh (Attribute(..), bindingDescriptions, attributeDescriptions, attrLocation)
import qualified Data.ByteString as B
import Foreign (sizeOf, castPtr, with, Storable, (.|.))
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
import Control.Monad.Managed (Managed)
import Hickory.Vulkan.Vulkan (VulkanResources(..), SwapchainContext(..), DeviceContext (..), allocate, withGraphicsPipeline)
import Data.Vector as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Vulkan.Core10 (PrimitiveTopology)
import Hickory.Vulkan.DescriptorSet (TextureDescriptorSet(..), BufferDescriptorSet(..), withBufferDescriptorSet)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Generics.Labels ()
import Control.Lens (view)
import Data.Word (Word32)
import Hickory.Vulkan.Framing (FramedResource, frameResource, resourceForFrame)
import Data.List (sortOn)

data Material uniform = Material
  { pipeline            :: Pipeline
  , pipelineLayout      :: PipelineLayout
  , materialDescriptor  :: FramedResource (BufferDescriptorSet uniform) -- ^Each frame has its own buffer to write uniform data for each draw call
  , uuid                :: UUID
  , attributes          :: [Attribute]
  }

withMaterial
  :: forall uniform. (Storable uniform)
  => VulkanResources
  -> SwapchainContext
  -> [Attribute]
  -> PrimitiveTopology
  -> B.ByteString
  -> B.ByteString
  -> Maybe TextureDescriptorSet
  -> Managed (Material uniform)
withMaterial bag@VulkanResources {..} swapchainContext (sortOn attrLocation -> attributes) topology vertShader fragShader descriptorSet = do
  let DeviceContext {..} = deviceContext

  materialDescriptor <- frameResource $ withBufferDescriptorSet bag

  let
    pipelineLayoutCreateInfo = zero
      { pushConstantRanges = [ PushConstantRange
          { size = fromIntegral $ sizeOf (undefined :: Word32)
          , offset = 0
          , stageFlags = SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT
          }]
      , setLayouts = V.fromList $ maybe id (:) (view #descriptorSetLayout <$> descriptorSet)
                                               [view #descriptorSetLayout (resourceForFrame (0 :: Int) materialDescriptor)]
      }

  pipelineLayout <- withPipelineLayout device pipelineLayoutCreateInfo Nothing allocate
  pipeline <- withGraphicsPipeline bag swapchainContext topology vertShader fragShader pipelineLayout (bindingDescriptions attributes) (attributeDescriptions attributes)
  uuid <- liftIO nextRandom

  pure Material {..}

cmdBindMaterial :: MonadIO m => Int -> CommandBuffer -> Material bufferUniform -> m ()
cmdBindMaterial frameNumber commandBuffer Material {..} = do
  cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 1 (view #descriptorSets (resourceForFrame frameNumber materialDescriptor)) []

cmdPushMaterialConstants :: (MonadIO m) => CommandBuffer -> Material bufferUniform -> Word32 -> m ()
cmdPushMaterialConstants commandBuffer Material {..} a =
  liftIO $ with a $ cmdPushConstants commandBuffer pipelineLayout (SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT) 0 (fromIntegral $ sizeOf a) . castPtr
