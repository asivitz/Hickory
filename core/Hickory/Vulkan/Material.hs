{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds, PatternSynonyms  #-}

module Hickory.Vulkan.Material where

import Vulkan.Zero (zero)
import Hickory.Vulkan.Mesh (Attribute(..), bindingDescription, attributeDescriptions)
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
  , pattern PIPELINE_BIND_POINT_GRAPHICS
  )
import Control.Monad.Managed (Managed)
import Hickory.Vulkan.Vulkan (VulkanResources(..), SwapchainContext(..), DeviceContext (..), allocate, withGraphicsPipeline)
import Data.Vector as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (maybeToList)
import Vulkan.Core10 (PrimitiveTopology)
import Hickory.Vulkan.DescriptorSet (TextureDescriptorSet, descriptorSetLayout)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

data Material pushConstant = Material
  { pipeline            :: Pipeline
  , pipelineLayout      :: PipelineLayout
  -- , materialDescriptor  :: Maybe MaterialDescriptor
  , uuid                :: UUID
  }

withMaterial :: forall pushConstant. Storable pushConstant => VulkanResources -> SwapchainContext -> [Attribute] -> PrimitiveTopology -> B.ByteString -> B.ByteString -> Maybe TextureDescriptorSet -> Managed (Material pushConstant)
withMaterial bag@VulkanResources {..} swapchainContext attrs topology vertShader fragShader descriptorSet = do
  let DeviceContext {..} = deviceContext

  -- materialDescriptor <- withMaterialDescriptor bag texturePaths

  let
    pipelineLayoutCreateInfo = zero
      { pushConstantRanges = [ PushConstantRange
          { size = fromIntegral $ sizeOf (undefined :: pushConstant)
          , offset = 0
          , stageFlags = SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT
          }]
      , setLayouts = V.fromList . maybeToList . fmap descriptorSetLayout $ descriptorSet
      }

  pipelineLayout <- withPipelineLayout device pipelineLayoutCreateInfo Nothing allocate
  pipeline <- withGraphicsPipeline bag swapchainContext topology vertShader fragShader pipelineLayout (bindingDescription attrs) (attributeDescriptions attrs)
  uuid <- liftIO nextRandom

  pure Material {..}

cmdBindMaterial :: MonadIO m => CommandBuffer -> Material pushConstant -> m ()
cmdBindMaterial commandBuffer Material {..} = do
  cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
  -- for_ materialDescriptor \MaterialDescriptor {..} ->
  --   cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 descriptorSets []

cmdPushMaterialConstants :: (MonadIO m, Storable pushConstant) => CommandBuffer -> Material pushConstant -> pushConstant -> m ()
cmdPushMaterialConstants commandBuffer Material {..} a =
  liftIO $ with a $ cmdPushConstants commandBuffer pipelineLayout (SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT) 0 (fromIntegral $ sizeOf a) . castPtr
