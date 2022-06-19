{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds, PatternSynonyms  #-}

module Hickory.Vulkan.Material where

import Vulkan.Zero (zero)
import Hickory.Vulkan.Mesh (Attribute(..), bindingDescription, attributeDescriptions)
import Data.Proxy (Proxy (..))
import qualified Data.ByteString as B
import Foreign (sizeOf, castPtr, with, Storable)
import Vulkan
  ( CommandBuffer
  , PipelineLayoutCreateInfo(..)
  , Pipeline
  , PipelineLayout
  , PushConstantRange(..), PipelineLayout, ShaderStageFlagBits (..)
  , withPipelineLayout
  , cmdPushConstants
  , cmdBindPipeline
  , pattern PIPELINE_BIND_POINT_GRAPHICS, DescriptorPool
  , withDescriptorPool
  , DescriptorPoolCreateInfo(..), DescriptorPoolSize (..), DescriptorType (..)
  , DescriptorSetAllocateInfo(..)
  , DescriptorSetLayoutCreateInfo(..)
  , DescriptorSetLayoutBinding(..)
  , withDescriptorSetLayout, DescriptorSetLayout, DescriptorSet
  , updateDescriptorSets
  , DescriptorImageInfo(..)
  , WriteDescriptorSet(..), Format (..), ImageLayout (..), cmdBindDescriptorSets, allocateDescriptorSets
  )
import Control.Monad.Managed (Managed)
import Hickory.Vulkan.Vulkan (Bag(..), DeviceContext (..), allocate, withGraphicsPipeline, with2DImageView)
import Data.Vector as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Vulkan.CStruct.Extends (SomeStruct(..))
import Hickory.Vulkan.Textures (withImageSampler, withTextureImage)

data Material = Material
  { pipeline            :: Pipeline
  , pipelineLayout      :: PipelineLayout
  , descriptorPool      :: DescriptorPool
  , descriptorSetLayout :: DescriptorSetLayout
  , descriptorSets      :: V.Vector DescriptorSet
  }

withMaterial :: Storable a => Bag -> [(Proxy a, ShaderStageFlagBits)] -> [Attribute] -> B.ByteString -> B.ByteString -> FilePath -> Managed Material
withMaterial bag@Bag {..} pushConstants attrs vertShader fragShader texturePath = do
  let DeviceContext {..} = deviceContext
  descriptorSetLayout <- withDescriptorSetLayout device zero
    { bindings =
      [ zero
        { binding         = 0
        , descriptorCount = 1
        , descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , stageFlags      = SHADER_STAGE_FRAGMENT_BIT
        }
      ]
    }
    Nothing allocate

  let
    pipelineLayoutCreateInfo = zero
      { pushConstantRanges = V.fromList $ pushConstants <&> \case
        (Proxy :: Proxy b, stageFlags) -> zero
          { size = fromIntegral $ sizeOf (undefined :: b)
          , stageFlags = stageFlags
          }
      , setLayouts = [ descriptorSetLayout ]
      }

  pipelineLayout <- withPipelineLayout device pipelineLayoutCreateInfo Nothing allocate
  pipeline <- withGraphicsPipeline bag vertShader fragShader pipelineLayout (bindingDescription attrs) (attributeDescriptions attrs)

  descriptorPool <- withDescriptorPool device zero
    { maxSets   = 1
    , poolSizes =
      [ DescriptorPoolSize DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
      ]
    }
    Nothing
    allocate

  -- We use allocateDescriptorSets, rather than withDescriptorSets, b/c we
  -- free all the memory at once via the descriptorPool
  descriptorSets <- allocateDescriptorSets device zero
    { descriptorPool = descriptorPool
    , setLayouts     = [ descriptorSetLayout ]
    }

  sampler <- withImageSampler bag
  image   <- withTextureImage bag texturePath
  imageView <- with2DImageView deviceContext FORMAT_R8G8B8A8_SRGB image

  updateDescriptorSets device
    [ SomeStruct $ zero
      { dstSet          = V.head descriptorSets
      , dstBinding      = 0
      , dstArrayElement = 0
      , descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , descriptorCount = 1
      , imageInfo       =
        [ zero
          { sampler     = sampler
          , imageView   = imageView
          , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          }
        ]
      }
    ]
    []

  pure Material {..}

cmdBindMaterial :: MonadIO m => CommandBuffer -> Material -> m ()
cmdBindMaterial commandBuffer Material {..} = do
  cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
  cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 descriptorSets []

cmdPushMaterialConstants :: (MonadIO m, Storable a) => CommandBuffer -> Material -> ShaderStageFlagBits -> a -> m ()
cmdPushMaterialConstants commandBuffer Material {..} flagBits a =
  liftIO $ with a $ cmdPushConstants commandBuffer pipelineLayout flagBits 0 (fromIntegral $ sizeOf a) . castPtr
