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
import Data.Functor (($>))
import Vulkan.CStruct.Extends (SomeStruct(..))
import Hickory.Vulkan.Textures (withImageSampler, withTextureImage)
import Data.Traversable (for)
import Data.Maybe (maybeToList)
import Data.Foldable (for_)
import Vulkan.Core10 (PrimitiveTopology)

data MaterialDescriptor = MaterialDescriptor
  { descriptorPool      :: DescriptorPool
  , descriptorSetLayout :: DescriptorSetLayout
  , descriptorSets      :: V.Vector DescriptorSet
  }

data Material pushConstant = Material
  { pipeline            :: Pipeline
  , pipelineLayout      :: PipelineLayout
  , materialDescriptor  :: Maybe MaterialDescriptor
  }

withMaterialDescriptor :: Bag -> [FilePath] -> Managed (Maybe MaterialDescriptor)
withMaterialDescriptor _ [] = pure Nothing
withMaterialDescriptor bag@Bag{..} texturePaths = do
  let DeviceContext {..} = deviceContext
  descriptorSetLayout <- withDescriptorSetLayout device zero
    { bindings = V.fromList $ texturePaths $> zero
        { binding         = 0
        , descriptorCount = 1
        , descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , stageFlags      = SHADER_STAGE_FRAGMENT_BIT
        }
    }
    Nothing allocate

  descriptorPool <- withDescriptorPool device zero
    { maxSets   = 1
    , poolSizes = V.fromList $ texturePaths $> DescriptorPoolSize DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
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
  writes <- for texturePaths \path -> do
    image   <- withTextureImage bag path
    imageView <- with2DImageView deviceContext FORMAT_R8G8B8A8_SRGB image
    pure zero
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

  updateDescriptorSets device (V.fromList $ SomeStruct <$> writes) []

  pure . Just $ MaterialDescriptor {..}

withMaterial :: forall pushConstant. Storable pushConstant => Bag -> [Attribute] -> PrimitiveTopology -> B.ByteString -> B.ByteString -> [FilePath] -> Managed (Material pushConstant)
withMaterial bag@Bag {..} attrs topology vertShader fragShader texturePaths = do
  let DeviceContext {..} = deviceContext

  materialDescriptor <- withMaterialDescriptor bag texturePaths

  let
    pipelineLayoutCreateInfo = zero
      { pushConstantRanges = [ PushConstantRange
          { size = fromIntegral $ sizeOf (undefined :: pushConstant)
          , offset = 0
          , stageFlags = SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT
          }]
      , setLayouts = V.fromList . maybeToList . fmap descriptorSetLayout $ materialDescriptor
      }

  pipelineLayout <- withPipelineLayout device pipelineLayoutCreateInfo Nothing allocate
  pipeline <- withGraphicsPipeline bag topology vertShader fragShader pipelineLayout (bindingDescription attrs) (attributeDescriptions attrs)

  pure Material {..}

cmdBindMaterial :: MonadIO m => CommandBuffer -> Material pushConstant -> m ()
cmdBindMaterial commandBuffer Material {..} = do
  cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
  for_ materialDescriptor \MaterialDescriptor {..} ->
    cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 descriptorSets []

cmdPushMaterialConstants :: (MonadIO m, Storable pushConstant) => CommandBuffer -> Material pushConstant -> pushConstant -> m ()
cmdPushMaterialConstants commandBuffer Material {..} a =
  liftIO $ with a $ cmdPushConstants commandBuffer pipelineLayout (SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT) 0 (fromIntegral $ sizeOf a) . castPtr
