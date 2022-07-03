{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds, PatternSynonyms  #-}

module Hickory.Vulkan.DescriptorSet where

import Vulkan.Zero (zero)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hickory.Vulkan.Vulkan (VulkanResources (..), DeviceContext (..), allocate, with2DImageView)
import Control.Monad.Managed (Managed)
import Vulkan
  ( ShaderStageFlagBits (..)
  , withDescriptorPool
  , DescriptorPoolCreateInfo(..), DescriptorPoolSize (..), DescriptorType (..)
  , DescriptorSetAllocateInfo(..)
  , DescriptorSetLayoutCreateInfo(..)
  , DescriptorSetLayoutBinding(..)
  , withDescriptorSetLayout
  , updateDescriptorSets
  , DescriptorImageInfo(..)
  , WriteDescriptorSet(..), Format (..), ImageLayout (..), allocateDescriptorSets
  , DescriptorPool, DescriptorSetLayout, DescriptorSet
  )
import Data.Maybe (maybeToList, listToMaybe)
import Data.Functor (($>))
import Hickory.Vulkan.Textures (withImageSampler, withTextureImage)
import Data.Foldable (for_)
import Data.Traversable (for)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Control.Lens (view)
import System.FilePath.Lens (basename)

data TextureDescriptorSet = TextureDescriptorSet
  { descriptorPool      :: DescriptorPool
  , descriptorSetLayout :: DescriptorSetLayout
  , descriptorSets      :: Vector DescriptorSet
  , textureNames        :: Vector Text -- TODO: Use Text instead of String
  }

withTextureDescriptorSet :: VulkanResources -> [FilePath] -> Managed TextureDescriptorSet
withTextureDescriptorSet _ [] = error "No textures in descriptor set"
withTextureDescriptorSet bag@VulkanResources{..} texturePaths = do
  let DeviceContext {..} = deviceContext
      numImages = fromIntegral $ Prelude.length texturePaths
  descriptorSetLayout <- withDescriptorSetLayout device zero
    -- bind textures as an array of sampled images
    { bindings = V.fromList . maybeToList $ listToMaybe texturePaths $> zero
        { binding         = 0
        , descriptorCount = numImages
        , descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , stageFlags      = SHADER_STAGE_FRAGMENT_BIT
        }
    }
    Nothing allocate

  descriptorPool <- withDescriptorPool device zero
    { maxSets   = 1
    , poolSizes = V.fromList $ texturePaths $> DescriptorPoolSize DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER numImages
    }
    Nothing
    allocate

  -- We use allocateDescriptorSets, rather than withDescriptorSets, b/c we
  -- free all the memory at once via the descriptorPool
  descriptorSets <- allocateDescriptorSets device zero
    { descriptorPool = descriptorPool
    , setLayouts     = [ descriptorSetLayout ]
    }

  for_ (listToMaybe texturePaths) . const $ do
    sampler <- withImageSampler bag
    imageInfos <- for texturePaths \path -> do
      image   <- withTextureImage bag path
      imageView <- with2DImageView deviceContext FORMAT_R8G8B8A8_SRGB image
      pure zero
        { sampler     = sampler
        , imageView   = imageView
        , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        }
    let write = zero
          { dstSet          = V.head descriptorSets
          , dstBinding      = 0
          , dstArrayElement = 0
          , descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , descriptorCount = numImages
          , imageInfo       = V.fromList imageInfos
          }
    updateDescriptorSets device [SomeStruct write] []

  let textureNames = V.fromList $ pack . view basename <$> texturePaths

  pure TextureDescriptorSet {..}
