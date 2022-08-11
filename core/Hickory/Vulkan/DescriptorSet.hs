{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE DataKinds, DeriveGeneric, PatternSynonyms  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Hickory.Vulkan.DescriptorSet where

import Vulkan.Zero (zero)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hickory.Vulkan.Vulkan (VulkanResources (..), DeviceContext (..), with2DImageView, ViewableImage (..), mkAcquire)
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
  , DescriptorPool, DescriptorSetLayout, DescriptorSet, Buffer
  , DescriptorBufferInfo(..)
  , pattern WHOLE_SIZE, BufferUsageFlagBits (..), MemoryPropertyFlagBits (..), DescriptorPoolCreateFlagBits (..), DescriptorSetLayoutCreateFlagBits (..), Filter
  , pattern IMAGE_ASPECT_COLOR_BIT, Sampler
  )
import Data.Functor ((<&>))
import Hickory.Vulkan.Textures (withImageSampler, withTextureImage)
import Data.Traversable (for)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Control.Lens (view)
import System.FilePath.Lens (filename)
import Data.Bits ((.|.))
import VulkanMemoryAllocator (Allocation, Allocator, withMappedMemory)
import Foreign (Storable, copyArray, castPtr, sizeOf, withArrayLen)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (bracket)
import Hickory.Vulkan.Mesh (withBuffer')
import GHC.Generics (Generic)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Hickory.Vulkan.Framing (FramedResource, resourceForFrame)
import Data.Generics.Labels ()
import Acquire.Acquire (Acquire)

type DescriptorSetBinding = (DescriptorSetLayout, FramedResource (Vector DescriptorSet))

withTextureArrayDescriptorSet :: VulkanResources -> [(ViewableImage, Sampler)] -> Acquire PointedDescriptorSet
withTextureArrayDescriptorSet VulkanResources{..} images = do
  let DeviceContext{..} = deviceContext
      numImages = fromIntegral $ length images
  descriptorSetLayout <- withDescriptorSetLayout device zero
    -- bind textures as an array of sampled images
    { bindings = [ zero
        { binding         = 0
        , descriptorCount = numImages
        , descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , stageFlags      = SHADER_STAGE_FRAGMENT_BIT
        } ]
      -- Needed for dynamic descriptor array sizing (e.g. global texture array)
    , flags = DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
    }
    Nothing mkAcquire

  descriptorPool <- withDescriptorPool device zero
    { maxSets   = 1
      -- Needed for dynamic descriptor array sizing (e.g. global texture array)
    , flags     = DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
    , poolSizes = [ DescriptorPoolSize DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER numImages ]
    }
    Nothing
    mkAcquire

  -- We use allocateDescriptorSets, rather than withDescriptorSets, b/c we
  -- free all the memory at once via the descriptorPool
  descriptorSets <- allocateDescriptorSets device zero
    { descriptorPool = descriptorPool
    , setLayouts     = [ descriptorSetLayout ]
    }

  let desImageInfos = images <&> \(ViewableImage _image imageView, sampler) -> zero
        { sampler     = sampler
        , imageView   = imageView
        , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        }
      write = zero
        { dstSet          = V.head descriptorSets
        , dstBinding      = 0
        , dstArrayElement = 0
        , descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = numImages
        , imageInfo       = V.fromList desImageInfos
        }
  updateDescriptorSets device [SomeStruct write] []

  pure PointedDescriptorSet {..}

data PointedDescriptorSet = PointedDescriptorSet
  { descriptorPool      :: DescriptorPool
  , descriptorSetLayout :: DescriptorSetLayout
  , descriptorSets      :: Vector DescriptorSet
  } deriving Generic

data TextureDescriptorSet = TextureDescriptorSet
  { descriptorSet :: PointedDescriptorSet
  , textureNames  :: Vector Text
  } deriving Generic

withTextureDescriptorSet :: VulkanResources -> [(FilePath, Filter)] -> Acquire TextureDescriptorSet
withTextureDescriptorSet _ [] = error "No textures in descriptor set"
withTextureDescriptorSet bag@VulkanResources{..} texturePaths = do
  let textureNames = V.fromList $ pack . view filename . fst <$> texturePaths
  images <- for texturePaths \(path, filt) -> do
    sampler <- withImageSampler bag filt
    image   <- withTextureImage bag path
    imageView <- with2DImageView deviceContext FORMAT_R8G8B8A8_SRGB IMAGE_ASPECT_COLOR_BIT image
    pure (ViewableImage image imageView, sampler)
  descriptorSet <- withTextureArrayDescriptorSet bag images

  pure TextureDescriptorSet {..}

data BufferDescriptorSet a = BufferDescriptorSet
  { descriptorSet :: PointedDescriptorSet
  , bufferPair    :: (Buffer, Allocation)
  , queuedData    :: IORef [a]
  , allocator     :: Allocator -- allocator used to create buffer
  } deriving (Generic)

descriptorSetBinding :: FramedResource PointedDescriptorSet -> DescriptorSetBinding
descriptorSetBinding bds = ( descriptorSetLayout (resourceForFrame (0 :: Int) bds)
                           , fmap descriptorSets bds
                           )

withBufferDescriptorSet :: forall a. Storable a => VulkanResources -> Acquire (BufferDescriptorSet a)
withBufferDescriptorSet VulkanResources{..} = do
  let DeviceContext {..} = deviceContext
  descriptorSetLayout <- withDescriptorSetLayout device zero
    { bindings = [ zero
        { binding         = 0
        , descriptorCount = 1
        , descriptorType  = DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , stageFlags      = SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT
        }
        ]
    }
    Nothing mkAcquire

  descriptorPool <- withDescriptorPool device zero
    { maxSets   = 1
    , poolSizes = [ DescriptorPoolSize DESCRIPTOR_TYPE_UNIFORM_BUFFER 1 ]
    }
    Nothing
    mkAcquire

  -- We use allocateDescriptorSets, rather than withDescriptorSets, b/c we
  -- free all the memory at once via the descriptorPool
  descriptorSets <- allocateDescriptorSets device zero
    { descriptorPool = descriptorPool
    , setLayouts     = [ descriptorSetLayout ]
    }

  (buffer, alloc, _) <- withBuffer' allocator
    BUFFER_USAGE_UNIFORM_BUFFER_BIT
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    (fromIntegral $ sizeOf (undefined :: a) * 128)

  let write = zero
        { dstSet          = V.head descriptorSets
        , dstBinding      = 0
        , dstArrayElement = 0
        , descriptorType  = DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , descriptorCount = 1
        , bufferInfo      = [ zero { buffer = buffer, offset = 0, range = WHOLE_SIZE } ]
        }
  updateDescriptorSets device [SomeStruct write] []

  let bufferPair = (buffer, alloc)
      descriptorSet = PointedDescriptorSet {..}

  queuedData <- liftIO $ newIORef []

  pure BufferDescriptorSet {..}

uploadBufferDescriptor :: (MonadIO m, Storable a) => BufferDescriptorSet a -> m ()
uploadBufferDescriptor BufferDescriptorSet {..} = liftIO do
  atomicModifyIORef' queuedData ([],) >>= \case
    [] -> pure () -- noop if there's no data to push
    as -> do
      let (_, alloc) = bufferPair

      withMappedMemory allocator alloc bracket \bptr ->
        withArrayLen (reverse as) \len dptr -> copyArray (castPtr bptr) dptr len
