{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds, DeriveGeneric, PatternSynonyms  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

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
  , DescriptorPool, DescriptorSetLayout, DescriptorSet, Buffer
  , DescriptorBufferInfo(..)
  , pattern WHOLE_SIZE, BufferUsageFlagBits (..), MemoryPropertyFlagBits (..), DescriptorPoolCreateFlagBits (..), DescriptorSetLayoutCreateFlagBits (..)
  )
import Data.Maybe (maybeToList, listToMaybe)
import Data.Functor (($>))
import Hickory.Vulkan.Textures (withImageSampler, withTextureImage)
import Data.Foldable (for_)
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
import Data.IORef (IORef, readIORef, newIORef, writeIORef)

data TextureDescriptorSet = TextureDescriptorSet
  { descriptorPool      :: DescriptorPool
  , descriptorSetLayout :: DescriptorSetLayout
  , descriptorSets      :: Vector DescriptorSet
  , textureNames        :: Vector Text -- TODO: Use Text instead of String
  } deriving (Generic)

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
      -- Needed for dynamic descriptor array sizing (e.g. global texture array)
    , flags = DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
    }
    Nothing allocate

  descriptorPool <- withDescriptorPool device zero
    { maxSets   = 1
      -- Needed for dynamic descriptor array sizing (e.g. global texture array)
    , flags     = DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
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

  let textureNames = V.fromList $ pack . view filename <$> texturePaths

  pure TextureDescriptorSet {..}

data BufferDescriptorSet a = BufferDescriptorSet
  { descriptorPool      :: DescriptorPool
  , descriptorSetLayout :: DescriptorSetLayout
  , descriptorSets      :: Vector DescriptorSet
  , bufferPair          :: (Buffer, Allocation)
  , queuedData          :: IORef [a]
  , allocator           :: Allocator -- allocator used to create buffer
  } deriving (Generic)

withBufferDescriptorSet :: forall a. Storable a => VulkanResources -> Managed (BufferDescriptorSet a)
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
    Nothing allocate

  descriptorPool <- withDescriptorPool device zero
    { maxSets   = 1
    , poolSizes = [ DescriptorPoolSize DESCRIPTOR_TYPE_UNIFORM_BUFFER 1 ]
    }
    Nothing
    allocate

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

  queuedData <- liftIO $ newIORef []

  pure BufferDescriptorSet {..}

{-
uploadBufferDescriptor :: (Storable a, MonadIO m) => BufferDescriptorSet a -> SV.Vector a -> m ()
uploadBufferDescriptor BufferDescriptorSet {..} dat = do
  let (_, alloc) = bufferPair

  liftIO $ withMappedMemory allocator alloc bracket \bptr ->
    SV.unsafeWith dat \dptr -> copyArray (castPtr bptr) dptr (SV.length dat)
    -}

uploadBufferDescriptor :: (MonadIO m, Storable a) => BufferDescriptorSet a -> m ()
uploadBufferDescriptor BufferDescriptorSet {..} = liftIO do
  as <- readIORef queuedData
  case as of
    [] -> pure () -- noop if there's no data to push
    _ -> do
      let (_, alloc) = bufferPair

      withMappedMemory allocator alloc bracket \bptr ->
        withArrayLen (reverse as) \len dptr -> copyArray (castPtr bptr) dptr len
      writeIORef queuedData []
