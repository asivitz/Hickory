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
import Hickory.Vulkan.Vulkan (with2DImageView, mkAcquire)
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
  , Format (..), ImageLayout (..), allocateDescriptorSets
  , DescriptorSetLayout, DescriptorSet
  , DescriptorBufferInfo(..)
  , pattern WHOLE_SIZE, BufferUsageFlagBits (..), MemoryPropertyFlagBits (..), Filter
  , pattern IMAGE_ASPECT_COLOR_BIT, SamplerAddressMode
  )
import qualified Vulkan as Writes (WriteDescriptorSet(..))
import Data.Functor ((<&>))
import Hickory.Vulkan.Textures (withImageSampler, withTextureImage)
import Data.Traversable (for)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Control.Lens (view, _1, (&))
import System.FilePath.Lens (filename)
import Data.Bits ((.|.))
import VulkanMemoryAllocator (withMappedMemory)
import Foreign (Storable, copyArray, castPtr, sizeOf, withArrayLen, poke)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (bracket)
import Hickory.Vulkan.Mesh (withBuffer')
import GHC.Generics (Generic)
import Hickory.Vulkan.Framing (FramedResource, resourceForFrame)
import Data.Generics.Labels ()
import Acquire.Acquire (Acquire)
import Data.UUID.V4 (nextRandom)
import Hickory.Vulkan.Types (PointedDescriptorSet(..), DescriptorSpec (..), DataBuffer (..), VulkanResources (..), DeviceContext (..), ViewableImage (..))
import Data.List (group, sort)
import GHC.Word (Word32)

type DescriptorSetBinding = (DescriptorSetLayout, FramedResource DescriptorSet)

descriptorSetBindings :: [DescriptorSpec] -> [DescriptorSetLayoutBinding]
descriptorSetBindings specs = zip [0..] specs <&> \(i, spec) -> case spec of
  ImageDescriptor is -> zero
    { binding         = i
    , descriptorCount = fromIntegral $ length is
    , descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
    , stageFlags      = SHADER_STAGE_FRAGMENT_BIT
    }
  DepthImageDescriptor _ _ -> zero
    { binding         = i
    , descriptorCount = 1
    , descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
    , stageFlags      = SHADER_STAGE_FRAGMENT_BIT
    }
  BufferDescriptor _ -> zero
    { binding         = i
    , descriptorCount = 1
    , descriptorType  = DESCRIPTOR_TYPE_UNIFORM_BUFFER
    , stageFlags      = SHADER_STAGE_VERTEX_BIT .|. SHADER_STAGE_FRAGMENT_BIT
    }

withDescriptorSetLayoutFromSpecs :: VulkanResources -> [DescriptorSpec] -> Acquire DescriptorSetLayout
withDescriptorSetLayoutFromSpecs VulkanResources {..} specs = do
  let DeviceContext{..} = deviceContext
  withDescriptorSetLayout device zero
      -- bind textures as an array of sampled images
      { bindings = V.fromList $ descriptorSetBindings specs
        -- Needed for dynamic descriptor array sizing (e.g. global texture array)
      -- , flags = DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
      }
      Nothing mkAcquire

-- |Each texture is in a separately bound descriptor
withDescriptorSet :: VulkanResources -> [DescriptorSpec] -> Acquire PointedDescriptorSet
withDescriptorSet vulkanResources@VulkanResources{..} specs = do
  let DeviceContext{..} = deviceContext
      bindings' = descriptorSetBindings specs
  descriptorSetLayout <- withDescriptorSetLayoutFromSpecs vulkanResources specs

  descriptorPool <- withDescriptorPool device zero
    { maxSets   = 1
      -- Needed for dynamic descriptor array sizing (e.g. global texture array)
    -- , flags     = DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
    , poolSizes = V.fromList $ bindings' <&> \DescriptorSetLayoutBinding {..} -> DescriptorPoolSize descriptorType descriptorCount
    }
    Nothing
    mkAcquire

  -- We use allocateDescriptorSets, rather than withDescriptorSets, b/c we
  -- free all the memory at once via the descriptorPool
  descriptorSet <- V.head <$> allocateDescriptorSets device zero
    { descriptorPool = descriptorPool
    , setLayouts     = [ descriptorSetLayout ]
    }

  let writes = zip [0..] specs <&> \(i, spec) -> case spec of
        ImageDescriptor images -> zero
          { Writes.dstSet          = descriptorSet
          , Writes.dstBinding      = i
          , Writes.dstArrayElement = 0
          , Writes.descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , Writes.descriptorCount = fromIntegral $ length images
          , Writes.imageInfo       = V.fromList $ images <&> \(ViewableImage _image imageView _format, sampler) -> zero
            { sampler     = sampler
            , imageView   = imageView
            , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            }
          }
        DepthImageDescriptor (ViewableImage _image imageView _format) sampler -> zero
          { Writes.dstSet          = descriptorSet
          , Writes.dstBinding      = i
          , Writes.dstArrayElement = 0
          , Writes.descriptorType  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , Writes.descriptorCount = 1
          , Writes.imageInfo       = [zero
            { sampler     = sampler
            , imageView   = imageView
            , imageLayout = IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
            }]
          }
        BufferDescriptor buffer -> zero
          { Writes.dstSet = descriptorSet
          , Writes.dstBinding      = i
          , Writes.dstArrayElement = 0
          , Writes.descriptorType  = DESCRIPTOR_TYPE_UNIFORM_BUFFER
          , Writes.descriptorCount = 1
          , Writes.bufferInfo      = [ zero { buffer = buffer, range = WHOLE_SIZE } ]
          }
  updateDescriptorSets device (V.fromList $ SomeStruct <$> writes) []

  uuid <- liftIO nextRandom

  pure PointedDescriptorSet {..}

data TextureDescriptorSet = TextureDescriptorSet
  { descriptorSet :: PointedDescriptorSet
  , textureNames  :: Vector Text
  } deriving Generic

withTextureDescriptorSet :: VulkanResources -> [(FilePath, Filter, SamplerAddressMode)] -> Acquire TextureDescriptorSet
withTextureDescriptorSet _ [] = error "No textures in descriptor set"
withTextureDescriptorSet bag@VulkanResources{..} texturePaths = do
  let textureNames = V.fromList $ pack . view filename . view _1 <$> texturePaths
  images <- for texturePaths \(path, filt, addressMode) -> do
    sampler <- withImageSampler bag filt addressMode
    image   <- withTextureImage bag path
    let format = FORMAT_R8G8B8A8_UNORM
    imageView <- with2DImageView deviceContext format IMAGE_ASPECT_COLOR_BIT image
    pure (ViewableImage image imageView format, sampler)
  descriptorSet <- withDescriptorSet bag [ImageDescriptor images]

  pure TextureDescriptorSet {..}

data BufferDescriptorSet a = BufferDescriptorSet
  { descriptorSet :: PointedDescriptorSet
  , dataBuffer    :: DataBuffer a
  , size          :: Int
  } deriving (Generic)

descriptorSetBinding :: FramedResource PointedDescriptorSet -> DescriptorSetBinding
descriptorSetBinding bds = ( descriptorSetLayout (resourceForFrame (0 :: Word32) bds)
                           , fmap (view #descriptorSet) bds
                           )

withDataBuffer :: forall a. Storable a => VulkanResources -> Int -> BufferUsageFlagBits -> Acquire (DataBuffer a)
withDataBuffer VulkanResources {..} num usageBits = do
  (buf, allocation, _) <- withBuffer' allocator
    usageBits
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    (fromIntegral $ sizeOf (undefined :: a) * num)
  pure DataBuffer {..}

withBufferDescriptorSet :: forall a. Storable a => VulkanResources -> Int -> Acquire (BufferDescriptorSet a)
withBufferDescriptorSet vulkanResources size = do
  dataBuffer <- withDataBuffer vulkanResources size BUFFER_USAGE_UNIFORM_BUFFER_BIT

  ds <-  withDescriptorSet vulkanResources [BufferDescriptor (buf dataBuffer)]

  pure BufferDescriptorSet {descriptorSet = ds, ..}

{-
uploadBufferDescriptor' :: (MonadIO m, Storable a) => BufferDescriptorSet a -> m ()
uploadBufferDescriptor' BufferDescriptorSet {..} = liftIO do
  atomicModifyIORef' queuedData ([],) >>= \case
    [] -> pure () -- noop if there's no data to push
    as -> do
      let (_, alloc) = bufferPair

      withMappedMemory allocator alloc bracket \bptr ->
        withArrayLen (reverse as) \len dptr -> copyArray (castPtr bptr) dptr len
        -}

uploadBufferDescriptorArray :: (MonadIO m, Storable a) => DataBuffer a -> [a] -> m ()
uploadBufferDescriptorArray DataBuffer {..} as = liftIO do
  withMappedMemory allocator allocation bracket \bptr ->
    withArrayLen as \len dptr -> copyArray (castPtr bptr) dptr len

uploadBufferDescriptor :: (MonadIO m, Storable a) => DataBuffer a -> a -> m ()
uploadBufferDescriptor DataBuffer {..} a = liftIO do
  withMappedMemory allocator allocation bracket \bufptr ->
    poke (castPtr bufptr) a
