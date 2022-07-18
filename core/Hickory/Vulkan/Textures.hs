{-# LANGUAGE DuplicateRecordFields, DataKinds, BlockArguments, PatternSynonyms, OverloadedLists  #-}

module Hickory.Vulkan.Textures where

import Vulkan (Image, ImageCreateInfo (..), BufferUsageFlagBits (..), MemoryPropertyFlagBits (..), ImageType (..), Extent3D (..), Format (..), ImageTiling (..), SampleCountFlagBits (..), ImageUsageFlagBits (..), SharingMode (..), ImageLayout (..), ImageSubresourceRange (..), ImageMemoryBarrier (..), cmdPipelineBarrier, PipelineStageFlagBits (..), AccessFlagBits (..), pattern QUEUE_FAMILY_IGNORED, ImageAspectFlagBits (..), BufferImageCopy(..), Buffer, ImageSubresourceLayers(..), cmdCopyBufferToImage, SamplerCreateInfo(..), withSampler, Sampler, SamplerMipmapMode (..), CompareOp (..), BorderColor (..), SamplerAddressMode (..), Filter (..))
import Control.Monad.Managed (Managed, runManaged)
import Hickory.Vulkan.Vulkan (VulkanResources(..), allocate, DeviceContext (..))
import qualified Codec.Picture as Png
import Data.Word (Word8, Word32)
import qualified Data.Vector.Storable as SV
import Foreign (sizeOf, Bits ((.|.)), copyArray, castPtr)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Text.Printf (printf)
import Hickory.Vulkan.Mesh (withBuffer', withSingleTimeCommands)
import VulkanMemoryAllocator (withMappedMemory, withImage, AllocationCreateInfo(..) )
import Control.Exception (bracket)
import Vulkan.Zero (zero)
import Vulkan.CStruct.Extends (SomeStruct(..))

withTextureImage :: VulkanResources -> FilePath -> Managed Image
withTextureImage bag@VulkanResources { allocator } path = do
  Png.Image width height dat <- liftIO $ Png.readPng path >>= \case
    Left s -> error $ printf "Can't load image at path %s: %s" path s
    Right dynImage -> pure $ Png.convertRGBA8 dynImage

  let bufferSize = fromIntegral $ SV.length dat * sizeOf (undefined :: Word8)

  let imageCreateInfo :: ImageCreateInfo '[]
      imageCreateInfo = zero
        { imageType     = IMAGE_TYPE_2D
        , extent        = Extent3D (fromIntegral width) (fromIntegral height) 1
        , format        = FORMAT_R8G8B8A8_SRGB
        , mipLevels     = 1
        , arrayLayers   = 1
        , tiling        = IMAGE_TILING_OPTIMAL
        , samples       = SAMPLE_COUNT_1_BIT
        , usage         = IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT
        , sharingMode   = SHARING_MODE_EXCLUSIVE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        }
      allocationCreateInfo :: AllocationCreateInfo
      allocationCreateInfo = zero { requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }

  (image, _, _) <- withImage allocator imageCreateInfo allocationCreateInfo allocate

  liftIO $ runManaged do
    (stagingBuffer, stagingAlloc, _) <- withBuffer' allocator
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
      bufferSize

    liftIO $ withMappedMemory allocator stagingAlloc bracket \bptr ->
      SV.unsafeWith dat $ \iptr -> copyArray (castPtr bptr) iptr (SV.length dat)

    transitionImageLayout bag image IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

    copyBufferToImage bag stagingBuffer image (fromIntegral width) (fromIntegral height)

    transitionImageLayout bag image IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  pure image

copyBufferToImage :: MonadIO m => VulkanResources -> Buffer -> Image -> Word32 -> Word32 -> m ()
copyBufferToImage bag buffer image width height = withSingleTimeCommands bag \commandBuffer -> do
    let region :: BufferImageCopy
        region = zero
          { bufferOffset      = 0
          , bufferRowLength   = 0
          , bufferImageHeight = 0
          , imageSubresource = zero
            { aspectMask     = IMAGE_ASPECT_COLOR_BIT
            , mipLevel       = 0
            , baseArrayLayer = 0
            , layerCount     = 1
            }
          , imageOffset = zero
          , imageExtent = Extent3D width height 1
          }

    cmdCopyBufferToImage commandBuffer buffer image IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [region]

transitionImageLayout :: MonadIO m => VulkanResources -> Image -> ImageLayout -> ImageLayout -> m ()
transitionImageLayout bag image oldLayout newLayout = withSingleTimeCommands bag \commandBuffer -> do
  let (srcAccessMask, dstAccessMask, sourceStage, destinationStage) = case (oldLayout, newLayout) of
        (IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) ->
          ( zero
          , ACCESS_TRANSFER_WRITE_BIT
          , PIPELINE_STAGE_TOP_OF_PIPE_BIT
          , PIPELINE_STAGE_TRANSFER_BIT
          )
        (IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) ->
          ( ACCESS_TRANSFER_WRITE_BIT
          , ACCESS_SHADER_READ_BIT
          , PIPELINE_STAGE_TRANSFER_BIT
          , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
          )
        _ -> error "Unsupported image layout transition"

  let barrier :: ImageMemoryBarrier '[]
      barrier = zero
        { oldLayout = oldLayout
        , newLayout = newLayout
        , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , image = image
        , srcAccessMask = srcAccessMask
        , dstAccessMask = dstAccessMask
        , subresourceRange = subResourceRange
        }

      subResourceRange :: ImageSubresourceRange
      subResourceRange = ImageSubresourceRange
        { aspectMask     = IMAGE_ASPECT_COLOR_BIT
        , baseMipLevel   = 0
        , levelCount     = 1
        , baseArrayLayer = 0
        , layerCount     = 1
        }

  cmdPipelineBarrier commandBuffer sourceStage destinationStage zero [] [] [SomeStruct barrier]

withImageSampler :: VulkanResources -> Filter -> Managed Sampler
withImageSampler VulkanResources { deviceContext = DeviceContext {..} } filt =
  withSampler device samplerInfo Nothing allocate
  where
  samplerInfo = zero
    { magFilter = filt
    , minFilter = filt
    , addressModeU = SAMPLER_ADDRESS_MODE_REPEAT
    , addressModeV = SAMPLER_ADDRESS_MODE_REPEAT
    , addressModeW = SAMPLER_ADDRESS_MODE_REPEAT
    , anisotropyEnable = False
    , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
    , unnormalizedCoordinates = False
    , compareEnable = False
    , compareOp = COMPARE_OP_ALWAYS
    , mipmapMode = SAMPLER_MIPMAP_MODE_LINEAR
    , mipLodBias = 0.0
    , minLod = 0.0
    , maxLod = 0.0
    }
