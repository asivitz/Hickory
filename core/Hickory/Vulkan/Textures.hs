{-# LANGUAGE DuplicateRecordFields, DataKinds, BlockArguments, PatternSynonyms, OverloadedLists  #-}

module Hickory.Vulkan.Textures where

import Vulkan
  ( Extent2D(..)
  , Image, ImageCreateInfo (..), BufferUsageFlagBits (..), MemoryPropertyFlagBits (..), ImageType (..), Extent3D (..), Format (..), ImageTiling (..), SampleCountFlagBits (..), ImageUsageFlagBits (..), SharingMode (..), ImageLayout (..), ImageSubresourceRange (..), ImageMemoryBarrier (..), cmdPipelineBarrier, PipelineStageFlagBits (..), AccessFlagBits (..), pattern QUEUE_FAMILY_IGNORED, ImageAspectFlagBits (..), BufferImageCopy(..), Buffer, ImageSubresourceLayers(..), cmdCopyBufferToImage, SamplerCreateInfo(..), withSampler, Sampler, SamplerMipmapMode (..), CompareOp (..), BorderColor (..), SamplerAddressMode (..), Filter (..), CommandBuffer, reductionMode, SamplerReductionMode (..)
  )
import Hickory.Vulkan.Vulkan (runAcquire, mkAcquire)
import qualified Codec.Picture as Png
import qualified Codec.Picture.Extra as Png
import Data.Word (Word8, Word32)
import qualified Data.Vector.Storable as SV
import Foreign (sizeOf, Bits ((.|.)), copyArray, castPtr, Storable)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Text.Printf (printf)
import Hickory.Vulkan.Mesh (withBuffer', withSingleTimeCommands)
import VulkanMemoryAllocator (withMappedMemory, withImage, AllocationCreateInfo(..) )
import Control.Exception (bracket)
import Vulkan.Zero (zero)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Acquire.Acquire (Acquire)
import Hickory.Vulkan.Types (VulkanResources(..), DeviceContext (..))
import Data.Foldable (for_)

withImageFromArray :: forall a. Storable a => VulkanResources -> Int -> Int -> Format -> SV.Vector a -> Acquire Image
withImageFromArray vr w h format dat = withImageFromArrayMips vr w h format [dat]

withImageFromArrayMips :: forall a. Storable a => VulkanResources -> Int -> Int -> Format -> [SV.Vector a] -> Acquire Image
withImageFromArrayMips bag@VulkanResources { allocator } width height format mips = do

  let mipLevels = fromIntegral $ length mips
      imageCreateInfo :: ImageCreateInfo '[]
      imageCreateInfo = zero
        { imageType     = IMAGE_TYPE_2D
        , extent        = Extent3D (fromIntegral width) (fromIntegral height) 1
        , format        = format
        , mipLevels     = mipLevels
        , arrayLayers   = 1
        , tiling        = IMAGE_TILING_OPTIMAL
        , samples       = SAMPLE_COUNT_1_BIT
        , usage         = IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT
        , sharingMode   = SHARING_MODE_EXCLUSIVE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        }
      allocationCreateInfo :: AllocationCreateInfo
      allocationCreateInfo = zero { requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }

  (image, _, _) <- withImage allocator imageCreateInfo allocationCreateInfo mkAcquire

  liftIO $ runAcquire do
    withSingleTimeCommands bag $ transitionImageLayoutMips image mipLevels IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

    let wpot = logBase 2 (realToFrac width)
        hpot = logBase 2 (realToFrac height)
    for_ (zip [0..] mips) \(mipLevel :: Word32, dat) -> do
      let bufferSize = fromIntegral $ SV.length dat * sizeOf (undefined :: a)
          w' :: Float = 2 ** (wpot - realToFrac mipLevel)
          h' :: Float = 2 ** (hpot - realToFrac mipLevel)
      (stagingBuffer, stagingAlloc, _) <- withBuffer' allocator
        BUFFER_USAGE_TRANSFER_SRC_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
        bufferSize

      liftIO $ withMappedMemory allocator stagingAlloc bracket \bptr ->
        SV.unsafeWith dat $ \iptr -> copyArray (castPtr bptr) iptr (SV.length dat)

      copyBufferToImage bag stagingBuffer image mipLevel (round w') (round h')

    withSingleTimeCommands bag $ transitionImageLayoutMips image mipLevels IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  pure image

withTextureImage :: VulkanResources -> FilePath -> Acquire Image
withTextureImage bag path = do
  Png.Image width height dat <- liftIO $ Png.readPng path >>= \case
    Left s -> error $ printf "Can't load image at path %s: %s" path s
    Right dynImage -> pure . Png.flipVertically $ Png.convertRGBA8 dynImage

  withImageFromArray bag width height FORMAT_R8G8B8A8_UNORM dat

copyBufferToImage :: MonadIO m => VulkanResources -> Buffer -> Image -> Word32 -> Word32 -> Word32 -> m ()
copyBufferToImage bag buffer image mipLevel width height = withSingleTimeCommands bag \commandBuffer -> do
    let region :: BufferImageCopy
        region = zero
          { bufferOffset      = 0
          , bufferRowLength   = 0
          , bufferImageHeight = 0
          , imageSubresource = zero
            { aspectMask     = IMAGE_ASPECT_COLOR_BIT
            , mipLevel       = mipLevel
            , baseArrayLayer = 0
            , layerCount     = 1
            }
          , imageOffset = zero
          , imageExtent = Extent3D width height 1
          }

    cmdCopyBufferToImage commandBuffer buffer image IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [region]

transitionImageLayout :: MonadIO m => Image -> ImageLayout -> ImageLayout -> CommandBuffer -> m ()
transitionImageLayout image = transitionImageLayoutMips image 1

transitionImageLayoutMips :: MonadIO m => Image -> Word32 -> ImageLayout -> ImageLayout -> CommandBuffer -> m ()
transitionImageLayoutMips image mipLevels oldLayout newLayout commandBuffer = do
  let (srcAccessMask, dstAccessMask, sourceStage, destinationStage, aspectMask) = case (oldLayout, newLayout) of
        (IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) ->
          ( zero
          , ACCESS_TRANSFER_WRITE_BIT
          , PIPELINE_STAGE_TOP_OF_PIPE_BIT
          , PIPELINE_STAGE_TRANSFER_BIT
          , IMAGE_ASPECT_COLOR_BIT
          )
        (IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) ->
          ( ACCESS_TRANSFER_WRITE_BIT
          , ACCESS_SHADER_READ_BIT
          , PIPELINE_STAGE_TRANSFER_BIT
          , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
          , IMAGE_ASPECT_COLOR_BIT
          )
        (IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_PRESENT_SRC_KHR) ->
          ( ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          , zero
          , PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
          , IMAGE_ASPECT_COLOR_BIT
          )
        (IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL) ->
          ( zero
          , ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          , PIPELINE_STAGE_TOP_OF_PIPE_BIT
          , PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , IMAGE_ASPECT_COLOR_BIT
          )
        (IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL) ->
          ( zero
          , ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
          , PIPELINE_STAGE_TOP_OF_PIPE_BIT
          , PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
          , IMAGE_ASPECT_DEPTH_BIT
          )
        (IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) ->
          ( ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          , ACCESS_SHADER_READ_BIT
          , PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
          , IMAGE_ASPECT_COLOR_BIT
          )
        (IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) ->
          ( ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
          , ACCESS_SHADER_READ_BIT
          , PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
          , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
          , IMAGE_ASPECT_DEPTH_BIT
          )
        (IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL, IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL ) ->
          ( ACCESS_SHADER_READ_BIT
          , ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
          , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
          , PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
          , IMAGE_ASPECT_DEPTH_BIT
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
        { aspectMask     = aspectMask
        , baseMipLevel   = 0
        , levelCount     = mipLevels
        , baseArrayLayer = 0
        , layerCount     = 1
        }

  cmdPipelineBarrier commandBuffer sourceStage destinationStage zero [] [] [SomeStruct barrier]

withImageSampler :: VulkanResources -> Filter -> SamplerAddressMode -> Acquire Sampler
withImageSampler vr = withImageSamplerMips vr 0

withImageSamplerMips :: VulkanResources -> Word32 -> Filter -> SamplerAddressMode -> Acquire Sampler
withImageSamplerMips VulkanResources { deviceContext = DeviceContext {..} } mipLevels filt addressMode =
  withSampler device samplerInfo Nothing mkAcquire
  where
  samplerInfo = zero
    { magFilter = filt
    , minFilter = filt
    , addressModeU = addressMode
    , addressModeV = addressMode
    , addressModeW = addressMode
    , anisotropyEnable = False
    , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
    , unnormalizedCoordinates = False
    , compareEnable = True
    , compareOp = COMPARE_OP_ALWAYS
    , mipmapMode = SAMPLER_MIPMAP_MODE_LINEAR
    , mipLodBias = 0.0
    , minLod = 0.0
    , maxLod = realToFrac mipLevels
    }

withShadowSampler :: VulkanResources -> Acquire Sampler
withShadowSampler VulkanResources { deviceContext = DeviceContext {..} } =
  withSampler device samplerInfo Nothing mkAcquire
  where
  samplerInfo = zero
    { magFilter = FILTER_LINEAR
    , minFilter = FILTER_LINEAR
    , addressModeU = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
    , addressModeV = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
    , addressModeW = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
    , anisotropyEnable = False
    , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
    , unnormalizedCoordinates = False
    , compareEnable = True
    , compareOp = COMPARE_OP_LESS
    , mipmapMode = SAMPLER_MIPMAP_MODE_LINEAR
    , mipLodBias = 0.0
    , minLod = 0.0
    , maxLod = 0.0
    }

withIntermediateImage :: VulkanResources -> Format -> ImageUsageFlagBits -> Extent2D -> SampleCountFlagBits -> Acquire Image
withIntermediateImage VulkanResources { allocator } format usage (Extent2D width height) samples = do
  let imageCreateInfo :: ImageCreateInfo '[]
      imageCreateInfo = zero
        { imageType     = IMAGE_TYPE_2D
        , extent        = Extent3D (fromIntegral width) (fromIntegral height) 1
        , format        = format
        , mipLevels     = 1
        , arrayLayers   = 1
        , tiling        = IMAGE_TILING_OPTIMAL
        , samples       = samples
        , usage         = usage .|. IMAGE_USAGE_SAMPLED_BIT
        , sharingMode   = SHARING_MODE_EXCLUSIVE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        }
      allocationCreateInfo :: AllocationCreateInfo
      allocationCreateInfo = zero { requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }

  (image, _, _) <- withImage allocator imageCreateInfo allocationCreateInfo mkAcquire
  pure image
