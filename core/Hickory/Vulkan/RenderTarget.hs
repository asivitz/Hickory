{-# LANGUAGE OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, PatternSynonyms #-}

module Hickory.Vulkan.RenderTarget where

import Vulkan (Format(..), BufferImageCopy (..), ImageSubresourceLayers (..), CommandBuffer, ImageAspectFlagBits (..), Offset3D (..), ImageLayout (..), cmdCopyImageToBuffer, Extent2D (..), Extent3D (..), BufferUsageFlagBits (..), pattern QUEUE_FAMILY_IGNORED, ImageMemoryBarrier(..), ImageSubresourceRange (..), AccessFlagBits (..), PipelineStageFlagBits (..), cmdPipelineBarrier)
import Hickory.Vulkan.Types (DescriptorSpec(..), RenderTarget (..), DataBuffer (..), VulkanResources, ViewableImage (..))
import VulkanMemoryAllocator (withMappedMemory)
import Foreign (peek, plusPtr)
import Control.Exception (bracket)
import GHC.Generics (Generic)
import Hickory.Vulkan.DescriptorSet (withDataBuffer)
import GHC.Word (Word16)
import Acquire.Acquire (Acquire)
import Hickory.Math (Scalar)
import Vulkan.Zero (Zero(..))
import Vulkan.CStruct.Extends (SomeStruct(..))

withImageBuffer :: VulkanResources -> RenderTarget -> Int -> Acquire ImageBuffer
withImageBuffer vulkanResources RenderTarget {..} descIdx = do
  buffer <- withDataBuffer vulkanResources (fromIntegral $ w*h) BUFFER_USAGE_TRANSFER_DST_BIT
  pure ImageBuffer {..}
  where
  Extent2D w h = extent
  viewableImage = case descriptorSpecs !! descIdx of
    ImageDescriptor [(vi, _sampler)] -> vi
    _ -> error "Can't only copy image from image descriptor of one image"
  region = BufferImageCopy
    { bufferOffset = 0
    , bufferRowLength = 0
    , bufferImageHeight = 0
    , imageSubresource = ImageSubresourceLayers
      { aspectMask = IMAGE_ASPECT_COLOR_BIT
      , mipLevel = 0
      , baseArrayLayer = 0
      , layerCount = 1
      }
    , imageOffset = Offset3D 0 0 0
    , imageExtent = Extent3D w h 1
    }

-- Copying the whole image, so addressing x,y is (y * rowLength + x) * texelSize
copyDescriptorImageToBuffer :: CommandBuffer -> ImageBuffer -> IO ()
copyDescriptorImageToBuffer cb ImageBuffer {..} = do
  let barrier :: ImageMemoryBarrier '[]
      barrier = zero
        { oldLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        , newLayout = IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , image = img
        , srcAccessMask = ACCESS_TRANSFER_WRITE_BIT
        , dstAccessMask = ACCESS_TRANSFER_READ_BIT
        , subresourceRange = ImageSubresourceRange
          { aspectMask = IMAGE_ASPECT_COLOR_BIT
          , baseMipLevel = 0
          , levelCount = 1
          , baseArrayLayer = 0
          , layerCount = 1
          }
        }
  -- Transition image to TRANSFER layout. Might not be needed depending on usage
  cmdPipelineBarrier cb PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_TRANSFER_BIT zero [] [] [SomeStruct barrier]

  cmdCopyImageToBuffer cb img IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL buf [ region ]
  where
  DataBuffer {..} = buffer
  ViewableImage {image = img} = viewableImage

data ImageBuffer = ImageBuffer
  { buffer :: DataBuffer Word16
  , region :: BufferImageCopy
  , viewableImage :: ViewableImage
  } deriving Generic

-- This API obviously needs improvement. Shouldn't assume integer pixels.
-- (x,y) are fractions of width/height
readPixel :: ImageBuffer -> (Scalar,Scalar) -> IO Word16
readPixel ImageBuffer {buffer = DataBuffer {..}, region = BufferImageCopy {..}, viewableImage = ViewableImage {..}} (xfrac,yfrac) = do
  withMappedMemory allocator allocation bracket \bufptr ->
    peek (plusPtr bufptr address)

  where
  address :: Int = fromIntegral bufferOffset + (((z * imageHeight) + y) * rowLength + x) * texelSize
  x = round $ xfrac * realToFrac w
  y = round $ yfrac * realToFrac h
  z = 0
  Extent3D w h _ = imageExtent
  imageHeight = fromIntegral $ if bufferImageHeight == 0 then h else bufferImageHeight
  rowLength = fromIntegral $ if bufferRowLength == 0 then w else bufferRowLength
  texelSize :: Int = case format of
    FORMAT_R16_UINT -> 2
    FORMAT_R16G16B16A16_SFLOAT -> 8
    FORMAT_D32_SFLOAT -> 4
    _ -> error "Unsupported format"
