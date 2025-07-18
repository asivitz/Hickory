{-# LANGUAGE OverloadedLists, OverloadedLabels, OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, PatternSynonyms #-}

module Hickory.Vulkan.RenderTarget where

import Vulkan (Format(..), BufferImageCopy (..), ImageSubresourceLayers (..), CommandBuffer, ImageAspectFlagBits (..), Offset3D (..), ImageLayout (..), cmdCopyImageToBuffer, Extent2D (..), Extent3D (..), BufferUsageFlagBits (..), pattern QUEUE_FAMILY_IGNORED, ImageMemoryBarrier(..), ImageSubresourceRange (..), AccessFlagBits (..), PipelineStageFlagBits (..), cmdPipelineBarrier, BufferMemoryBarrier(..), pattern WHOLE_SIZE)
import Hickory.Vulkan.Types (DescriptorSpec(..), RenderConfig (..), DataBuffer (..), VulkanResources, ViewableImage (..))
import VulkanMemoryAllocator (withMappedMemory)
import Foreign (peek, plusPtr, Bits (..))
import Control.Exception (bracket)
import GHC.Generics (Generic)
import GHC.Word (Word16)
import Acquire (Acquire)
import Vulkan.Zero (Zero(..))
import Vulkan.CStruct.Extends (SomeStruct(..))
import Hickory.Vulkan.Framing (FramedResource)
import Data.Traversable (for)

-- Copying the whole image, so addressing x,y is (y * rowLength + x) * texelSize
copyDescriptorImageToBuffer :: CommandBuffer -> ImageBuffer -> IO ()
copyDescriptorImageToBuffer cb ImageBuffer {..} = do
  let imageBarrier :: ImageMemoryBarrier '[]
      imageBarrier = zero
        { oldLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        , newLayout = IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , image = img
        , srcAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        , dstAccessMask = ACCESS_TRANSFER_READ_BIT
        , subresourceRange = ImageSubresourceRange
          { aspectMask = IMAGE_ASPECT_COLOR_BIT
          , baseMipLevel = 0
          , levelCount = 1
          , baseArrayLayer = 0
          , layerCount = 1
          }
        }
      bufferBarrier :: BufferMemoryBarrier '[]
      bufferBarrier = zero
        { srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , buffer = buffer.buf
        , srcAccessMask = ACCESS_TRANSFER_WRITE_BIT
        , dstAccessMask = ACCESS_TRANSFER_WRITE_BIT
        , offset = 0
        , size = WHOLE_SIZE
        }

  cmdPipelineBarrier cb PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT PIPELINE_STAGE_TRANSFER_BIT zero [] [] [SomeStruct imageBarrier]
  cmdPipelineBarrier cb PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_TRANSFER_BIT zero [] [SomeStruct bufferBarrier] []
  cmdCopyImageToBuffer cb img IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL buf [ region ]
  where
  DataBuffer {..} = buffer
  ViewableImage {image = img} = viewableImage

data ImageBuffer = ImageBuffer
  { buffer :: DataBuffer Word16
  , region :: BufferImageCopy
  , viewableImage :: ViewableImage
  } deriving Generic
