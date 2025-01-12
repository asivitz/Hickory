{-# LANGUAGE OverloadedLists #-}

module Hickory.Vulkan.Renderer.Screenshot where

import Vulkan (Format (..), Extent2D, PipelineRenderingCreateInfo (..), SampleCountFlagBits (..))
import Acquire (Acquire)
import Hickory.Vulkan.Types (RenderConfig (..), FrameContext (..))
import Hickory.Vulkan.Renderer.Renderer (renderToRenderer)

linearFormat :: Format
linearFormat = FORMAT_R16G16B16A16_SFLOAT

sRGBFormat :: Format
sRGBFormat = FORMAT_R8G8B8A8_UNORM

withScreenshotRenderConfig :: Format -> Extent2D -> Acquire RenderConfig
withScreenshotRenderConfig format extent = do
  let samples = SAMPLE_COUNT_1_BIT
      renderPassInfo = Right PipelineRenderingCreateInfo
        { colorAttachmentFormats = [format]
        , depthAttachmentFormat = FORMAT_UNDEFINED
        , stencilAttachmentFormat = FORMAT_UNDEFINED
        , viewMask = 0
        }
  pure RenderConfig {..}

renderLinearScreenshot renderer extent renderSettings litF overlayF filePath = do
  renderConfig <- withScreenshotRenderConfig linearFormat extent
  let frameContext = FrameContext {..}

  renderToRenderer frameContext renderer renderSettings litF overlayF
