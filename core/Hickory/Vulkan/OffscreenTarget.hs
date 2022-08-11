{-# LANGUAGE DeriveGeneric, OverloadedLabels #-}

module Hickory.Vulkan.OffscreenTarget where

import Hickory.Vulkan.Vulkan
    ( VulkanResources(..),
      Swapchain,
      ViewableImage(..),
      with2DImageView,
      imageFormat,
      VulkanResources(..),
      Swapchain(..),
      ViewableImage(..) )
import Hickory.Vulkan.DescriptorSet (withTextureArrayDescriptorSet, PointedDescriptorSet)
import Vulkan
    ( Filter(..),
      Format(..),
      ImageAspectFlagBits(..),
      ImageUsageFlagBits(..),
      SurfaceFormatKHR(..),
      Sampler,
      ImageLayout(..) )
import Hickory.Vulkan.Textures
    ( withImageSampler, withIntermediateImage, transitionImageLayout )
import GHC.Generics (Generic)

import Control.Lens (view)
import Control.Monad.Managed

import Linear (V4(..))
import Data.Generics.Labels ()
import Hickory.Vulkan.Framing (FramedResource, resourceForFrame)
import Control.Arrow ((&&&))
import Hickory.Vulkan.Monad (FrameMonad (..))
import Hickory.Vulkan.Frame (FrameContext (..), useDynamicRenderPass)
import Acquire.Acquire (Acquire)

data OffscreenTarget = OffscreenTarget
  { colorImage    :: ViewableImage
  , depthImage    :: ViewableImage
  , descriptorSet :: PointedDescriptorSet
  , sampler       :: Sampler
  } deriving Generic

withOffscreenTarget :: VulkanResources -> Swapchain -> (Int,Int) -> Acquire OffscreenTarget
withOffscreenTarget vulkanResources@VulkanResources{..} swapchain fbSize = do
  let colorFormat = format (imageFormat swapchain) -- Don't _have_ to use the swapchain format, but currently our pipelines automatically use that format
      depthFormat = FORMAT_D32_SFLOAT

  offscreenColorImage     <- withIntermediateImage vulkanResources colorFormat IMAGE_USAGE_COLOR_ATTACHMENT_BIT fbSize
  offscreenColorImageView <- with2DImageView deviceContext colorFormat IMAGE_ASPECT_COLOR_BIT offscreenColorImage
  offscreenDepthImage     <- withIntermediateImage vulkanResources depthFormat IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT fbSize
  offscreenDepthImageView <- with2DImageView deviceContext depthFormat IMAGE_ASPECT_DEPTH_BIT offscreenDepthImage

  sampler <- withImageSampler vulkanResources FILTER_LINEAR

  let colorImage = ViewableImage offscreenColorImage offscreenColorImageView
      depthImage = ViewableImage offscreenDepthImage offscreenDepthImageView

  descriptorSet <- withTextureArrayDescriptorSet vulkanResources
    [ (colorImage, sampler)
    , (depthImage, sampler)
    ]
  pure OffscreenTarget {..}

renderToSwapchain :: (FrameMonad m, MonadIO m) => V4 Float -> m () -> m ()
renderToSwapchain clearColor f = do
  FrameContext {..} <- askFrameContext
  transitionImageLayout (view #image colorImage) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL commandBuffer
  transitionImageLayout (view #image depthImage) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL commandBuffer

  useDynamicRenderPass commandBuffer extent clearColor colorImage depthImage f

  transitionImageLayout (view #image colorImage) IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL IMAGE_LAYOUT_PRESENT_SRC_KHR commandBuffer

renderOffscreen :: (FrameMonad m, MonadIO m) => V4 Float -> FramedResource OffscreenTarget -> m () -> m ()
renderOffscreen clearColor offscreenTarget f = do
  FrameContext { extent, commandBuffer, frameNumber } <- askFrameContext
  let (offscreenColor, offscreenDepth) = (view #colorImage &&& view #depthImage) (resourceForFrame frameNumber offscreenTarget)

  -- prepare offscreen images for rendering
  transitionImageLayout (view #image offscreenColor) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL commandBuffer
  transitionImageLayout (view #image offscreenDepth) IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL commandBuffer

  useDynamicRenderPass commandBuffer extent clearColor offscreenColor offscreenDepth f

  -- prepare offscreen image for use as shader input
  transitionImageLayout (view #image offscreenColor) IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL commandBuffer
  transitionImageLayout (view #image offscreenDepth) IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL commandBuffer
