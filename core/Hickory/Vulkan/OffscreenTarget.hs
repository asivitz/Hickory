{-# LANGUAGE DeriveGeneric #-}

module Hickory.Vulkan.OffscreenTarget where

import Hickory.Vulkan.Vulkan (VulkanResources (..), Swapchain, ViewableImage (..), with2DImageView, imageFormat)
import Control.Monad.Managed (Managed)
import Hickory.Vulkan.DescriptorSet (withTextureArrayDescriptorSet, PointedDescriptorSet)
import Vulkan (Filter (..), Format (..), ImageAspectFlagBits (..), ImageUsageFlagBits (..)
  , SurfaceFormatKHR(..), Sampler)
import Hickory.Vulkan.Textures (withImageSampler, withIntermediateImage)
import GHC.Generics (Generic)

data OffscreenTarget = OffscreenTarget
  { colorImage    :: ViewableImage
  , depthImage    :: ViewableImage
  , descriptorSet :: PointedDescriptorSet
  , sampler       :: Sampler
  } deriving Generic

withOffscreenTarget :: VulkanResources -> Swapchain -> (Int,Int) -> Managed OffscreenTarget
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
