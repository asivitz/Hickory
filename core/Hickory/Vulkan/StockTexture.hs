module Hickory.Vulkan.StockTexture where
import qualified Data.Vector.Storable as SV
import Hickory.Vulkan.Types (VulkanResources (..), DescriptorSpec (..), ViewableImage (..))
import Acquire.Acquire (Acquire)
import Vulkan (SamplerAddressMode (..), Filter (..), ImageAspectFlagBits (..), Format (..))
import Hickory.Vulkan.Textures (withImageFromArray, withImageSampler)
import Hickory.Vulkan.Vulkan (with2DImageView)

withWhiteImageDescriptor :: VulkanResources -> Acquire DescriptorSpec
withWhiteImageDescriptor vr@VulkanResources {..} = do
  let format = FORMAT_R8G8B8A8_UNORM
  image <- withImageFromArray vr 1 1 (SV.fromList [255, 255, 255, 255])
  sampler <- withImageSampler vr FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  imageView <- with2DImageView deviceContext format IMAGE_ASPECT_COLOR_BIT image

  pure $ ImageDescriptor [(ViewableImage image imageView format, sampler)]
