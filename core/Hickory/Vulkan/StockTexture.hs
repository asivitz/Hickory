module Hickory.Vulkan.StockTexture where
import qualified Data.Vector.Storable as SV
import Hickory.Vulkan.Types (VulkanResources (..), DescriptorSpec (..), ViewableImage (..))
import Acquire (Acquire)
import Vulkan (SamplerAddressMode (..), Filter (..), ImageAspectFlagBits (..), Format (..), SamplerMipmapMode (..), ImageViewType (..), Extent3D (..), ImageType (..), ImageCreateFlagBits (..))
import Hickory.Vulkan.Textures (withImageFromArray, withImageSampler)
import Hickory.Vulkan.Vulkan (with2DImageView)
import Codec.Picture (Pixel(..), PixelRGBA8)
import Data.Bits (Bits(..))

withWhiteImageDescriptor :: VulkanResources -> Acquire DescriptorSpec
withWhiteImageDescriptor vr@VulkanResources {..} = do
  let format = FORMAT_R8G8B8A8_UNORM
  image <- fst <$> withImageFromArray vr (Extent3D 1 1 1) IMAGE_TYPE_2D format False 1 zeroBits (SV.fromList [255 :: PixelBaseComponent PixelRGBA8, 255, 255, 255])
  sampler <- withImageSampler vr FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE SAMPLER_MIPMAP_MODE_LINEAR
  imageView <- with2DImageView deviceContext format IMAGE_ASPECT_COLOR_BIT image IMAGE_VIEW_TYPE_2D 0 1

  pure $ ImageDescriptor [(ViewableImage image imageView format, sampler)]

withWhiteCubeImageDescriptor :: VulkanResources -> Acquire DescriptorSpec
withWhiteCubeImageDescriptor vr@VulkanResources {..} = do
  let format = FORMAT_R8G8B8A8_UNORM
  image <- fst <$> withImageFromArray vr (Extent3D 1 1 1) IMAGE_TYPE_2D format False 6 IMAGE_CREATE_CUBE_COMPATIBLE_BIT
    (SV.fromList [ 255 :: PixelBaseComponent PixelRGBA8, 255, 255, 255
                 , 255, 255, 255, 255
                 , 255, 255, 255, 255
                 , 255, 255, 255, 255
                 , 255, 255, 255, 255
                 , 255, 255, 255, 255
                 ])
  sampler <- withImageSampler vr FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE SAMPLER_MIPMAP_MODE_LINEAR
  imageView <- with2DImageView deviceContext format IMAGE_ASPECT_COLOR_BIT image IMAGE_VIEW_TYPE_CUBE 0 6

  pure $ ImageDescriptor [(ViewableImage image imageView format, sampler)]
