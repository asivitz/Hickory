module Hickory.Vulkan.LUT where

import Codec.Picture
import Hickory.Vulkan.Types (ViewableImage(..), DescriptorSpec (..), VulkanResources (..), PointedDescriptorSet)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet)
import Hickory.Vulkan.Vulkan (with2DImageViewMips)
import Hickory.Vulkan.Textures (withImageSamplerMips, withImageFromArray)
import Vulkan (Filter(..), SamplerAddressMode (..), SamplerMipmapMode (..), ImageViewType (..), Format (..), ImageAspectFlagBits (..), Extent3D (..), ImageType (..))
import Data.Bits (zeroBits)
import Acquire (Acquire)

-- Generate a base LUT image
generateBaseLUT :: Image PixelRGBA8
generateBaseLUT = generateImage pixelFunc 32 1024
  where
    pixelFunc :: Int -> Int -> PixelRGBA8
    pixelFunc x y = PixelRGBA8 r g b 255
      where
        -- blue component (block index)
        blockY = y `div` 32
        b = fromIntegral $ round $ fromIntegral blockY / 31 * 255

        -- the red component (x position)
        r = fromIntegral $ round $ fromIntegral x / 31 * 255

        -- the green component (within-block y)
        inBlockY = y `mod` 32
        g = fromIntegral $ round $ fromIntegral inBlockY / 31 * 255

-- Might be easier to have a square image (duplicate luts repeated side by side), for use in some color grading softwares
generateBaseLUTSquare :: Image PixelRGBA8
generateBaseLUTSquare = generateImage pixelFunc 1024 1024
  where
    pixelFunc :: Int -> Int -> PixelRGBA8
    pixelFunc x y = PixelRGBA8 r g b 255
      where
        -- blue component (block index)
        blockY = y `div` 32
        b = fromIntegral $ round $ fromIntegral blockY / 31 * 255

        -- the red component (x position)
        inBlockX = x `mod` 32
        r = fromIntegral $ round $ fromIntegral inBlockX / 31 * 255

        -- the green component (within-block y)
        inBlockY = y `mod` 32
        g = fromIntegral $ round $ fromIntegral inBlockY / 31 * 255

withBaseLUT :: VulkanResources -> Acquire DescriptorSpec
withBaseLUT vulkanResources = do
  let Image width height dat = generateBaseLUT
      extent = Extent3D (fromIntegral width) (fromIntegral width) (fromIntegral width)
  (image, _) <- withImageFromArray vulkanResources extent IMAGE_TYPE_3D FORMAT_R8G8B8A8_UNORM False 1 zeroBits dat

  sampler <- withImageSamplerMips vulkanResources 1 FILTER_LINEAR SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE SAMPLER_MIPMAP_MODE_LINEAR
  let viewType = IMAGE_VIEW_TYPE_3D
      format   = FORMAT_R8G8B8A8_UNORM
      layers = 1

  imageView <- with2DImageViewMips deviceContext format IMAGE_ASPECT_COLOR_BIT image 1 viewType 0 layers
  let viewableImage = ViewableImage image imageView format
  pure $ ImageDescriptor [(viewableImage, sampler)]
  where
  VulkanResources {..} = vulkanResources

writeBaseLUT :: FilePath -> IO ()
writeBaseLUT path = savePngImage path (ImageRGBA8 generateBaseLUT)

writeBaseLUTSquare :: FilePath -> IO ()
writeBaseLUTSquare path = savePngImage path (ImageRGBA8 generateBaseLUTSquare)
