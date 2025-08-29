{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot, DataKinds, BlockArguments, PatternSynonyms, OverloadedLists  #-}
{-# LANGUAGE TypeFamilies, OverloadedLabels #-}
{-# LANGUAGE MultiWayIf #-}

module Hickory.Vulkan.Textures where

import Vulkan
  ( Extent2D(..)
  , Image, ImageCreateInfo (..), BufferUsageFlagBits (..), MemoryPropertyFlagBits (..), ImageType (..), Extent3D (..), Format (..), ImageTiling (..), SampleCountFlagBits (..), ImageUsageFlagBits (..), SharingMode (..), ImageLayout (..), ImageSubresourceRange (..), ImageMemoryBarrier (..), cmdPipelineBarrier, PipelineStageFlagBits (..), AccessFlagBits (..), pattern QUEUE_FAMILY_IGNORED, ImageAspectFlagBits (..), BufferImageCopy(..), Buffer, ImageSubresourceLayers(..), cmdCopyBufferToImage, SamplerCreateInfo(..), withSampler, Sampler, SamplerMipmapMode (..), CompareOp (..), BorderColor (..), SamplerAddressMode (..), Filter (..), CommandBuffer, ImageBlit (..), getPhysicalDeviceFormatProperties, FormatProperties(..), FormatFeatureFlagBits (..), Offset3D (..), cmdBlitImage
  , PhysicalDeviceProperties(..), PhysicalDeviceLimits(..), ImageCreateFlagBits (..), AccessFlags, PipelineStageFlags, ImageAspectFlags, ImageViewType (..), cmdCopyImageToBuffer
  )
import Hickory.Vulkan.Vulkan (runAcquire, mkAcquire, with2DImageViewMips)
import qualified Codec.Picture as Picture
import qualified Codec.Picture.Extra as Picture
import Data.Word (Word32)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import Foreign (sizeOf, Bits ((.|.), zeroBits), copyArray, castPtr, Storable, Ptr, plusPtr, shiftR, alignPtr, ptrToWordPtr, WordPtr (..), wordPtrToPtr, IntPtr (..), ptrToIntPtr)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Text.Printf (printf)
import Hickory.Vulkan.Mesh (withBuffer', withSingleTimeCommands)
import VulkanMemoryAllocator (withMappedMemory, withImage, AllocationCreateInfo(..) )
import Control.Exception (bracket)
import Vulkan.Zero (zero)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Acquire (Acquire)
import Hickory.Vulkan.Types (VulkanResources(..), DeviceContext (..), TextureLoadOptions(..), ImageType (..), formatForImageType, ConversionTo3D (..), ViewableImage (..))
import Data.Foldable (for_)
import Vulkan.Utils.Misc ((.&&.))
import Control.Monad (unless)
import Data.Bool (bool)
import qualified Data.ByteString as BS
import Control.Lens (_1, _2, _3, preview, toListOf, each)
import Data.Traversable (for)
import System.FilePath (dropExtension)
import Data.Maybe (fromMaybe)
import Hickory.Vulkan.HDR (PixelRGBAF (..))

import qualified Codec.Ktx2.Read as KTX
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Codec.Ktx2.Level as KTX
import qualified Codec.Ktx2.Header as KTX
import qualified Codec.Ktx2.Write as KTX
import qualified Codec.Ktx2.DFD.Khronos.BasicV2 as KTX

withImageFromArray :: forall a. Storable a => VulkanResources -> Extent3D -> Vulkan.ImageType -> Format -> Bool -> Word32 -> ImageCreateFlagBits -> SV.Vector a -> Acquire (Image, Word32)
withImageFromArray = withImageFromArrayGeneratedMips

withImageFromArrayGeneratedMips
  :: forall a. Storable a
  => VulkanResources
  -> Extent3D
  -> Vulkan.ImageType
  -> Format
  -> Bool
  -> Word32
  -> ImageCreateFlagBits
  -> SV.Vector a
  -> Acquire (Image, Word32)
withImageFromArrayGeneratedMips bag@VulkanResources { allocator } extent imageType format shouldGenerateMips arrayLayers imageFlags imageDat = do
  let mipLevels = if shouldGenerateMips then floor (logBase 2 (realToFrac $ max extent.width extent.height)) + 1 else 1

  let imageCreateInfo :: ImageCreateInfo '[]
      imageCreateInfo = zero
        { imageType
        , extent        = extent
        , format        = format
        , mipLevels     = mipLevels
        , arrayLayers   = arrayLayers
        , tiling        = IMAGE_TILING_OPTIMAL
        , samples       = SAMPLE_COUNT_1_BIT
        , usage         = IMAGE_USAGE_TRANSFER_SRC_BIT .|. IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT
        , sharingMode   = SHARING_MODE_EXCLUSIVE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        , flags         = imageFlags
        }
      allocationCreateInfo :: AllocationCreateInfo
      allocationCreateInfo = zero { requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }

  (image, _, _) <- withImage allocator imageCreateInfo allocationCreateInfo mkAcquire

  liftIO $ runAcquire do
    withSingleTimeCommands bag $ transitionImageLayoutMips image mipLevels arrayLayers IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

    let bufferSize = fromIntegral $ SV.length imageDat * sizeOf (undefined :: a)
    (stagingBuffer, stagingAlloc, _) <- withBuffer' allocator
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
      bufferSize

    liftIO $ withMappedMemory allocator stagingAlloc bracket \bptr ->
      SV.unsafeWith imageDat $ \iptr -> copyArray (castPtr bptr) iptr (SV.length imageDat)

    copyBufferToImage bag stagingBuffer image 0 arrayLayers extent

    if shouldGenerateMips
    then liftIO $ generateMipmaps bag image format extent.width extent.height mipLevels
    else withSingleTimeCommands bag $ transitionImageLayoutMips image mipLevels arrayLayers IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  pure (image, mipLevels)

withImageFromArrayCustomMips :: forall a. Storable a => VulkanResources -> Int -> Int -> Format -> [SV.Vector a] -> Acquire Image
withImageFromArrayCustomMips bag@VulkanResources { allocator } width height format mips = do

  let mipLevels = fromIntegral $ length mips
      arrayLayers = 1
      imageCreateInfo :: ImageCreateInfo '[]
      imageCreateInfo = zero
        { imageType     = IMAGE_TYPE_2D
        , extent        = Extent3D (fromIntegral width) (fromIntegral height) 1
        , format        = format
        , mipLevels     = mipLevels
        , arrayLayers   = arrayLayers
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
    withSingleTimeCommands bag $ transitionImageLayoutMips image mipLevels arrayLayers IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

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

      let extent' = Extent3D (round w') (round h') 1
      copyBufferToImage bag stagingBuffer image mipLevel arrayLayers extent'

    withSingleTimeCommands bag $ transitionImageLayoutMips image mipLevels arrayLayers IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  pure image

horizontalSlicesToVertical :: Picture.Pixel a => Picture.Image a -> Picture.Image a
horizontalSlicesToVertical img =
  if | width /= height * height -> error ("Image data must be a cube laid out horizontally")
     | otherwise -> Picture.generateImage genPixel height (height * height)
  where
  height = Picture.imageHeight img
  width = Picture.imageWidth img
  genPixel nx ny =
    let (slice, yInSlice) = ny `divMod` height
        xInSlice          = nx
        ox = slice * height + xInSlice
        oy = yInSlice
    in Picture.pixelAt img ox oy

withTextureImage :: VulkanResources -> Bool -> TextureLoadOptions -> FilePath -> Acquire (Image, Word32)
withTextureImage bag shouldGenerateMips options path = do
  let arrayLayers = if options.isCubemap then 6 else 1
      flags = if options.isCubemap then IMAGE_CREATE_CUBE_COMPATIBLE_BIT else zeroBits
  case options.fileType of
    PNG -> do
      liftIO (Picture.readPng path) >>= \case
        Left s -> error $ printf "Can't load image at path %s: %s" path s
        Right dynImage -> do
          let xform = case options.conversionTo3D of
                HorizontalSlices -> horizontalSlicesToVertical
                _ -> id
              Picture.Image width height dat = xform . (bool id Picture.flipVertically options.shouldFlipVertically) $ Picture.convertRGBA8 dynImage
              extent = case options.conversionTo3D of
                Simply2D -> Extent3D (fromIntegral width) (fromIntegral height) 1
                VerticalSlices   -> Extent3D (fromIntegral width) (fromIntegral width) (fromIntegral width)
                HorizontalSlices -> Extent3D (fromIntegral width) (fromIntegral width) (fromIntegral width)
              vit = case options.conversionTo3D of
                Simply2D         -> IMAGE_TYPE_2D
                VerticalSlices   -> IMAGE_TYPE_3D
                HorizontalSlices -> IMAGE_TYPE_3D
          withImageFromArray bag extent vit (formatForImageType options.fileType) shouldGenerateMips arrayLayers flags dat
    HDR -> do
      (width, height, dat :: SV.Vector Float) <-
        if options.isCubemap
        then do
          let base = dropExtension path
              allPaths = [ base <> "_posx.hdr"
                         , base <> "_negx.hdr"
                         , base <> "_posy.hdr"
                         , base <> "_negy.hdr"
                         , base <> "_posz.hdr"
                         , base <> "_negz.hdr"
                         ] :: [FilePath]
          ress <- for allPaths \path' -> do
            bytes <- liftIO $ BS.readFile path'

            case Picture.decodeHDR bytes of
              Left s -> error $ printf "Can't decode HDR image at path %s: %s" path' s
              Right dynImage -> case dynImage of
                Picture.ImageRGBF (Picture.pixelMap addAlpha . (bool id Picture.flipVertically options.shouldFlipVertically) -> (Picture.Image width height dat)) -> pure (width, height, dat)
                _ -> error "Invalid image type decoded at path %s" path'
          let w = fromMaybe (error $ printf "Can't find Cubemap width for HDR at path %s" path) $ preview (each . _1) ress
              h = fromMaybe (error $ printf "Can't find Cubemap width for HDR at path %s" path) $ preview (each . _2) ress
              allDats = toListOf (each . _3) ress
          pure (w,h,SV.concat allDats)

        else do
          bytes <- liftIO $ BS.readFile path
          case Picture.decodeHDR bytes of
            Left s -> error $ printf "Can't decode HDR image at path %s: %s" path s
            Right dynImage -> case dynImage of
              Picture.ImageRGBF (Picture.pixelMap addAlpha . (bool id Picture.flipVertically options.shouldFlipVertically) -> (Picture.Image width height dat)) -> pure (width, height, dat)
              _ -> error $ printf "Invalid image type decoded at path %s" path

      let extent = Extent3D (fromIntegral width) (fromIntegral height) 1
      withImageFromArray bag extent IMAGE_TYPE_2D (formatForImageType options.fileType) shouldGenerateMips arrayLayers flags dat
    KTX2 -> error "Unsupported"
  where
  addAlpha :: Picture.PixelRGBF -> PixelRGBAF
  addAlpha (Picture.PixelRGBF r g b) = PixelRGBAF r g b 1


copyBufferToImage :: MonadIO m => VulkanResources -> Buffer -> Image -> Word32 -> Word32 -> Extent3D -> m ()
copyBufferToImage bag buffer image mipLevel arrayLayers extent = withSingleTimeCommands bag \commandBuffer -> do
    let region :: BufferImageCopy
        region = zero
          { bufferOffset      = 0
          , bufferRowLength   = 0
          , bufferImageHeight = 0
          , imageSubresource = zero
            { aspectMask     = IMAGE_ASPECT_COLOR_BIT
            , mipLevel       = mipLevel
            , baseArrayLayer = 0
            , layerCount     = arrayLayers
            }
          , imageOffset = zero
          , imageExtent = extent
          }

    cmdCopyBufferToImage commandBuffer buffer image IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [region]

transitionImageLayout :: MonadIO m => Image -> ImageLayout -> ImageLayout -> CommandBuffer -> m ()
transitionImageLayout image = transitionImageLayoutMips image 1 1

transitionImageLayoutMips :: MonadIO m => Image -> Word32 -> Word32 -> ImageLayout -> ImageLayout -> CommandBuffer -> m ()
transitionImageLayoutMips image mipLevels arrayLayers oldLayout newLayout commandBuffer = do
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
        (IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL, IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL) ->
          ( ACCESS_SHADER_READ_BIT
          , ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
          , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
          , PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
          , IMAGE_ASPECT_DEPTH_BIT
          )
        (IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL) ->
          ( ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          , ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          , PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          , IMAGE_ASPECT_COLOR_BIT
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
        , layerCount     = arrayLayers
        }

  cmdPipelineBarrier commandBuffer sourceStage destinationStage zero [] [] [SomeStruct barrier]

imageBarrier
  :: MonadIO io
  => CommandBuffer
  -> ImageLayout
  -> PipelineStageFlags
  -> AccessFlags
  -> ImageLayout
  -> PipelineStageFlags
  -> AccessFlags
  -> ImageAspectFlags
  -> Word32
  -> Word32
  -> Image
  -> io ()
imageBarrier commandBuffer oldLayout sourceStage srcAccessMask newLayout destinationStage dstAccessMask aspectMask numLevels numLayers image =
  cmdPipelineBarrier commandBuffer sourceStage destinationStage zero [] [] [SomeStruct barrier]
  where
  barrier :: ImageMemoryBarrier '[]
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
    , levelCount     = numLevels
    , baseArrayLayer = 0
    , layerCount     = numLayers
    }

withImageSampler :: VulkanResources -> Filter -> SamplerAddressMode -> SamplerMipmapMode -> Acquire Sampler
withImageSampler vr = withImageSamplerMips vr 0

withImageSamplerMips :: VulkanResources -> Word32 -> Filter -> SamplerAddressMode -> SamplerMipmapMode -> Acquire Sampler
withImageSamplerMips VulkanResources { deviceContext = DeviceContext {..} } mipLevels filt addressMode mipmapMode =
  withSampler device samplerInfo Nothing mkAcquire
  where
  samplerInfo = zero
    { magFilter = filt
    , minFilter = filt
    , addressModeU = addressMode
    , addressModeV = addressMode
    , addressModeW = addressMode
    , anisotropyEnable = mipLevels > 0
    , maxAnisotropy = properties.limits.maxSamplerAnisotropy
    , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
    , unnormalizedCoordinates = False
    , compareEnable = True
    , compareOp = COMPARE_OP_ALWAYS
    , mipmapMode = mipmapMode
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
    , compareEnable = False
    -- , compareOp = COMPARE_OP_LESS
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

generateMipmaps :: VulkanResources -> Image -> Format -> Word32 -> Word32 -> Word32 -> IO ()
generateMipmaps bag image imageFormat texWidth texHeight mipLevels = do
  props <- getPhysicalDeviceFormatProperties bag.deviceContext.physicalDevice imageFormat
  unless (props.optimalTilingFeatures .&&. FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT) do
    error "Image format does not support linear blit"

  withSingleTimeCommands bag \cb -> do
    for_ ([1..mipLevels-1] :: [Word32]) \level -> do
      let barrier :: ImageMemoryBarrier '[]
          barrier = zero
            { oldLayout = IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            , newLayout = IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
            , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
            , image = image
            , srcAccessMask = ACCESS_TRANSFER_WRITE_BIT
            , dstAccessMask = ACCESS_TRANSFER_READ_BIT
            , subresourceRange = subResourceRange
            }

          subResourceRange :: ImageSubresourceRange
          subResourceRange = ImageSubresourceRange
            { aspectMask     = IMAGE_ASPECT_COLOR_BIT
            , baseMipLevel   = level - 1
            , levelCount     = 1
            , baseArrayLayer = 0
            , layerCount     = 1
            }

          mipWidth  :: Double = realToFrac texWidth * 0.5 ** realToFrac (level - 1)
          mipHeight :: Double = realToFrac texHeight * 0.5 ** realToFrac (level - 1)

          blit :: ImageBlit
          blit = ImageBlit
            { srcOffsets = ( Offset3D 0 0 0, Offset3D (floor mipWidth) (floor mipHeight) 1)
            , dstOffsets = ( Offset3D 0 0 0, Offset3D (if mipWidth > 1 then floor (mipWidth / 2) else 1) (if mipHeight > 1 then floor (mipHeight / 2) else 1) 1)
            , srcSubresource = blitSrcSubresourceRange
            , dstSubresource = blitDstSubresourceRange
            }

          blitSrcSubresourceRange :: ImageSubresourceLayers
          blitSrcSubresourceRange = ImageSubresourceLayers
            { aspectMask = IMAGE_ASPECT_COLOR_BIT
            , mipLevel = level - 1
            , baseArrayLayer = 0
            , layerCount = 1
            }

          blitDstSubresourceRange :: ImageSubresourceLayers
          blitDstSubresourceRange = ImageSubresourceLayers
            { aspectMask = IMAGE_ASPECT_COLOR_BIT
            , mipLevel = level
            , baseArrayLayer = 0
            , layerCount = 1
            }

      cmdPipelineBarrier cb PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_TRANSFER_BIT zero [] [] [SomeStruct barrier]

      cmdBlitImage
        cb
        image IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        image IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        [blit]
        FILTER_LINEAR

      cmdPipelineBarrier cb PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_FRAGMENT_SHADER_BIT zero [] []
        [SomeStruct $ barrier { oldLayout = IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                              , newLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                              , srcAccessMask = ACCESS_TRANSFER_READ_BIT
                              , dstAccessMask = ACCESS_SHADER_READ_BIT
                              } ]

    let barrier :: ImageMemoryBarrier '[]
        barrier = zero
          { oldLayout = IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
          , newLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
          , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
          , image = image
          , srcAccessMask = ACCESS_TRANSFER_WRITE_BIT
          , dstAccessMask = ACCESS_SHADER_READ_BIT
          , subresourceRange = subResourceRange
          }

        subResourceRange :: ImageSubresourceRange
        subResourceRange = ImageSubresourceRange
          { aspectMask     = IMAGE_ASPECT_COLOR_BIT
          , baseMipLevel   = mipLevels - 1
          , levelCount     = 1
          , baseArrayLayer = 0
          , layerCount     = 1
          }
    cmdPipelineBarrier cb PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_FRAGMENT_SHADER_BIT zero [] [] [SomeStruct barrier]

-- | Load a KTX2 texture
withKtxTexture
  :: VulkanResources
  -> TextureLoadOptions
  -> FilePath
  -> Acquire (ViewableImage, Sampler)
withKtxTexture vulkanResources@VulkanResources { allocator } options fp = do
  ctx <- liftIO $ KTX.open fp
  let hdr    = KTX.header ctx
      fmt    = Format (fromIntegral hdr.vkFormat)
  lvls <- liftIO $ KTX.levels ctx
  let imgW   = fromIntegral hdr.pixelWidth
      imgH   = fromIntegral hdr.pixelHeight
      imgD   = max 1 (fromIntegral hdr.pixelDepth)
      faceCount = hdr.faceCount
      layers = max 1 hdr.layerCount * faceCount
      numLevels = fromIntegral (length lvls)
      viewType = if faceCount == 6 then IMAGE_VIEW_TYPE_CUBE else IMAGE_VIEW_TYPE_2D
      imageType = if | hdr.pixelDepth > 0  -> IMAGE_TYPE_3D
                     | hdr.pixelHeight > 0 -> IMAGE_TYPE_2D
                     | otherwise           -> IMAGE_TYPE_1D

  let perPixelBytes = bytesPerPixel fmt
      levelSizes   = V.map (fromIntegral . (.byteLength)) lvls
      levelOffsets = V.scanl
                      (\off sz -> alignPtr (plusPtr off sz) perPixelBytes)
                      (wordPtrToPtr (WordPtr 0))
                      levelSizes
      WordPtr (fromIntegral -> totalSz) = ptrToWordPtr $ V.last levelOffsets

  (stagingBuffer, stagingAlloc, _) <- withBuffer' allocator
    BUFFER_USAGE_TRANSFER_SRC_BIT
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    totalSz

  liftIO $ withMappedMemory allocator stagingAlloc bracket \basePtr -> (\f -> V.zipWithM_ f lvls levelOffsets) \lvl off -> do
    let IntPtr offInt = ptrToIntPtr off
        dst = basePtr `plusPtr` offInt
    res <- KTX.readLevelTo ctx.context lvl dst
    unless res $ fail "Couldn't read KTX2 level"

  let imageCreateInfo :: ImageCreateInfo '[]
      imageCreateInfo = zero
        { imageType     = imageType
        , extent        = Extent3D imgW imgH imgD
        , format        = fmt
        , mipLevels     = numLevels
        , arrayLayers   = layers
        , tiling        = IMAGE_TILING_OPTIMAL
        , samples       = SAMPLE_COUNT_1_BIT
        , usage         = IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT
        , sharingMode   = SHARING_MODE_EXCLUSIVE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        , flags         = if faceCount == 6 then IMAGE_CREATE_CUBE_COMPATIBLE_BIT else zero
        }
      allocationCreateInfo :: AllocationCreateInfo
      allocationCreateInfo = zero { requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }
  (img, _, _) <- withImage allocator imageCreateInfo allocationCreateInfo mkAcquire

  withSingleTimeCommands vulkanResources \cb -> do
    imageBarrier cb IMAGE_LAYOUT_UNDEFINED PIPELINE_STAGE_TOP_OF_PIPE_BIT zero
                    IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL PIPELINE_STAGE_TRANSFER_BIT ACCESS_TRANSFER_WRITE_BIT
                    IMAGE_ASPECT_COLOR_BIT (fromIntegral $ length lvls) layers img

    let copyRegions = flip V.imap (V.zipWith const levelOffsets lvls) \(fromIntegral -> mip) (ptrToWordPtr -> WordPtr off) ->
          BufferImageCopy
          { bufferOffset      = fromIntegral off
          , bufferRowLength   = 0
          , bufferImageHeight = 0
          , imageSubresource  = ImageSubresourceLayers
            { aspectMask       = IMAGE_ASPECT_COLOR_BIT
            , mipLevel         = mip
            , baseArrayLayer   = 0
            , layerCount       = layers
            }
          , imageOffset = Offset3D 0 0 0
          , imageExtent = Extent3D
            { width  = max 1 (imgW `shiftR` fromIntegral mip)
            , height = max 1 (imgH `shiftR` fromIntegral mip)
            , depth  = 1
            }
          }

    cmdCopyBufferToImage cb stagingBuffer img IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL copyRegions
    imageBarrier cb IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL PIPELINE_STAGE_TRANSFER_BIT ACCESS_TRANSFER_WRITE_BIT
                    IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL PIPELINE_STAGE_FRAGMENT_SHADER_BIT ACCESS_SHADER_READ_BIT
                    IMAGE_ASPECT_COLOR_BIT (fromIntegral $ length lvls) layers img

  imageView <- with2DImageViewMips vulkanResources.deviceContext fmt IMAGE_ASPECT_COLOR_BIT img numLevels viewType 0 layers
  sampler <- withImageSamplerMips vulkanResources numLevels options.filter options.samplerAddressMode (fromMaybe SAMPLER_MIPMAP_MODE_LINEAR options.samplerMipmapMode)
  pure (ViewableImage img imageView fmt, sampler)

bytesPerPixel :: Format -> Int
bytesPerPixel = \case
  FORMAT_R8_UNORM           -> 1
  FORMAT_R8G8_UNORM         -> 2
  FORMAT_R8G8B8_UNORM       -> 3
  FORMAT_R8G8B8A8_UNORM     -> 4

  FORMAT_R16_SFLOAT         -> 2
  FORMAT_R16G16_SFLOAT      -> 4
  FORMAT_R16G16B16A16_SFLOAT-> 8

  FORMAT_R32_SFLOAT         -> 4
  FORMAT_R32G32_SFLOAT      -> 8
  FORMAT_R32G32B32A32_SFLOAT-> 16

  fmt -> error $ "bytesPerPixel: unsupported format " ++ show fmt

-- | Write image as .ktx2
writeKtxFile
  :: VulkanResources
  -> Image
  -> Format
  -> Word32 -- ^ mip 0 width
  -> Word32 -- ^ mip 0 height
  -> Word32 -- ^ mip 0 depth  (1 for 2D)
  -> Word32 -- ^ array layers (0 for no array)
  -> Word32 -- ^ face count (1 or 6 for cube)
  -> Word32 -- ^ mip count (0 for no mips)
  -> FilePath
  -> IO ()
writeKtxFile vk@VulkanResources { allocator } img format w0 h0 d0 layersHdr facesHdr mipHdr path = runAcquire do
  let layers        = max 1 layersHdr
      faces         = if facesHdr == 6 then 6 else 1
      arrayLayers   = faces * layers
      mips          = max 1 mipHdr
      perPixelBytes = bytesPerPixel format
      dims          = [ ( max 1 (w0 `shiftR` fromIntegral i)
                        , max 1 (h0 `shiftR` fromIntegral i)
                        , max 1 (d0 `shiftR` fromIntegral i)
                        )
                      | i <- [0..mips-1] ]
      levelSizes    = [   fromIntegral (w * h * d * arrayLayers)
                        * fromIntegral perPixelBytes
                      | (w,h,d) <- dims ]
      totalSz       = sum levelSizes
      Format (fromIntegral -> formatRawEnum) = format
      offsets                                = scanl (+) 0 levelSizes

  (stgBuf, stgAlloc, _) <- withBuffer' allocator
    BUFFER_USAGE_TRANSFER_DST_BIT
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    totalSz

  withSingleTimeCommands vk \cb -> do
    imageBarrier cb IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                     PIPELINE_STAGE_FRAGMENT_SHADER_BIT ACCESS_SHADER_READ_BIT
                     IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL   PIPELINE_STAGE_TRANSFER_BIT ACCESS_TRANSFER_READ_BIT
                     IMAGE_ASPECT_COLOR_BIT mips arrayLayers img

    let regions = zipWith3 (\mipLevel off (w,h,d) ->
                    BufferImageCopy
                      { bufferOffset      = off
                      , bufferRowLength   = 0
                      , bufferImageHeight = 0
                      , imageSubresource  = ImageSubresourceLayers IMAGE_ASPECT_COLOR_BIT mipLevel 0 arrayLayers
                      , imageOffset       = Offset3D 0 0 0
                      , imageExtent       = Extent3D w h d
                      })
                  [0..]
                  offsets
                  dims

    cmdCopyImageToBuffer cb img IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL stgBuf (V.fromList regions)

  allBS <- liftIO $ withMappedMemory allocator stgAlloc bracket \(ptr :: Ptr ()) ->
    BS.packCStringLen (castPtr ptr, fromIntegral totalSz)

  let blobs =
            [ (Nothing, BS.take (fromIntegral sz) . BS.drop (fromIntegral off) $ allBS)
            | (off,sz) <- zip offsets levelSizes
            ]
      header =
        (KTX.prepare formatRawEnum
          4  -- can we assume 4 byte components? does this field even matter?
          w0 h0 d0
          0) -- no supercompression
          { KTX.layerCount = layersHdr
          , KTX.faceCount  = facesHdr
          , KTX.levelCount = mipHdr
          }
      dfd = KTX.toBlock KTX.unspecified
      kvd = mempty
      sgd = BS.empty

  KTX.toFile path header (V.singleton dfd) kvd sgd blobs

cubeFormat :: Format
cubeFormat = FORMAT_R32G32B32A32_SFLOAT
