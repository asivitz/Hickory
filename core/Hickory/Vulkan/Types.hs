{-# LANGUAGE DuplicateRecordFields, DerivingStrategies  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Types where

import Vulkan (RenderPass, Framebuffer, Pipeline, PipelineLayout, DescriptorPool, DescriptorSetLayout, DescriptorSet, Buffer, Sampler, Extent2D, SampleCountFlagBits, CommandPool, Instance, CommandBuffer, Fence, Semaphore, PhysicalDevice, Queue, Device, SurfaceFormatKHR, PresentModeKHR, SwapchainKHR, Image, ImageView, Format (..), PhysicalDeviceProperties, Filter (..), SamplerAddressMode (..), SamplerMipmapMode, PipelineRenderingCreateInfo)
import qualified Data.Vector as V
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hickory.Vulkan.Framing (FramedResource)
import VulkanMemoryAllocator (Allocation, Allocator)
import Linear (V3 (..))
import Foreign.Storable.Generic (GStorable)
import GHC.Word (Word32)
import Acquire (Acquire)
import qualified Data.Vector.Storable as SV
import Data.Text (Text)
import Data.IORef (IORef, atomicModifyIORef')
import Control.Monad (join)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)

data VulkanResources = VulkanResources
  { deviceContext         :: DeviceContext
  , allocator             :: Allocator
  , shortLivedCommandPool :: CommandPool -- For, e.g., mem copy commands
  , inst                  :: Instance
  , frames                :: FramedResource Frame
  , acquireSwapchain      :: (Int,Int) -> Acquire Swapchain
  , cleanupQueue          :: IORef [IO ()]
  }

data DeviceContext = DeviceContext
  { device            :: Device
  , surfaceFormat     :: SurfaceFormatKHR
  , graphicsQueue     :: Queue
  , presentQueue      :: Queue
  , graphicsFamilyIdx :: Word32
  , presentFamilyIdx  :: Word32
  , physicalDevice    :: PhysicalDevice
  , presentMode       :: PresentModeKHR
  , maxSampleCount    :: SampleCountFlagBits
  , properties        :: PhysicalDeviceProperties
  }

-- |Contains resources needed to render a frame. Need two of these for 'Double Buffering'.
data Frame = Frame
  { imageAvailableSemaphore :: Semaphore
  , renderFinishedSemaphore :: Semaphore
  , inFlightFence           :: Fence
  , commandPool             :: CommandPool
  , commandBuffer           :: CommandBuffer
  }

-- |User accessible context to render a given frame
data FrameContext = FrameContext
  { extent              :: Extent2D      -- swapchain extent
  , colorImage          :: ViewableImage -- swapchain color image for this frame (to be presented on screen)
  , commandBuffer       :: CommandBuffer -- commandbuffer to render this frame
  , frameNumber         :: Int           -- used to index FramedResources
  , swapchainImageIndex :: Word32
  , multiSampleCount    :: Int
  }

data Swapchain = Swapchain
  { imageFormat       :: SurfaceFormatKHR
  , swapchainHandle   :: SwapchainKHR
  , images            :: V.Vector ViewableImage
  , extent            :: Extent2D
  }

data ViewableImage = ViewableImage
  { image     :: Image
  , imageView :: ImageView
  , format    :: Format
  } deriving Generic

data Attribute
  = Position
  | Normal
  | TextureCoord
  | Tangent
  | Color
  | BoneIndex
  | MaterialIndex
  | JointIndices
  | JointWeights
  | FloatAttribute Word32
  deriving (Generic, Show, Eq)
  deriving anyclass Hashable

data Mesh = Mesh
  { vertices :: [(Attribute, SV.Vector Float)]
  , indices :: Maybe (SV.Vector Word32)
  , minPosition  :: V3 Float
  , maxPosition  :: V3 Float
  , morphTargets :: [(Text, [(Attribute, SV.Vector Float)])]
  } deriving (Generic, Show)

data BufferedMeshMember = BufferedMeshMember
  { indexCount   :: Maybe Word32
  , vertexCount  :: Word32
  , firstIndex   :: Maybe Word32
  , vertexOffset :: Word32
  , minPosition  :: V3 Float
  , maxPosition  :: V3 Float
  }

data BufferedMesh = BufferedMesh
  { vertexBuffer :: Buffer
  , indexBuffer  :: Maybe Buffer
  , meshOffsets  :: [(Attribute, Word32)]
  , numIndices   :: Maybe Word32
  , numVertices  :: Word32
  , name         :: Maybe String -- For tracking / debugging. Not required
  , members      :: HashMap Text BufferedMeshMember
  , uuid         :: UUID
  } deriving Generic

data FrameInOut = FrameInOut
  { frameBuffer :: Framebuffer
  , descriptorSpecs :: [DescriptorSpec]
  }

data RenderConfig = RenderConfig
  -- Left for traditional renderpass, right for dynamic rendering
  { renderPassInfo  :: Either RenderPass PipelineRenderingCreateInfo
  , extent          :: Extent2D
  , samples         :: SampleCountFlagBits
  } deriving Generic

data Material a = Material
  { pipelineLayout          :: PipelineLayout
  , uuid                    :: UUID
  , attributes              :: [Attribute]
  -- This is likely [globalSet, materialSet]
  -- We rebind this global set along with the material, b/c if the push
  -- constant range doesn't match between materials, this can get unbound :(
  , descriptorSets          :: [FramedResource PointedDescriptorSet]
  , pipeline                :: Pipeline
  , hasPerDrawDescriptorSet :: Bool
  } deriving Generic

data PointedDescriptorSet = PointedDescriptorSet
  { descriptorPool      :: DescriptorPool
  , descriptorSetLayout :: DescriptorSetLayout
  , descriptorSet       :: DescriptorSet
  , uuid                :: UUID
  } deriving Generic

data DescriptorSpec
  = ImageDescriptor [(ViewableImage, Sampler)]
  | ImageFileDescriptor (FilePath, TextureLoadOptions)
  | DepthImageDescriptor ViewableImage Sampler
  | BufferDescriptor Buffer

data DataBuffer a = DataBuffer
  { buf        :: Buffer
  , allocation :: Allocation
  , allocator  :: Allocator
  } deriving Generic

data ImageType = PNG | HDR
data ConversionTo3D = Simply2D | HorizontalSlices | VerticalSlices
  deriving Eq

data TextureLoadOptions = TextureLoadOptions
  { filter               :: Filter
  , samplerAddressMode   :: SamplerAddressMode
  , samplerMipmapMode    :: Maybe SamplerMipmapMode
  , isCubemap            :: Bool
  , fileType             :: ImageType
  , conversionTo3D       :: ConversionTo3D
  , shouldFlipVertically :: Bool
  }

formatForImageType :: ImageType -> Format
formatForImageType = \case
  PNG -> FORMAT_R8G8B8A8_UNORM
  HDR -> FORMAT_R32G32B32A32_SFLOAT

pngLoadOptions :: TextureLoadOptions
pngLoadOptions = TextureLoadOptions
  { filter = FILTER_LINEAR
  , samplerAddressMode = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , samplerMipmapMode = Nothing
  , isCubemap = False
  , fileType = PNG
  , conversionTo3D = Simply2D
  , shouldFlipVertically = True
  }

lutLoadOptions :: TextureLoadOptions
lutLoadOptions = TextureLoadOptions
  { filter = FILTER_LINEAR
  , samplerAddressMode = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , samplerMipmapMode = Nothing
  , isCubemap = False
  , fileType = PNG
  , conversionTo3D = HorizontalSlices
  , shouldFlipVertically = False
  }

hdrCubeMapLoadOptions :: TextureLoadOptions
hdrCubeMapLoadOptions = TextureLoadOptions
  { filter = FILTER_LINEAR
  , samplerAddressMode = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , samplerMipmapMode = Nothing
  , isCubemap = True
  , fileType = HDR
  , conversionTo3D = Simply2D
  , shouldFlipVertically = False
  }

-- Run this every frame. If a cleanup is queued, it will run in a few
-- frames, once the frame using the resource has been processed
runCleanup :: VulkanResources -> IO ()
runCleanup VulkanResources {..} = do
  join $ atomicModifyIORef' cleanupQueue \case
    [] -> error "Empty cleanup queue"
    (x:xs) -> (xs ++ [pure ()], x)

addCleanup :: VulkanResources -> IO () -> IO ()
addCleanup VulkanResources {..} action = do
  atomicModifyIORef' cleanupQueue \case
    [] -> error "Empty cleanup queue"
    xs -> (modifyLast (action >>) xs, ())

modifyLast :: (a -> a) -> [a] -> [a]
modifyLast _ [] = []
modifyLast f [x] = [f x]
modifyLast f (x:xs) = x : modifyLast f xs
