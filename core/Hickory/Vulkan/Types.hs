{-# LANGUAGE DuplicateRecordFields, DerivingStrategies  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Types where

import Vulkan (RenderPass, Framebuffer, Pipeline, PipelineLayout, DescriptorPool, DescriptorSetLayout, DescriptorSet, Buffer, Sampler, Extent2D, SampleCountFlagBits, CullModeFlagBits, CommandPool, Instance, CommandBuffer, Fence, Semaphore, PhysicalDevice, Queue, Device, SurfaceFormatKHR, PresentModeKHR, SwapchainKHR, Image, ImageView, Format)
import qualified Data.Vector as V
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hickory.Vulkan.Framing (FramedResource)
import VulkanMemoryAllocator (Allocation, Allocator)
import Linear (V3 (..))
import Foreign.Storable.Generic (GStorable)
import GHC.Word (Word32)
import Acquire.Acquire (Acquire)
import qualified Data.Vector.Storable as SV

data VulkanResources = VulkanResources
  { deviceContext         :: DeviceContext
  , allocator             :: Allocator
  , shortLivedCommandPool :: CommandPool -- For, e.g., mem copy commands
  , inst                  :: Instance
  , frames                :: FramedResource Frame
  , acquireSwapchain      :: (Int,Int) -> Acquire Swapchain
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
  | Color
  | BoneIndex
  | MaterialIndex
  deriving (Bounded, Enum, Generic, Show, Eq)

data Mesh = Mesh
  { vertices :: [(Attribute, SV.Vector Float)]
  , indices :: Maybe (SV.Vector Word32)
  } deriving (Generic, Show)

data BufferedMesh = BufferedMesh
  { mesh         :: Mesh
  , vertexBuffer :: Buffer
  , indexBuffer  :: Maybe Buffer
  }

data RenderTarget = RenderTarget
  { renderPass      :: !RenderPass
  , frameBuffers    :: !(V.Vector Framebuffer)
  , descriptorSpecs :: [DescriptorSpec]
  , extent          :: !Extent2D
  , samples         :: !SampleCountFlagBits
  , cullMode        :: !CullModeFlagBits
  } deriving Generic

data Material a = Material
  { pipelineLayout         :: PipelineLayout
  , materialDescriptorSet  :: FramedResource PointedDescriptorSet -- Bound along with the material
  , uuid                   :: UUID
  , attributes             :: [Attribute]
  -- We rebind this global set along with the material, b/c if the push
  -- constant range doesn't match between materials, this can get unbound :(
  , globalDescriptorSet    :: FramedResource PointedDescriptorSet
  , pipeline               :: Pipeline
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
  | DepthImageDescriptor ViewableImage Sampler
  | BufferDescriptor Buffer

data ForwardRenderTarget = ForwardRenderTarget
  { swapchainRenderTarget :: !RenderTarget
  , shadowRenderTarget    :: !RenderTarget
  , litRenderTarget       :: !RenderTarget
  , postProcessMaterial   :: !(Material PostConstants)
  , globalDescriptorSet   :: !PointedDescriptorSet
  , globalsBuffer         :: (Buffer, Allocation, Allocator)
  } deriving Generic

data PostConstants = PostConstants
  { exposure    :: Float
  , colorShift  :: V3 Float
  , saturation  :: Float
  , filmGrain   :: Float
  } deriving Generic
    deriving anyclass GStorable

data DataBuffer a = DataBuffer
  { buf        :: Buffer
  , allocation :: Allocation
  , allocator  :: Allocator
  } deriving Generic

postDefaults :: PostConstants
postDefaults = PostConstants
  { exposure    = 0
  , colorShift  = V3 1 1 1
  , saturation  = 1
  , filmGrain   = 0
  }
