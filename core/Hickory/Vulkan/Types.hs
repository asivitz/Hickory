{-# LANGUAGE DuplicateRecordFields, DerivingStrategies  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Types where

import Vulkan (RenderPass, Framebuffer, Pipeline, PipelineLayout, DescriptorPool, DescriptorSetLayout, DescriptorSet, Buffer, Sampler, Extent2D, SampleCountFlagBits, CullModeFlagBits)
import qualified Data.Vector as V
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hickory.Vulkan.Framing (FramedResource)
import Hickory.Vulkan.Mesh (Attribute)
import VulkanMemoryAllocator (Allocation, Allocator)
import Hickory.Vulkan.Vulkan (ViewableImage)
import Linear (V3)
import Foreign.Storable.Generic (GStorable)
import qualified Data.ByteString as B

data RenderTarget = RenderTarget
  { renderPass     :: !RenderPass
  , frameBuffers   :: !(V.Vector Framebuffer)
  , descriptorSpec :: DescriptorSpec
  , extent         :: !Extent2D
  , samples        :: !SampleCountFlagBits
  , cullMode       :: !CullModeFlagBits
  , fragShaderOverride :: Maybe B.ByteString

  } deriving Generic

data Material a = Material
  { pipelineLayout         :: PipelineLayout
  , materialDescriptorSet  :: FramedResource PointedDescriptorSet -- Bound along with the material
  , uuid                   :: UUID
  , attributes             :: [Attribute]
  -- We rebind this global set along with the material, b/c if the push
  -- constant range doesn't match between materials, this can get unbound :(
  , globalDescriptorSet    :: FramedResource PointedDescriptorSet
  , pipelines              :: V.Vector Pipeline
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
  , frameNumber :: Int
  } deriving Generic
    deriving anyclass GStorable
