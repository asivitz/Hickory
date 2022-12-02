{-# LANGUAGE DuplicateRecordFields, DerivingStrategies  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Types where

import Vulkan (RenderPass, Framebuffer, Pipeline, PipelineLayout, DescriptorPool, DescriptorSetLayout, DescriptorSet, Buffer)
import qualified Data.Vector as V
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hickory.Vulkan.Framing (FramedResource)
import Hickory.Vulkan.Mesh (Attribute)
import VulkanMemoryAllocator (Allocation, Allocator)

data RenderTarget = RenderTarget
  { shadowPass           :: !RenderPass
  , renderPass           :: !RenderPass
  , shadowFrameBuffer    :: !Framebuffer
  , frameBuffers         :: !(V.Vector Framebuffer)
  , globalDescriptorSet  :: !PointedDescriptorSet
  , globalsBuffer        :: (Buffer, Allocation, Allocator)
  } deriving Generic

data Material a = Material
  { pipelineLayout         :: PipelineLayout
  , shadowPipeline         :: Pipeline
  , multiSamplePipeline    :: Pipeline
  , singleSamplePipeline   :: Pipeline
  , materialDescriptorSet  :: FramedResource PointedDescriptorSet -- Bound along with the material
  , uuid                   :: UUID
  , attributes             :: [Attribute]
  -- We rebind this global set along with the material, b/c if the push
  -- constant range doesn't match between materials, this can get unbound :(
  , globalDescriptorSet    :: FramedResource PointedDescriptorSet
  } deriving Generic

data PointedDescriptorSet = PointedDescriptorSet
  { descriptorPool      :: DescriptorPool
  , descriptorSetLayout :: DescriptorSetLayout
  , descriptorSet       :: DescriptorSet
  , uuid                :: UUID
  } deriving Generic
