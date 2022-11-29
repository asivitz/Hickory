{-# LANGUAGE DuplicateRecordFields, DerivingStrategies  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Types where

import Vulkan (RenderPass, Framebuffer, Pipeline, PipelineLayout, DescriptorPool, DescriptorSetLayout, DescriptorSet)
import qualified Data.Vector as V
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hickory.Vulkan.Framing (FramedResource)
import Hickory.Vulkan.Mesh (Attribute)

data RenderTarget = RenderTarget
  { renderPass          :: !RenderPass
  , frameBuffers        :: !(V.Vector Framebuffer)
  , globalDescriptorSet :: !PointedDescriptorSet
  }

data Material a = Material
  { pipelineLayout         :: PipelineLayout
  , shadowPipeline         :: Pipeline
  , multiSamplePipeline    :: Pipeline
  , singleSamplePipeline   :: Pipeline
  , materialDescriptorSet  :: FramedResource PointedDescriptorSet -- Bound along with the material
  , uuid                   :: UUID
  , attributes             :: [Attribute]
  } deriving Generic

data PointedDescriptorSet = PointedDescriptorSet
  { descriptorPool      :: DescriptorPool
  , descriptorSetLayout :: DescriptorSetLayout
  , descriptorSet       :: DescriptorSet
  , uuid                :: UUID
  } deriving Generic
