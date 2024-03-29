{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}

module Hickory.Vulkan.Forward.Types where

import Hickory.Vulkan.Types (PointedDescriptorSet, RenderConfig, Material, PostConstants, DataBuffer, BufferedMesh, Mesh, FrameContext, DescriptorSpec, ViewableImage)
import Linear (M44, V4, V2, M33, V3 (..), identity, zero)
import qualified Data.Vector.Storable.Sized as VSS
import qualified Data.Vector.Sized as VS
import GHC.Generics (Generic)
import Hickory.Vulkan.Monad (BufferedUniformMaterial)
import Hickory.Vulkan.Forward.ObjectPicking (ObjectIDConstants)
import Foreign.Storable.Generic (GStorable)
import Hickory.Vulkan.Text (MSDFMatConstants)
import Hickory.Vulkan.Framing (FramedResource)
import Hickory.Vulkan.DynamicMesh (DynamicBufferedMesh(..))
import Data.Functor.Identity (Identity (..))
import Hickory.Math (Scalar)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Writer.Strict (MonadWriter(..), WriterT (..), Writer)
import Hickory.Vulkan.RenderTarget (ImageBuffer)
import GHC.Word (Word32)
import Hickory.Camera (Camera(..), Projection (..))
import Foreign (Ptr)
import Hickory.Vulkan.DescriptorSet
import Data.UUID (UUID)
import Vulkan (DescriptorSetLayout, Framebuffer)
import Hickory.Types (Size)
import Hickory.Input (InputFrame)
import Hickory.Vulkan.Forward.ShaderDefinitions (MaxShadowCascadesNat)

data ForwardRenderTargets = ForwardRenderTargets
  { swapchainRenderConfig        :: !RenderConfig
  , swapchainRenderFrame         :: FramedResource (Framebuffer, [DescriptorSpec])
  , litRenderConfig              :: !RenderConfig
  , litRenderFrame               :: FramedResource (Framebuffer, [DescriptorSpec])
  , objectIDRenderConfig         :: !RenderConfig
  , pickingRenderFrame           :: FramedResource (Framebuffer, [DescriptorSpec])
  , currentSelectionRenderFrame  :: FramedResource (Framebuffer, [DescriptorSpec])
  -- Shadows
  , shadowRenderConfig           :: !RenderConfig
  , cascadedShadowMap            :: FramedResource (VS.Vector MaxShadowCascadesNat (Framebuffer, [DescriptorSpec]), DescriptorSpec)
  }

data Renderer = Renderer
  { renderTargets :: ForwardRenderTargets

  -- Pipelines
  , pickingMaterial            :: !(BufferedUniformMaterial Word32 ObjectIDConstants)
  , currentSelectionMaterial   :: !(BufferedUniformMaterial Word32 ObjectIDConstants)
  , staticShadowMaterial       :: !(BufferedUniformMaterial ShadowPushConsts StaticConstants)
  , animatedShadowMaterial     :: !(BufferedUniformMaterial ShadowPushConsts AnimatedConstants)
  , staticLitWorldMaterial     :: !(BufferedUniformMaterial Word32 StaticConstants)
  , staticUnlitWorldMaterial   :: !(BufferedUniformMaterial Word32 StaticConstants)
  , animatedLitWorldMaterial   :: !(BufferedUniformMaterial Word32 AnimatedConstants)
  , msdfWorldMaterial          :: !(BufferedUniformMaterial Word32 MSDFMatConstants)
  , linesWorldMaterial         :: !(BufferedUniformMaterial Word32 StaticConstants)
  , pointsWorldMaterial        :: !(BufferedUniformMaterial Word32 StaticConstants)
  , objHighlightMaterial       :: !(Material Word32)
  , staticOverlayMaterial      :: !(BufferedUniformMaterial Word32 StaticConstants)
  , msdfOverlayMaterial        :: !(BufferedUniformMaterial Word32 MSDFMatConstants)

  , postProcessMaterial      :: !(Material PostConstants)
  , globalBuffer             :: !(FramedResource (DataBuffer Globals))
  , globalShadowPassBuffer   :: !(FramedResource (DataBuffer ShadowGlobals))
  , globalWorldBuffer        :: !(FramedResource (DataBuffer WorldGlobals))
  , globalOverlayBuffer      :: !(FramedResource (DataBuffer OverlayGlobals))
  , dynamicMesh              :: FramedResource DynamicBufferedMesh
  , objectPickingImageBuffer :: FramedResource ImageBuffer

  , globalDescriptorSet      :: FramedResource PointedDescriptorSet
  , shadowMapDescriptorSet   :: FramedResource PointedDescriptorSet
  , imageSetLayout           :: DescriptorSetLayout
  } deriving Generic

-- params: targ, shaders

-- objid: static params
-- shadow: static params
-- shadow: animated params
-- world: lit shaders, static params
-- world: unlit shaders, static params
-- world: lit shaders, animated params
-- world: unlit shaders, animated params
-- world: unlit shaders, msdf
-- world: unlit shaders, lines
-- overlay: unlit shaders, static params
-- overlay: unlit shaders, msdf

data Globals = Globals
  { frameNumber :: Int
  } deriving Generic
    deriving anyclass GStorable

data DrawCommand = DrawCommand
  { modelMat    :: M44 Float
  , mesh        :: MeshType
  , color       :: V4 Float
  , specularity :: Float
  , drawType    :: DrawType
  , lit         :: Bool
  , castsShadow :: Bool
  , blend       :: Bool
  , ident       :: Maybe Int
  }
  | Custom CustomDrawCommand
  deriving Generic

data CustomDrawCommand = forall uniform. CustomDrawCommand
  { material        :: AllStageMaterial uniform
  , pokeData        :: Ptr uniform -> IO ()
  , mesh            :: MeshType
  , instanceCount   :: Word32
  , modelMat        :: M44 Float
  , descriptorSet   :: Maybe PointedDescriptorSet
  , doBlend         :: Bool
  , doCastShadow    :: Bool
  , hasIdent        :: Maybe Int
  , cull            :: Bool
  }

data Stage
  = Picking
  | ShowSelection
  | CreateShadowMap
  | World
  | Overlay
  deriving Eq

data DrawType
  = Animated AnimatedMesh
  | Static StaticMesh
  | MSDF MSDFMesh
  | Lines
  | Points
  deriving Generic

data MeshType
  = Buffered !BufferedMesh
  | Dynamic !Mesh

data AnimatedMesh = AnimatedMesh
  { albedo   :: PointedDescriptorSet
  , boneMat  :: VSS.Vector 66 (M44 Float)
  , colors   :: VSS.Vector 6 (V4 Float)
  }

data StaticMesh = StaticMesh
  { albedo   :: PointedDescriptorSet
  , tiling   :: V2 Float
  }

data MSDFMesh = MSDFMesh
  { tex           :: PointedDescriptorSet
  , outlineColor  :: V4 Float
  , outlineSize   :: Float -- In pixels
  , sdfPixelRange :: Float -- Should match parameter used to generate MSDF
  , tiling        :: V2 Float
  }

data StaticConstants = StaticConstants
  { modelMat    :: M44 Float
  , normalMat   :: M33 Float
  , color       :: V4 Float
  , specularity :: Float
  , tiling      :: V2 Float
  , objectID    :: Word32
  } deriving Generic
    deriving anyclass GStorable

data AnimatedConstants = AnimatedConstants
  { modelMat    :: M44 Float
  , normalMat   :: M33 Float
  , color       :: V4 Float
  , specularity :: Float
  , boneMat     :: VSS.Vector 66 (M44 Float) -- TODO: Parameterize
  , colors      :: VSS.Vector 6 (V4 Float)
  , objectID    :: Word32
  } deriving Generic
    deriving anyclass GStorable

data ShadowPushConsts = ShadowPushConsts
  { uniformIndex :: Word32
  , cascadeIndex :: Word32
  } deriving Generic
    deriving anyclass GStorable

data AllStageMaterial uniform = AllStageMaterial
  { worldMaterial            :: Material Word32
  , shadowMaterial           :: Material ShadowPushConsts
  , objectIDMaterial         :: Material Word32
  , showSelectionMaterial    :: Material Word32
  , overlayMaterial          :: Maybe (Material Word32)
  , descriptor               :: FramedResource (BufferDescriptorSet uniform)
  , uniformSize              :: Int -- Bytes
  , uuid                     :: UUID
  }

data RenderSettings = RenderSettings
  { clearColor     :: V4 Scalar
  , worldSettings  :: WorldSettings
  , overlayGlobals :: OverlayGlobals
  , postSettings   :: PostConstants
  , highlightObjs  :: [Int]
  } deriving Generic

data WorldGlobals = WorldGlobals
  { viewMat        :: M44 Scalar
  , projMat        :: M44 Scalar
  , viewProjMat    :: M44 Scalar
  , camPos         :: V3 Scalar
  , lightDirection :: V3 Scalar
  , sunColor       :: V3 Scalar -- HDR
  , ambientColor   :: V3 Scalar -- HDR
  , multiSampleCount :: Scalar
  , nearPlane      :: Scalar
  , farPlane       :: Scalar
  } deriving Generic
    deriving anyclass GStorable

data OverlayGlobals = OverlayGlobals
  { viewMat        :: M44 Scalar
  , projMat        :: M44 Scalar
  , viewProjMat    :: M44 Scalar
  } deriving Generic
    deriving anyclass GStorable

data ShadowGlobals = ShadowGlobals
  { viewProjMats :: VS.Vector MaxShadowCascadesNat (M44 Scalar)
  , splitDepths  :: VS.Vector MaxShadowCascadesNat Scalar -- Far plane of each cascade
  } deriving Generic
    deriving anyclass GStorable

data WorldSettings = WorldSettings
  { camera :: Camera
  , lightTransform :: M44 Scalar
  , lightDirection :: V3 Scalar
  , sunColor       :: V3 Scalar -- HDR
  , ambientColor   :: V3 Scalar -- HDR
  } deriving Generic

worldSettingsDefaults :: WorldSettings
worldSettingsDefaults = WorldSettings {..}
  where
  camera = Camera zero (V3 (-1) (-1) (-1)) (V3 0 0 1) (Perspective (pi/4) 0.1 100) "DefaultWorldSettings"
  lightTransform = identity
  lightDirection = V3 1 1 1
  sunColor = V3 1 1 1
  ambientColor = V3 1 1 1

-- Monad

-- newtype CommandT m a = CommandT { unCommandT :: StateT [DrawCommand] m a }
  -- deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState [DrawCommand], MonadTrans)

type CommandMonad m = MonadWriter [DrawCommand] m
type Command a = Writer [DrawCommand] a
type CommandT = WriterT [DrawCommand]

runCommand :: Command a -> [DrawCommand]
runCommand = snd . runIdentity . runWriterT

addCommand :: CommandMonad m => DrawCommand -> m ()
addCommand = tell . pure

type RenderFunction swapchainResources = Size Int -> (swapchainResources, FrameContext) -> IO ()
type Scene swapchainResources = InputFrame -> IO (Scalar -> InputFrame -> RenderFunction swapchainResources)
