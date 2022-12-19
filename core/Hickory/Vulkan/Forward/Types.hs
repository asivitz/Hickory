{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Forward.Types where

import Hickory.Vulkan.Types (PointedDescriptorSet, RenderTarget, Material, PostConstants, DataBuffer, BufferedMesh, Mesh)
import Linear (M44, V4, V2, M33, V3 (..), identity)
import qualified Data.Vector.Fixed.Storable as VFS
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

data Renderer = Renderer
  { swapchainRenderTarget  :: !RenderTarget
  , shadowRenderTarget     :: !RenderTarget
  , litRenderTarget        :: !RenderTarget
  , pickingRenderTarget   :: !RenderTarget
  , currentSelectionRenderTarget   :: !RenderTarget
  -- Pipelines
  , pickingMaterial       :: !(BufferedUniformMaterial ObjectIDConstants)
  , currentSelectionMaterial       :: !(BufferedUniformMaterial ObjectIDConstants)
  , staticShadowMaterial   :: !(BufferedUniformMaterial StaticConstants)
  , animatedShadowMaterial :: !(BufferedUniformMaterial AnimatedConstants)

  , staticLitWorldMaterial     :: !(BufferedUniformMaterial StaticConstants)
  , staticUnlitWorldMaterial   :: !(BufferedUniformMaterial StaticConstants)
  , animatedLitWorldMaterial   :: !(BufferedUniformMaterial AnimatedConstants)
  -- , animatedUnlitWorldMaterial :: !(BufferedUniformMaterial AnimatedConstants)
  , msdfWorldMaterial          :: !(BufferedUniformMaterial MSDFMatConstants)
  , linesWorldMaterial         :: !(BufferedUniformMaterial StaticConstants)
  , objHighlightMaterial    :: !(Material Word32)

  , staticOverlayMaterial :: !(BufferedUniformMaterial StaticConstants)
  , msdfOverlayMaterial   :: !(BufferedUniformMaterial MSDFMatConstants)

  , postProcessMaterial    :: !(Material PostConstants)
  , globalBuffer           :: !(DataBuffer Globals)
  , globalShadowPassBuffer :: !(DataBuffer WorldGlobals)
  , globalWorldBuffer      :: !(DataBuffer WorldGlobals)
  , globalOverlayBuffer    :: !(DataBuffer WorldGlobals)
  , dynamicMesh            :: FramedResource DynamicBufferedMesh
  , objectPickingImageBuffer :: FramedResource ImageBuffer
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
  , drawType    :: DrawType
  , lit         :: Bool
  , castsShadow :: Bool
  , blend       :: Bool
  , ident       :: Maybe Int
  } deriving Generic

data DrawType
  = Animated AnimatedMesh
  | Static StaticMesh
  | MSDF MSDFMesh
  | Lines
  deriving Generic

data MeshType
  = Buffered !BufferedMesh
  | Dynamic !Mesh

data AnimatedMesh = AnimatedMesh
  { albedo   :: PointedDescriptorSet
  , boneMat  :: VFS.Vec 32 (M44 Float)
  , colors   :: VFS.Vec 6 (V4 Float)
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
  { modelMat  :: M44 Float
  , normalMat :: M33 Float
  , color     :: V4 Float
  , tiling    :: V2 Float
  } deriving Generic
    deriving anyclass GStorable

data AnimatedConstants = AnimatedConstants
  { modelMat  :: M44 Float
  , normalMat :: M33 Float
  , color     :: V4 Float
  , boneMat   :: VFS.Vec 32 (M44 Float)
  , colors    :: VFS.Vec 6 (V4 Float)
  } deriving Generic
    deriving anyclass GStorable

data RenderSettings = RenderSettings
  { clearColor     :: V4 Scalar
  , worldGlobals   :: WorldGlobals
  , overlayGlobals :: OverlayGlobals
  , highlightObjs  :: [Int]
  }

data WorldGlobals = WorldGlobals
  { viewMat        :: M44 Scalar
  , projMat        :: M44 Scalar
  , cameraPos      :: V3 Scalar
  , lightTransform :: M44 Scalar
  , lightDirection :: V3 Scalar
  , sunColor       :: V3 Scalar -- HDR
  , ambientColor   :: V3 Scalar -- HDR
  } deriving Generic
    deriving anyclass GStorable

data OverlayGlobals = OverlayGlobals
  { viewMat        :: M44 Scalar
  , projMat        :: M44 Scalar
  } deriving Generic
    deriving anyclass GStorable

worldGlobalDefaults :: WorldGlobals
worldGlobalDefaults = WorldGlobals {..}
  where
  viewMat = identity
  projMat = identity
  cameraPos = V3 0 0 0
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