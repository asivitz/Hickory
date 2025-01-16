{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hickory.Vulkan.Renderer.Types where

import Hickory.Vulkan.Types (PointedDescriptorSet, RenderConfig, Material, DataBuffer, BufferedMesh, Mesh, FrameContext, DescriptorSpec, ViewableImage, VulkanResources, device, deviceContext)
import Linear (M44, V4, V2, M33, V3 (..), identity, zero)
import qualified Data.Vector.Storable.Sized as VSS
import qualified Data.Vector.Sized as VS
import GHC.Generics (Generic)
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
import Data.UUID (UUID)
import Vulkan (DescriptorSetLayout, Framebuffer, HasObjectType (..), DebugUtilsObjectNameInfoEXT (..), setDebugUtilsObjectNameEXT)
import Hickory.Types (Size)
import Hickory.Input (InputFrame)
import Hickory.Vulkan.Renderer.ShaderDefinitions (MaxShadowCascadesNat)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)

{- Public API -}
data RenderSettings = RenderSettings
  { clearColor      :: V4 Scalar
  , worldSettings   :: WorldSettings
  , overlayGlobals  :: OverlayGlobals
  , postSettings    :: PostConstants
  , highlightObjs   :: [Word32]
  , ssaoSettings    :: SSAOSettings
  , shadowBiasSlope :: Scalar
  , features        :: Features
  , lut             :: Maybe PointedDescriptorSet
  } deriving Generic

data WorldSettings = WorldSettings
  { camera :: Camera
  , lightTransform :: M44 Scalar
  , lightDirection :: V3 Scalar
  , sunColor       :: V3 Scalar -- HDR
  , ambientColor   :: V3 Scalar -- HDR
  , envMap         :: Maybe PointedDescriptorSet
  , envMapStrength :: Scalar
  , irradianceStrength :: Scalar
  } deriving Generic

data OverlayGlobals = OverlayGlobals
  { viewMat        :: M44 Scalar
  , projMat        :: M44 Scalar
  , viewProjMat    :: M44 Scalar
  } deriving Generic
    deriving anyclass GStorable

data PostConstants = PostConstants
  { exposure    :: Float
  , colorShift  :: V3 Float
  , saturation  :: Float
  , filmGrain   :: Float
  , falseColor  :: Bool
  } deriving Generic
    deriving anyclass GStorable

data SSAOSettings = SSAOSettings
  { kernelSize :: Word32
  , kernelRadius :: Scalar
  } deriving Generic
    deriving anyclass GStorable

data Features = Features
  { diffuse    :: Bool
  , specular   :: Bool
  , ssao       :: Bool
  , shadows    :: Bool
  } deriving (Show, Read, Generic)

{- -}

postDefaults :: PostConstants
postDefaults = PostConstants
  { exposure    = 0
  , colorShift  = V3 1 1 1
  , saturation  = 1
  , filmGrain   = 0
  , falseColor  = False
  }

data RenderTargets = RenderTargets
  -- Stage 1 Shadows
  { shadowRenderConfig           :: !RenderConfig
  , cascadedShadowMap            :: FramedResource (VS.Vector MaxShadowCascadesNat (Framebuffer, DescriptorSpec), DescriptorSpec)
  -- Stage 2 Current Selection
  , currentSelectionRenderConfig         :: !RenderConfig
  , currentSelectionRenderFrame  :: FramedResource (Framebuffer, DescriptorSpec)
  -- Stage 3 GBuffer
  , gbufferRenderConfig          :: !RenderConfig
  , gbufferRenderFrame           :: FramedResource Framebuffer
  -- Stage 4 Decals
  , decalRenderConfig            :: !RenderConfig
  , decalRenderFrame             :: FramedResource Framebuffer
  -- Stage 5 SSAO
  , ssaoRenderConfig             :: !RenderConfig
  , ssaoRenderFrame              :: FramedResource (Framebuffer, DescriptorSpec)
  -- Stage 6 Lighting
  , lightingRenderConfig         :: !RenderConfig
  , lightingRenderFrame          :: FramedResource (Framebuffer, DescriptorSpec)
  -- Stage 7 Post + Stage 9 Overlay
  , overlayRenderConfig          :: !RenderConfig
  -- Stage 8 Forward
  , directRenderConfig           :: !RenderConfig
  -- Etc
  , gbufferFloatDesc             :: FramedResource DescriptorSpec -- A set of 3 images (albedo, normal, depth)
  , gbufferUIntDesc              :: FramedResource DescriptorSpec -- A set of 1 image (objId)
  , decalDesc                    :: FramedResource [DescriptorSpec] -- ObjID and Depth
  }

data Renderer = Renderer
  { renderTargets :: RenderTargets
  -- , colorViewableImage :: FramedResource ViewableImage
  , depthViewableImage :: FramedResource ViewableImage
  , objIDViewableImage :: FramedResource ViewableImage

  -- Pipelines
  -- , currentSelectionMaterial   :: !(BufferedUniformMaterial Word32 ObjectIDConstants)
  -- , staticShadowMaterial       :: !(BufferedUniformMaterial ShadowPushConsts StaticConstants)
  -- , animatedShadowMaterial     :: !(BufferedUniformMaterial ShadowPushConsts AnimatedConstants)
  , staticGBufferMaterialConfig   :: MaterialConfig StaticConstants
  , animatedGBufferMaterialConfig :: MaterialConfig AnimatedConstants
  , staticDirectMaterialConfig    :: MaterialConfig StaticConstants
  , lineDirectMaterialConfig      :: MaterialConfig StaticConstants
  , pointDirectMaterialConfig     :: MaterialConfig StaticConstants
  , msdfMaterialConfig            :: MaterialConfig MSDFMatConstants
  , decalMaterialConfig           :: MaterialConfig DecalConstants

  {-
  , staticUnlitWorldMaterial   :: !(BufferedUniformMaterial Word32 StaticConstants)
  , msdfWorldMaterial          :: !(BufferedUniformMaterial Word32 MSDFMatConstants)
  , linesWorldMaterial         :: !(BufferedUniformMaterial Word32 StaticConstants)
  , pointsWorldMaterial        :: !(BufferedUniformMaterial Word32 StaticConstants)
  , staticOverlayMaterial      :: !(BufferedUniformMaterial Word32 StaticConstants)
  , msdfOverlayMaterial        :: !(BufferedUniformMaterial Word32 MSDFMatConstants)
  -}

  , objHighlightMaterial     :: !(Material Word32)
  , postProcessMaterial      :: !(Material PostConstants)
  , sunMaterial              :: !(Material Word32) -- Can't have () as a push constant so we have a placeholder
  , ssaoMaterial             :: !(Material SSAOSettings)
  , globalBuffer             :: !(FramedResource (DataBuffer Globals))
  , globalShadowPassBuffer   :: !(FramedResource (DataBuffer ShadowGlobals))
  , globalWorldBuffer        :: !(FramedResource (DataBuffer WorldGlobals))
  , globalOverlayBuffer      :: !(FramedResource (DataBuffer OverlayGlobals))
  , dynamicMesh              :: FramedResource DynamicBufferedMesh
  , objectPickingImageBuffer :: FramedResource ImageBuffer

  , globalDescriptorSet      :: FramedResource PointedDescriptorSet
  , shadowMapDescriptorSet   :: FramedResource PointedDescriptorSet
  , singleImageSetLayout     :: DescriptorSetLayout
  , uberImageSetLayout       :: DescriptorSetLayout
  , skinBuffer               :: FramedResource (DataBuffer (M44 Scalar))

  , defaultEnvMapDescriptorSet :: PointedDescriptorSet
  , defaultLutDescriptorSet    :: PointedDescriptorSet
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

data DrawConfig = forall uniform pushConsts. DrawConfig
  { mesh                 :: MeshType
  , perDrawDescriptorSet :: Maybe PointedDescriptorSet
  , material             :: Material pushConsts
  , materialDescriptor   :: FramedResource (MaterialDescriptorSet uniform)
  }

data DrawBatch = DrawBatch
  { firstInstanceIndex :: Word32
  , numInstances       :: Word32
  , meshSelector       :: Text -- MeshMember Name
  , pushConst          :: Word32
  , ordering           :: Int -- Indicates original draw order
  } deriving Generic

data DrawCommand = forall uniform. DrawCommand
  { materialConfig  :: MaterialConfig uniform
  , pokeData        :: Word32 -> Ptr uniform -> IO ()
  , mesh            :: MeshType
  -- This describes the actual draw commands we send to vulkan
  , instances       :: [(Text, [(Word32, M44 Float)])] -- MeshMember Name, [(Id, Transform)]
  , descriptorSet   :: Maybe PointedDescriptorSet
  , doBlend         :: Bool
  , doCastShadow    :: Bool
  , cull            :: Bool
  }

data MaterialConfig uniform
  = GBufferConfig (GBufferMaterialStack uniform)
  | DirectConfig (DirectMaterial uniform)
  | DecalConfig (DecalMaterial uniform)

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
  , material    :: V4 Float
  , tiling      :: V2 Float
  } deriving Generic
    deriving anyclass GStorable

data AnimatedConstants = AnimatedConstants
  { modelMat    :: M44 Float
  , normalMat   :: M33 Float
  , color       :: V4 Float
  , material    :: V4 Float
  , skinIdx     :: Word32
  , colors      :: VSS.Vector 6 (V4 Float)
  } deriving Generic
    deriving anyclass GStorable

data DecalConstants = DecalConstants
  { modelMat            :: M44 Float
  , normalMat           :: M33 Float
  , invModelViewProjMat :: M44 Float
  , color               :: V4 Float
  , receiverId          :: Word32
  } deriving Generic
    deriving anyclass GStorable

data ShadowPushConsts = ShadowPushConsts
  { cascadeIndex :: Word32
  } deriving Generic
    deriving anyclass GStorable

-- data GBufferPushConsts = GBufferPushConsts
--   { uniformIndex :: Word32
--   } deriving Generic
--     deriving anyclass GStorable

data MaterialDescriptorSet a = MaterialDescriptorSet
  { descriptorSet   :: PointedDescriptorSet
  -- Each unique instance has a uniform struct
  , uniformBuffer   :: DataBuffer a
  -- Each unique instance has an id
  , idBuffer        :: DataBuffer Word32
  -- Each draw call has a list of indices (that index into the uniformBuffer)
  , instancesBuffer :: DataBuffer Word32
  } deriving Generic

data GBufferMaterialStack uniform = GBufferMaterialStack
  { gbufferMaterial          :: Material Word32 -- Unused push consts
  , shadowMaterial           :: Material ShadowPushConsts
  , showSelectionMaterial    :: Material Word32 -- Unused push consts
  , descriptor               :: FramedResource (MaterialDescriptorSet uniform)
  , uniformSize              :: Int -- Bytes
  , uuid                     :: UUID
  }

data DirectStage = WorldDirect | OverlayDirect
  deriving (Eq)

data DirectMaterial uniform = DirectMaterial
  { directMaterial  :: Material Word32
  , overlayMaterial :: Material Word32
  , descriptor      :: FramedResource (MaterialDescriptorSet uniform)
  , uniformSize     :: Int -- Bytes
  , uuid            :: UUID
  }

data DecalMaterial uniform = DecalMaterial
  { material  :: Material Word32
  , descriptor      :: FramedResource (MaterialDescriptorSet uniform)
  , uniformSize     :: Int -- Bytes
  , uuid            :: UUID
  }

data WorldGlobals = WorldGlobals
  { viewMat        :: M44 Scalar
  , projMat        :: M44 Scalar
  , viewProjMat    :: M44 Scalar
  , invViewMat     :: M44 Scalar
  , invProjMat     :: M44 Scalar
  , camPos         :: V3 Scalar
  , envMapStrength :: Scalar
  , lightDirection :: V3 Scalar
  , irradianceStrength :: Scalar
  , sunColor       :: V3 Scalar -- HDR
  , padding3       :: Scalar
  , ambientColor   :: V3 Scalar -- HDR
  , padding4       :: Scalar
  , gbufferSize    :: V2 Scalar
  , multiSampleCount :: Scalar
  , nearPlane      :: Scalar
  , farPlane       :: Scalar
  , diffuseMask    :: Scalar
  , specularMask   :: Scalar
  , ssaoMask       :: Scalar
  , shadowsMask    :: Scalar
  } deriving Generic
    deriving anyclass GStorable

data ShadowGlobals = ShadowGlobals
  { viewProjMats    :: VS.Vector MaxShadowCascadesNat (M44 Scalar)
  , splitDepths     :: VS.Vector MaxShadowCascadesNat Scalar -- Far plane of each cascade
  , shadowBiasSlope :: Scalar
  } deriving Generic
    deriving anyclass GStorable

worldSettingsDefaults :: WorldSettings
worldSettingsDefaults = WorldSettings {..}
  where
  camera = Camera zero (V3 (-1) (-1) (-1)) (V3 0 0 1) (Perspective (pi/4) 0.1 100) "DefaultWorldSettings"
  lightTransform = identity
  lightDirection = V3 1 1 1
  sunColor = V3 1 1 1
  ambientColor = V3 1 1 1
  envMapStrength = 1
  envMap = Nothing
  irradianceStrength = 0.2

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

debugName :: (MonadIO io, HasObjectType p) => VulkanResources -> p -> ByteString -> io ()
debugName vulkanResources a name =
  let (otype, handle) = objectTypeAndHandle a
  in setDebugUtilsObjectNameEXT vulkanResources.deviceContext.device (DebugUtilsObjectNameInfoEXT otype handle (Just name))
