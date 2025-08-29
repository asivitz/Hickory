{-# LANGUAGE DataKinds, OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts, OverloadedRecordDot, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Renderer.StockMaterials where

import Hickory.Vulkan.Renderer.Types (StaticConstants (..), AnimatedConstants (..), RenderTargets (..), GBufferMaterialStack(..), DirectMaterial(..), MaterialConfig (..), MaterialDescriptorSet (..), DecalMaterial (..), DecalConstants)
import Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import Hickory.Vulkan.Types (DescriptorSpec (..), PointedDescriptorSet, buf, Material(..), VulkanResources (..), DataBuffer (..), DeviceContext (..))
import qualified Hickory.Vulkan.Types as HVT
import Hickory.Vulkan.Text (MSDFMatConstants (..), msdfVertShader, msdfFragShader)
import Hickory.Vulkan.Renderer.GBuffer (staticGBufferVertShader, staticGBufferFragShader, animatedGBufferVertShader, animatedGBufferFragShader, staticGBufferShadowVertShader, animatedGBufferShadowVertShader)
import Hickory.Vulkan.Renderer.ShadowPass (noColorFragShader)
import Vulkan (BufferUsageFlagBits (..), DescriptorSetLayout, CullModeFlagBits (..), PrimitiveTopology (..))
import Foreign (Storable, sizeOf)
import Hickory.Vulkan.DescriptorSet (withDescriptorSet, withDataBuffer)
import Control.Lens (view)
import Hickory.Vulkan.Framing (frameResource, FramedResource)
import Hickory.Vulkan.Material (PipelineOptions (..), withMaterial, pipelineDefaults, noBlend, defaultBlend, colorBlendLeaveAlpha)
import qualified Data.Vector as V
import qualified Hickory.Vulkan.Renderer.ObjectPicking as OP
import Linear.Matrix (M44)
import Hickory.Math (Scalar)
import Data.ByteString (ByteString)
import Data.UUID.V4 (nextRandom)
import Hickory.Vulkan.Renderer.ShaderDefinitions (buildDirectVertShader, buildOverlayVertShader)
import Hickory.Vulkan.Renderer.Direct (staticDirectVertShader, staticDirectFragShader, simpleFragShader)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ)
import Data.Functor ((<&>))
import Hickory.Vulkan.Renderer.Decals (decalVertShader, decalFragShader)
import Hickory.Vulkan.Renderer.Direct (lineVertShader)
import Hickory.Vulkan.Renderer.Direct (pointVertShader)
import Hickory.Vulkan.Vulkan (debugName)

standardMaxNumDraws :: Num a => a
standardMaxNumDraws = 2048

withGBufferMaterialStack
  :: forall uniform
  .  Storable uniform
  => VulkanResources
  -> RenderTargets
  -> FramedResource PointedDescriptorSet
  -> Maybe (FramedResource [DescriptorSpec])
  -> Int
  -> PipelineOptions
  -> [HVT.Attribute]
  -> Maybe DescriptorSetLayout -- Per draw descriptor set
  -> ByteString
  -> ByteString
  -> ByteString
  -> ByteString
  -> Acquire (MaterialConfig uniform)
withGBufferMaterialStack vulkanResources@VulkanResources { deviceContext = DeviceContext {..} } RenderTargets {..} globalDescriptorSet extraMaterialDescriptors maxNumDraws pipelineOptions attributes perDrawLayout
  gbufferVertShader gbufferFragShader
  shadowVertShader shadowFragShader
  = GBufferConfig <$> do
  uuid <- liftIO nextRandom
  extras <- maybe (frameResource $ pure []) pure extraMaterialDescriptors
  descriptor <- flip V.mapM extras \descs -> do
    uniformBuffer   <- withDataBuffer vulkanResources "GBufferUniform" maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    idBuffer        <- withDataBuffer vulkanResources "GBufferID" maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    instancesBuffer <- withDataBuffer vulkanResources "GBufferInstances" maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    descriptorSet <- withDescriptorSet vulkanResources $
      [ BufferDescriptor uniformBuffer.size uniformBuffer.buf
      , BufferDescriptor idBuffer.size idBuffer.buf
      , BufferDescriptor instancesBuffer.size instancesBuffer.buf
      ] ++ descs
    pure MaterialDescriptorSet {..}

  let uniformSize = sizeOf (undefined :: uniform)
      materialSet = view #descriptorSet <$> descriptor
      materialSets =
        [ globalDescriptorSet
        , materialSet
        ]

  gbufferMaterial <- withMaterial vulkanResources "Gbuffer" gbufferRenderConfig attributes pipelineOptions pipelineOptions.cullMode gbufferVertShader gbufferFragShader materialSets perDrawLayout
  shadowMaterial  <- withMaterial vulkanResources "Shadow" shadowRenderConfig attributes pipelineOptions { depthClampEnable = True } pipelineOptions.shadowCullMode shadowVertShader shadowFragShader materialSets Nothing
  showSelectionMaterial <- withMaterial vulkanResources "ShowSelection" currentSelectionRenderConfig attributes pipelineOptions { colorBlends = [noBlend]} pipelineOptions.cullMode gbufferVertShader OP.objectIDFragShader materialSets Nothing

  debugName device gbufferMaterial.pipeline "GBuffer"
  debugName device gbufferMaterial.pipelineLayout "GBuffer"

  debugName device shadowMaterial.pipeline "Shadow"
  debugName device shadowMaterial.pipelineLayout "Shadow"

  debugName device showSelectionMaterial.pipeline "Show Sel"
  debugName device showSelectionMaterial.pipelineLayout "Show Sel"
  pure GBufferMaterialStack {..}

withStaticGBufferMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig StaticConstants)
withStaticGBufferMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout =
  withGBufferMaterialStack vulkanResources renderTargets globalPDS Nothing standardMaxNumDraws (pipelineDefaults [noBlend, noBlend, noBlend, noBlend]) [HVT.Position, HVT.Normal, HVT.TextureCoord, HVT.Tangent] perDrawLayout staticGBufferVertShader staticGBufferFragShader staticGBufferShadowVertShader noColorFragShader

withAnimatedGBufferMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig AnimatedConstants, FramedResource (DataBuffer (M44 Scalar)))
withAnimatedGBufferMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout = do
  skinBuffer :: FramedResource (DataBuffer (M44 Scalar))
    <- frameResource $ withDataBuffer vulkanResources "Skin" (66 * 14) BUFFER_USAGE_UNIFORM_BUFFER_BIT -- TODO: Enough for 14 skins, but should be dynamic
  let descs = skinBuffer <&> \buffer -> [BufferDescriptor buffer.size buffer.buf]
  config <- withGBufferMaterialStack vulkanResources renderTargets globalPDS (Just descs) standardMaxNumDraws (pipelineDefaults [noBlend, noBlend, noBlend, noBlend]) [HVT.Position, HVT.Normal, HVT.TextureCoord, HVT.Tangent, HVT.JointIndices, HVT.JointWeights] perDrawLayout animatedGBufferVertShader animatedGBufferFragShader animatedGBufferShadowVertShader noColorFragShader
  pure (config, skinBuffer)

withDecalMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig DecalConstants)
withDecalMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout =
  withDecalMaterialStack vulkanResources renderTargets globalPDS (Just renderTargets.decalDesc) standardMaxNumDraws
    ((pipelineDefaults [colorBlendLeaveAlpha, defaultBlend, defaultBlend ]) { cullMode = CULL_MODE_FRONT_BIT }) [HVT.Position] perDrawLayout decalFragShader

withDirectMaterialStack
  :: forall uniform
  .  Storable uniform
  => VulkanResources
  -> RenderTargets
  -> FramedResource PointedDescriptorSet
  -> Int
  -> PipelineOptions
  -> [HVT.Attribute]
  -> Maybe DescriptorSetLayout -- Per draw descriptor set
  -- -> DirectStage
  -> ByteString
  -> ByteString
  -> ByteString
  -> Acquire (MaterialConfig uniform)
withDirectMaterialStack vulkanResources RenderTargets {..} globalDescriptorSet maxNumDraws pipelineOptions attributes perDrawLayout
  directVertShader overlayVertShader fragShader
  = DirectConfig <$> do
  uuid <- liftIO nextRandom
  descriptor <- frameResource do
    uniformBuffer   <- withDataBuffer vulkanResources "DirectUniform" maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    idBuffer        <- withDataBuffer vulkanResources "DirectID" maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    instancesBuffer <- withDataBuffer vulkanResources "DirectInstances" maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    descriptorSet <- withDescriptorSet vulkanResources [ BufferDescriptor uniformBuffer.size uniformBuffer.buf
                                                       , BufferDescriptor idBuffer.size idBuffer.buf
                                                       , BufferDescriptor instancesBuffer.size instancesBuffer.buf]
    pure MaterialDescriptorSet {..}

  let uniformSize = sizeOf (undefined :: uniform)
      materialSet = view #descriptorSet <$> descriptor
      materialSets =
        [ globalDescriptorSet
        , materialSet
        ]

  directMaterial  <- withMaterial vulkanResources "Direct" directRenderConfig  attributes pipelineOptions pipelineOptions.cullMode directVertShader fragShader materialSets perDrawLayout
  overlayMaterial <- withMaterial vulkanResources "Overlay" overlayRenderConfig attributes pipelineOptions pipelineOptions.cullMode overlayVertShader fragShader materialSets perDrawLayout
  pure DirectMaterial {..}

withStaticDirectMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig StaticConstants)
withStaticDirectMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout =
  withDirectMaterialStack vulkanResources renderTargets globalPDS standardMaxNumDraws (pipelineDefaults [defaultBlend]) [HVT.Position, HVT.TextureCoord] perDrawLayout
    $(compileShaderQ Nothing "vert" Nothing (buildDirectVertShader staticDirectVertShader))
    $(compileShaderQ Nothing "vert" Nothing (buildOverlayVertShader staticDirectVertShader))
    staticDirectFragShader

withLineDirectMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Acquire (MaterialConfig StaticConstants)
withLineDirectMaterialConfig vulkanResources renderTargets globalPDS =
  withDirectMaterialStack vulkanResources renderTargets globalPDS standardMaxNumDraws pipelineOptions [HVT.Position] Nothing
    $(compileShaderQ Nothing "vert" Nothing (buildDirectVertShader lineVertShader))
    $(compileShaderQ Nothing "vert" Nothing (buildOverlayVertShader lineVertShader))
    simpleFragShader
  where
  pipelineOptions = (pipelineDefaults [defaultBlend]) { primitiveTopology = PRIMITIVE_TOPOLOGY_LINE_LIST, depthTestEnable = False }

withPointDirectMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Acquire (MaterialConfig StaticConstants)
withPointDirectMaterialConfig vulkanResources renderTargets globalPDS =
  withDirectMaterialStack vulkanResources renderTargets globalPDS standardMaxNumDraws pipelineOptions [HVT.Position] Nothing
    $(compileShaderQ Nothing "vert" Nothing (buildDirectVertShader pointVertShader))
    $(compileShaderQ Nothing "vert" Nothing (buildOverlayVertShader pointVertShader))
    simpleFragShader
  where
  pipelineOptions = (pipelineDefaults [defaultBlend]) { primitiveTopology = PRIMITIVE_TOPOLOGY_POINT_LIST, depthTestEnable = False }

withMSDFMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig MSDFMatConstants)
withMSDFMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout =
  withDirectMaterialStack vulkanResources renderTargets globalPDS standardMaxNumDraws (pipelineDefaults [defaultBlend]) [HVT.Position, HVT.TextureCoord] perDrawLayout
    $(compileShaderQ Nothing "vert" Nothing (buildDirectVertShader msdfVertShader))
    $(compileShaderQ Nothing "vert" Nothing (buildOverlayVertShader msdfVertShader))
    msdfFragShader

withDecalMaterialStack
  :: forall uniform
  .  Storable uniform
  => VulkanResources
  -> RenderTargets
  -> FramedResource PointedDescriptorSet
  -> Maybe (FramedResource [DescriptorSpec])
  -> Int
  -> PipelineOptions
  -> [HVT.Attribute]
  -> Maybe DescriptorSetLayout -- Per draw descriptor set
  -> ByteString
  -> Acquire (MaterialConfig uniform)
withDecalMaterialStack vulkanResources RenderTargets {..} globalDescriptorSet extraMaterialDescriptors maxNumDraws pipelineOptions attributes perDrawLayout fragShader
  = DecalConfig <$> do
  uuid <- liftIO nextRandom
  extras <- maybe (frameResource $ pure []) pure extraMaterialDescriptors
  descriptor <- flip V.mapM extras \descs -> do
    uniformBuffer   <- withDataBuffer vulkanResources "DecalUniform" maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    idBuffer        <- withDataBuffer vulkanResources "DecalID" maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    instancesBuffer <- withDataBuffer vulkanResources "DecalInstances" maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    descriptorSet <- withDescriptorSet vulkanResources $
      [ BufferDescriptor uniformBuffer.size uniformBuffer.buf
      , BufferDescriptor idBuffer.size idBuffer.buf
      , BufferDescriptor instancesBuffer.size instancesBuffer.buf
      ] ++ descs
    pure MaterialDescriptorSet {..}

  let uniformSize = sizeOf (undefined :: uniform)
      materialSet = view #descriptorSet <$> descriptor
      materialSets =
        [ globalDescriptorSet
        , materialSet
        ]

  material <- withMaterial vulkanResources "Decal" decalRenderConfig attributes pipelineOptions pipelineOptions.cullMode decalVertShader fragShader materialSets perDrawLayout
  pure DecalMaterial {..}

