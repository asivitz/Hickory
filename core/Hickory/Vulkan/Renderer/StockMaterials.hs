{-# LANGUAGE DataKinds, OverloadedLists, OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts, OverloadedRecordDot, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hickory.Vulkan.Renderer.StockMaterials where

import Hickory.Vulkan.Renderer.Types (StaticConstants (..), AnimatedConstants (..), RenderTargets (..), GBufferMaterialStack(..), DirectMaterial(..), MaterialConfig (..), MaterialDescriptorSet (..), DecalMaterial (..), DecalConstants, debugName)
import Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import Hickory.Vulkan.Types (DescriptorSpec (..), PointedDescriptorSet, buf, Material(..), VulkanResources (..), DataBuffer (..))
import qualified Hickory.Vulkan.Types as HVT
import Hickory.Vulkan.Text (MSDFMatConstants (..), msdfVertShader, msdfFragShader)
import Hickory.Vulkan.Renderer.GBuffer (staticGBufferVertShader, staticGBufferFragShader, animatedGBufferVertShader, animatedGBufferFragShader, staticGBufferShadowVertShader, animatedGBufferShadowVertShader)
import Hickory.Vulkan.Renderer.ShadowPass (whiteFragShader)
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
withGBufferMaterialStack vulkanResources RenderTargets {..} globalDescriptorSet extraMaterialDescriptors maxNumDraws pipelineOptions attributes perDrawLayout
  gbufferVertShader gbufferFragShader
  shadowVertShader shadowFragShader
  = GBufferConfig <$> do
  uuid <- liftIO nextRandom
  extras <- maybe (frameResource $ pure []) pure extraMaterialDescriptors
  descriptor <- flip V.mapM extras \descs -> do
    uniformBuffer   <- withDataBuffer vulkanResources maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    idBuffer        <- withDataBuffer vulkanResources maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    instancesBuffer <- withDataBuffer vulkanResources maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    descriptorSet <- withDescriptorSet vulkanResources $
      [ BufferDescriptor (buf uniformBuffer)
      , BufferDescriptor (buf idBuffer)
      , BufferDescriptor (buf instancesBuffer)
      ] ++ descs
    pure MaterialDescriptorSet {..}

  let uniformSize = sizeOf (undefined :: uniform)
      materialSet = view #descriptorSet <$> descriptor
      materialSets =
        [ globalDescriptorSet
        , materialSet
        ]

  gbufferMaterial <- withMaterial vulkanResources gbufferRenderConfig attributes pipelineOptions pipelineOptions.cullMode gbufferVertShader gbufferFragShader materialSets perDrawLayout
  shadowMaterial  <- withMaterial vulkanResources shadowRenderConfig attributes pipelineOptions { depthClampEnable = True } pipelineOptions.shadowCullMode shadowVertShader shadowFragShader materialSets Nothing
  showSelectionMaterial <- withMaterial vulkanResources currentSelectionRenderConfig attributes pipelineOptions { colorBlends = [noBlend]} pipelineOptions.cullMode gbufferVertShader OP.objectIDFragShader materialSets Nothing

  debugName vulkanResources gbufferMaterial.pipeline "GBuffer"
  debugName vulkanResources gbufferMaterial.pipelineLayout "GBuffer"

  debugName vulkanResources shadowMaterial.pipeline "Shadow"
  debugName vulkanResources shadowMaterial.pipelineLayout "Shadow"

  debugName vulkanResources showSelectionMaterial.pipeline "Show Sel"
  debugName vulkanResources showSelectionMaterial.pipelineLayout "Show Sel"
  pure GBufferMaterialStack {..}

withStaticGBufferMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig StaticConstants)
withStaticGBufferMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout =
  withGBufferMaterialStack vulkanResources renderTargets globalPDS Nothing standardMaxNumDraws (pipelineDefaults [noBlend, noBlend, noBlend, noBlend]) [HVT.Position, HVT.Normal, HVT.TextureCoord, HVT.Tangent] perDrawLayout staticGBufferVertShader staticGBufferFragShader staticGBufferShadowVertShader whiteFragShader

withAnimatedGBufferMaterialConfig :: VulkanResources -> RenderTargets -> FramedResource PointedDescriptorSet -> Maybe DescriptorSetLayout -> Acquire (MaterialConfig AnimatedConstants, FramedResource (DataBuffer (M44 Scalar)))
withAnimatedGBufferMaterialConfig vulkanResources renderTargets globalPDS perDrawLayout = do
  skinBuffer :: FramedResource (DataBuffer (M44 Scalar))
    <- frameResource $ withDataBuffer vulkanResources (66 * 14) BUFFER_USAGE_UNIFORM_BUFFER_BIT -- TODO: Enough for 14 skins, but should be dynamic
  let descs = skinBuffer <&> \buffer -> [BufferDescriptor buffer.buf]
  config <- withGBufferMaterialStack vulkanResources renderTargets globalPDS (Just descs) standardMaxNumDraws (pipelineDefaults [noBlend, noBlend, noBlend, noBlend]) [HVT.Position, HVT.Normal, HVT.TextureCoord, HVT.Tangent, HVT.JointIndices, HVT.JointWeights] perDrawLayout animatedGBufferVertShader animatedGBufferFragShader animatedGBufferShadowVertShader whiteFragShader
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
    uniformBuffer   <- withDataBuffer vulkanResources maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    idBuffer        <- withDataBuffer vulkanResources maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    instancesBuffer <- withDataBuffer vulkanResources maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    descriptorSet <- withDescriptorSet vulkanResources [BufferDescriptor (buf uniformBuffer), BufferDescriptor (buf idBuffer), BufferDescriptor (buf instancesBuffer)]
    pure MaterialDescriptorSet {..}

  let uniformSize = sizeOf (undefined :: uniform)
      materialSet = view #descriptorSet <$> descriptor
      materialSets =
        [ globalDescriptorSet
        , materialSet
        ]

  directMaterial  <- withMaterial vulkanResources directRenderConfig  attributes pipelineOptions pipelineOptions.cullMode directVertShader fragShader materialSets perDrawLayout
  overlayMaterial <- withMaterial vulkanResources overlayRenderConfig attributes pipelineOptions pipelineOptions.cullMode overlayVertShader fragShader materialSets perDrawLayout
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
    uniformBuffer   <- withDataBuffer vulkanResources maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    idBuffer        <- withDataBuffer vulkanResources maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    instancesBuffer <- withDataBuffer vulkanResources maxNumDraws BUFFER_USAGE_UNIFORM_BUFFER_BIT
    descriptorSet <- withDescriptorSet vulkanResources $
      [ BufferDescriptor (buf uniformBuffer)
      , BufferDescriptor (buf idBuffer)
      , BufferDescriptor (buf instancesBuffer)
      ] ++ descs
    pure MaterialDescriptorSet {..}

  let uniformSize = sizeOf (undefined :: uniform)
      materialSet = view #descriptorSet <$> descriptor
      materialSets =
        [ globalDescriptorSet
        , materialSet
        ]

  material <- withMaterial vulkanResources decalRenderConfig attributes pipelineOptions pipelineOptions.cullMode decalVertShader fragShader materialSets perDrawLayout
  pure DecalMaterial {..}

