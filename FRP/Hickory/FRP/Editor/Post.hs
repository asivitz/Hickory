{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels, OverloadedRecordDot #-}

module Hickory.FRP.Editor.Post where

import DearImGui
    ( ImVec3,
      withMenuBarOpen,
      withMenuOpen,
      menuItem,
      dragFloat3,
      dragFloat,
      colorEdit3, ImVec2 (..), ImVec4 (..), image, withCollapsingHeaderOpen, dragInt, checkbox )
import Data.IORef ( IORef, readIORef, newIORef )
import GHC.Generics (Generic)
import Hickory.ImGUI.Helpers (imVec3ToV3, tripleToV3, v3ToImVec3, v3ToTriple, myWithWindow)
import Control.Monad.Extra (whenM)
import Hickory.Math (Scalar)
import Linear (V3 (..))
import Control.Monad (void)
import Hickory.Vulkan.Renderer.Types (Renderer(..), RenderTargets(..), Features (..))
import Vulkan (Extent2D (..), objectTypeAndHandle)
import Foreign (with, castPtr, wordPtrToPtr, WordPtr(..))
import Control.Lens (view)
import Hickory.Vulkan.Types (FrameContext(..), RenderConfig(..))
import Hickory.Vulkan.Framing (resourceForFrame)
import Data.Bits

data PostEditorState = PostEditorState
  { exposureRef        :: IORef Float
  , colorShiftRef      :: IORef ImVec3
  , saturationRef      :: IORef Scalar
  , filmGrainRef       :: IORef Scalar
  , ambientLightRef    :: IORef ImVec3
  , ambientStrengthRef :: IORef Scalar
  , envMapStrengthRef  :: IORef Scalar
  , irradianceStrengthRef  :: IORef Scalar
  , sunLightRef        :: IORef ImVec3
  , sunStrengthRef     :: IORef Scalar
  , sunDirectionRef    :: IORef (Float, Float, Float)
  , ssaoKernelSizeRef  :: IORef Int
  , ssaoKernelRadiusRef :: IORef Scalar
  , shadowBiasSlopeRef    :: IORef Scalar
  , diffuseRef         :: IORef Bool
  , specularRef        :: IORef Bool
  , ssaoRef            :: IORef Bool
  , shadowsRef         :: IORef Bool
  , falseColorRef      :: IORef Bool
  , applyLutRef        :: IORef Bool
  }

data GraphicsParams = GraphicsParams
  { exposure        :: Scalar
  , colorShift      :: V3 Scalar
  , saturation      :: Scalar
  , filmGrain       :: Scalar
  , ambientLight    :: V3 Scalar
  , ambientStrength :: Scalar
  , envMapStrength  :: Scalar
  , irradianceStrength :: Scalar
  , sunLight        :: V3 Scalar
  , sunStrength     :: Scalar
  , sunDirection    :: V3 Scalar
  , ssaoKernelSize  :: Int
  , ssaoKernelRadius :: Scalar
  , shadowBiasSlope :: Scalar
  , features        :: Features
  , falseColor      :: Bool
  , applyLut        :: Bool
  } deriving (Show, Read, Generic)

defaultGraphicsParams :: GraphicsParams
defaultGraphicsParams = GraphicsParams
    { exposure        = 0
    , colorShift      = V3 1 1 1
    , saturation      = 1
    , filmGrain       = 0
    , ambientLight    = V3 1 1 1
    , ambientStrength = 1
    , envMapStrength  = 1
    , irradianceStrength  = 1
    , sunLight        = V3 1 1 1
    , sunStrength     = 1
    , sunDirection    = V3 (-1) (-1) (-6)
    , ssaoKernelSize  = 16
    , ssaoKernelRadius = 0.5
    , shadowBiasSlope = 0
    , features = Features
      { diffuse  = True
      , specular = True
      , ssao     = True
      , shadows  = True
      }
    , falseColor = False
    , applyLut   = True
    }

readGraphicsParams :: PostEditorState -> IO GraphicsParams
readGraphicsParams PostEditorState {..} =
  GraphicsParams
    <$> readIORef exposureRef
    <*> (imVec3ToV3 <$> readIORef colorShiftRef)
    <*> readIORef saturationRef
    <*> readIORef filmGrainRef
    <*> (imVec3ToV3 <$> readIORef ambientLightRef)
    <*> readIORef ambientStrengthRef
    <*> readIORef envMapStrengthRef
    <*> readIORef irradianceStrengthRef
    <*> (imVec3ToV3 <$> readIORef sunLightRef)
    <*> readIORef sunStrengthRef
    <*> (tripleToV3 <$> readIORef sunDirectionRef)
    <*> readIORef ssaoKernelSizeRef
    <*> readIORef ssaoKernelRadiusRef
    <*> readIORef shadowBiasSlopeRef
    <*> readFeatures
    <*> readIORef falseColorRef
    <*> readIORef applyLutRef
  where
  readFeatures
    = Features <$> readIORef diffuseRef
               <*> readIORef specularRef
               <*> readIORef ssaoRef
               <*> readIORef shadowsRef

mkPostEditorState :: GraphicsParams -> IO PostEditorState
mkPostEditorState GraphicsParams {..} = do
  exposureRef        <- newIORef exposure
  colorShiftRef      <- newIORef (v3ToImVec3 colorShift)
  saturationRef      <- newIORef saturation
  filmGrainRef       <- newIORef filmGrain
  ambientLightRef    <- newIORef (v3ToImVec3 ambientLight)
  ambientStrengthRef <- newIORef ambientStrength
  envMapStrengthRef  <- newIORef envMapStrength
  irradianceStrengthRef  <- newIORef irradianceStrength
  sunLightRef        <- newIORef (v3ToImVec3 sunLight)
  sunStrengthRef     <- newIORef sunStrength
  sunDirectionRef    <- newIORef (v3ToTriple sunDirection)

  ssaoKernelSizeRef    <- newIORef ssaoKernelSize
  ssaoKernelRadiusRef    <- newIORef ssaoKernelRadius

  shadowBiasSlopeRef    <- newIORef shadowBiasSlope

  diffuseRef  <- newIORef features.diffuse
  specularRef <- newIORef features.specular
  ssaoRef     <- newIORef features.ssao
  shadowsRef  <- newIORef features.shadows
  falseColorRef  <- newIORef falseColor
  applyLutRef  <- newIORef applyLut

  pure PostEditorState {..}

drawPostUI :: PostEditorState -> (Renderer, FrameContext) -> IO ()
drawPostUI pes@PostEditorState {..} (Renderer {..}, FrameContext {..}) = do
  myWithWindow "Post Processing" do
    withMenuBarOpen do
      withMenuOpen "File" do
        whenM (menuItem "Save Post Parameters") do
          readGraphicsParams pes >>= writeFile "post.txt" . show

    void $ checkbox "False Color" falseColorRef
    void $ checkbox "Apply Lut" applyLutRef
    void $ dragFloat "Exposure" exposureRef 0.05 (-10) 10
    void $ colorEdit3 "ColorShift" colorShiftRef
    void $ dragFloat "Saturation" saturationRef 0.05 0 2
    void $ dragFloat "Film Grain" filmGrainRef 0.01 0 1
    void $ colorEdit3 "Ambient Light" ambientLightRef
    void $ dragFloat "Ambient Strength" ambientStrengthRef 0.1 0 10
    void $ dragFloat "Env Map Strength" envMapStrengthRef 0.1 0 10
    void $ dragFloat "Irradiance Strength" irradianceStrengthRef 0.1 0 10
    void $ colorEdit3 "Sun Light" sunLightRef
    void $ dragFloat "Sun Strength" sunStrengthRef 0.1 0 100
    void $ dragFloat3 "Sun Direction" sunDirectionRef 0.1 (-100) 100

    withCollapsingHeaderOpen "SSAO Config" zeroBits do
      void $ dragInt "Kernel Size" ssaoKernelSizeRef 1 0 64
      void $ dragFloat "Kernel Radius" ssaoKernelRadiusRef 0.01 0 5

    void $ dragFloat "Shadow Bias Slope" shadowBiasSlopeRef 0.001 0 1

    withCollapsingHeaderOpen "Shadowmap Cascades" zeroBits do
      let RenderTargets {..} = renderTargets
          Extent2D w h = shadowRenderConfig.extent
          desSetHandle = snd $ objectTypeAndHandle (view #descriptorSet (resourceForFrame swapchainImageIndex shadowMapDescriptorSet))
          imagePtr = wordPtrToPtr (WordPtr $ fromIntegral desSetHandle)
      with (ImVec2 (realToFrac w / 4) (realToFrac h / 4)) \sizeptr ->
        with (ImVec2 0 0) \uv0 ->
        with (ImVec2 1 1) \uv1 ->
        with (ImVec4 1 1 1 1) \tintCol ->
        with (ImVec4 0 0 0 0) \borderCol ->
          image (castPtr imagePtr) sizeptr uv0 uv1 tintCol borderCol

    withCollapsingHeaderOpen "Features" zeroBits do
      void $ checkbox "Diffuse" diffuseRef
      void $ checkbox "Specular" specularRef
      void $ checkbox "SSAO" ssaoRef
      void $ checkbox "Shadows" shadowsRef
