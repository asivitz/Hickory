{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels, OverloadedRecordDot #-}

module Hickory.FRP.Editor.Post where

import DearImGui
    ( withMenuBarOpen,
      withMenuOpen,
      menuItem,
      dragFloat3,
      dragFloat,
      colorEdit3, ImVec2 (..), ImVec4 (..), image, collapsingHeader, dragInt, checkbox )
import Data.IORef ( IORef, readIORef, modifyIORef' )
import GHC.Generics (Generic)
import Hickory.ImGUI.Helpers (myWithWindow, v3TripleIso)
import Control.Monad.Extra (whenM)
import Hickory.Math (Scalar)
import Linear (V3 (..))
import Control.Monad (void)
import Hickory.Vulkan.Renderer.Types (Renderer(..), RenderTargets(..), Features (..))
import Vulkan (Extent2D (..), objectTypeAndHandle)
import Foreign (with, castPtr, wordPtrToPtr, WordPtr(..))
import Control.Lens (view, set, Lens')
import Hickory.Vulkan.Types (FrameContext(..), RenderConfig(..))
import Hickory.Vulkan.Framing (resourceForFrame)
import Data.Bits
import Data.StateVar (makeStateVar, StateVar)
import Hickory.ImGUI.Helpers (v3ImVec3Iso)

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

drawPostUI :: IORef GraphicsParams -> (Renderer, FrameContext) -> IO ()
drawPostUI graphicsParamsRef (Renderer {..}, FrameContext {..}) = do
  void $ myWithWindow "Post Processing" do
    withMenuBarOpen do
      withMenuOpen "File" do
        whenM (menuItem "Save Post Parameters") do
          readIORef graphicsParamsRef >>= writeFile "post.txt" . show

    void $ checkbox "False Color" (mkVar #falseColor)
    void $ checkbox "Apply Lut" (mkVar #applyLut)
    void $ dragFloat "Exposure" (mkVar #exposure) 0.05 (-10) 10
    void $ colorEdit3 "ColorShift" (mkVar (#colorShift . v3ImVec3Iso))
    void $ dragFloat "Saturation" (mkVar #saturation) 0.05 0 2
    void $ dragFloat "Film Grain" (mkVar #filmGrain) 0.01 0 1
    void $ colorEdit3 "Ambient Light" (mkVar (#ambientLight . v3ImVec3Iso))
    void $ dragFloat "Ambient Strength" (mkVar #ambientStrength) 0.1 0 10
    void $ dragFloat "Env Map Strength" (mkVar #envMapStrength) 0.1 0 10
    void $ dragFloat "Irradiance Strength" (mkVar #irradianceStrength) 0.1 0 10
    void $ colorEdit3 "Sun Light" (mkVar (#sunLight . v3ImVec3Iso))
    void $ dragFloat "Sun Strength" (mkVar #sunStrength) 0.1 0 100
    void $ dragFloat3 "Sun Direction" (mkVar (#sunDirection . v3TripleIso)) 0.1 (-100) 100

    whenM (collapsingHeader "SSAO Config" (Just True)) do
      void $ dragInt "Kernel Size" (mkVar #ssaoKernelSize) 1 0 64
      void $ dragFloat "Kernel Radius" (mkVar #ssaoKernelRadius) 0.01 0 5

    void $ dragFloat "Shadow Bias Slope" (mkVar #shadowBiasSlope) 0.001 0 1

    whenM (collapsingHeader "Shadowmap Cascades" (Just True)) do
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

    whenM (collapsingHeader "Features" (Just True)) do
      void $ checkbox "Diffuse" (mkVar (#features . #diffuse))
      void $ checkbox "Specular" (mkVar (#features . #specular))
      void $ checkbox "SSAO" (mkVar (#features . #ssao))
      void $ checkbox "Shadows" (mkVar (#features . #shadows))
    pure Nothing
  where
  mkVar :: Lens' GraphicsParams a -> StateVar a
  mkVar l = makeStateVar (view l <$> readIORef graphicsParamsRef) (\a -> modifyIORef' graphicsParamsRef $ set l a)
