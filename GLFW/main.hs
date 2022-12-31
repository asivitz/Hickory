{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields #-}
{-# LANGUAGE DataKinds, OverloadedLists, TypeApplications, DerivingStrategies, DeriveGeneric, DeriveAnyClass, OverloadedLabels #-}

module Main where

import Vulkan
  ( pattern FILTER_LINEAR
  , SamplerAddressMode(..)
  )

import Platforms.GLFW.Vulkan
import Hickory.Vulkan.Vulkan
import qualified Hickory.Vulkan.DescriptorSet as H
import qualified Hickory.Vulkan.Types as H
import Linear.Matrix ((!*!))
import Linear ( M44, V2 (..), V3(..), V4(..), identity)
import Hickory.Math (mkTranslation)
import Hickory.Math.Matrix ( orthographicProjection, mkScale )
import Hickory.Camera (Camera(..), Projection(..))
import qualified Hickory.Vulkan.Forward.Types as H
import Hickory.Vulkan.Forward.Types (DrawCommand(..), RenderSettings(..), WorldSettings(..), OverlayGlobals(..))
import Hickory.Vulkan.Types (VulkanResources(..))
import qualified Hickory.Vulkan.Forward.Renderer as H
import qualified Hickory.Vulkan.StockMesh as H
import Hickory.Color (white)
import Control.Monad.IO.Class (liftIO)

import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Control.Lens (view)
import Acquire.Acquire (Acquire)

data Resources = Resources
  { square              :: H.BufferedMesh
  , starTex             :: H.PointedDescriptorSet
  , xTex                :: H.PointedDescriptorSet
  }

data Uniform = Uniform
  { modelView :: M44 Float
  } deriving Generic
    deriving anyclass GStorable

acquireResources :: VulkanResources -> Acquire Resources
acquireResources vulkanResources = do
  square  <- H.withSquareMesh vulkanResources
  starTex <- view #descriptorSet <$> H.withTextureDescriptorSet vulkanResources [("star.png", FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)]
  xTex    <- view #descriptorSet <$> H.withTextureDescriptorSet vulkanResources [("x.png", FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)]

  pure Resources {..}

main :: IO ()
main = withWindow 800 800 "Vulkan Test" \win -> runAcquire do
  vulkanResources <- initGLFWVulkan win
  Resources {..} <- acquireResources vulkanResources
  liftIO $ runFrames win vulkanResources (H.withRenderer vulkanResources) \renderer fc -> do
    let
      overlayF = do
        pure ()
      litF = do
        H.addCommand $ DrawCommand
          { modelMat = mkTranslation (V2 25 25) !*! mkScale (V2 20 20)
          , mesh = H.Buffered square
          , color = white
          , drawType = H.Static $ H.StaticMesh starTex (V2 1 1)
          , lit = False
          , castsShadow = False
          , blend = True
          , ident = Nothing
          , specularity = 1
          }

        H.addCommand $ DrawCommand
          { modelMat = mkTranslation (V2 75 25) !*! mkScale (V2 20 20)
          , mesh = H.Buffered square
          , color = white
          , drawType = H.Static $ H.StaticMesh xTex (V2 1 1)
          , lit = False
          , castsShadow = False
          , blend = True
          , ident = Nothing
          , specularity = 1
          }

    let settings = RenderSettings
          { worldSettings = H.worldSettingsDefaults { camera = Camera (V3 50 50 0) (V3 0 0 (1)) (V3 0 (-1) 0) (Ortho 100 0 100 True) }
          , overlayGlobals = OverlayGlobals identity identity
          , postSettings = H.postDefaults
          , clearColor = V4 0 0 0 1
          , highlightObjs = []
          }
    H.renderToRenderer fc renderer settings litF overlayF
