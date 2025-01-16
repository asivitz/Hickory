{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields, OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE DataKinds #-}

module Hickory.Vulkan.Instance where

import Vulkan
  ( ApplicationInfo(..)
  , Instance
  , InstanceCreateInfo (..)
  , withInstance
  , pattern API_VERSION_1_2, InstanceCreateFlagBits (..), enumerateInstanceExtensionProperties, layerName, extensionName, enumerateInstanceLayerProperties, ValidationFeaturesEXT (..), ValidationFeatureEnableEXT (..)
  )
import Vulkan.Zero
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Hickory.Vulkan.Vulkan (mkAcquire)
import Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as DL
import Data.Foldable (for_)

validationLayers :: [B.ByteString]
-- validationLayers = ["VK_LAYER_LUNARG_api_dump", "VK_LAYER_KHRONOS_validation"]
validationLayers = ["VK_LAYER_KHRONOS_validation"]

withStandardInstance :: [B.ByteString] -> [B.ByteString] -> Acquire Instance
withStandardInstance (DL.nub -> desiredExtensions) (DL.nub -> desiredLayers) = do
  (_, V.toList . fmap extensionName -> availableExtensions) <- liftIO (enumerateInstanceExtensionProperties Nothing)
  (_, V.toList . fmap layerName     -> availableLayers)     <- liftIO enumerateInstanceLayerProperties
  let layersToEnable = DL.intersect desiredLayers availableLayers
      layersNotAvailable = desiredLayers DL.\\ layersToEnable
      extensionsToEnable = DL.intersect desiredExtensions availableExtensions
      extensionsNotAvailable = desiredExtensions DL.\\ extensionsToEnable

  for_ layersNotAvailable \l ->
    liftIO . putStrLn $ "Requested layer not available: " ++ show l

  for_ extensionsNotAvailable \e ->
    liftIO . putStrLn $ "Requested extension not available: " ++ show e

  let
    instanceCreateInfo :: InstanceCreateInfo '[ValidationFeaturesEXT]
    instanceCreateInfo = zero
      { applicationInfo = Just zero { applicationName = Just "Vulkan Demo", apiVersion = API_VERSION_1_2 }
      , enabledLayerNames     = V.fromList layersToEnable
      , enabledExtensionNames = V.fromList extensionsToEnable
      , flags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
      , next = (zero
        { enabledValidationFeatures =
          [ -- VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT
          ]
        },())
      }

  withInstance instanceCreateInfo Nothing mkAcquire
