{-# LANGUAGE BlockArguments, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DuplicateRecordFields, OverloadedLists, CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE DataKinds #-}

module Hickory.Vulkan.Instance where

import Vulkan
  ( ApplicationInfo(..)
  , Instance
  , InstanceCreateInfo (..)
  , withInstance
  , pattern API_VERSION_1_3, InstanceCreateFlagBits (..), enumerateInstanceExtensionProperties, layerName, extensionName, enumerateInstanceLayerProperties, ValidationFeaturesEXT (..), ValidationFeatureEnableEXT (..)
  )
import Vulkan.Zero
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Hickory.Vulkan.Vulkan (mkAcquire)
import Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as DL
import Data.Foldable (for_)
import GHC.Bits (zeroBits)
import System.Info (os)

validationLayers :: [B.ByteString]
validationLayers = []

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
    instanceCreateInfo :: InstanceCreateInfo '[]
    instanceCreateInfo = zero
      { applicationInfo = Just zero { applicationName = Just "Vulkan Demo", apiVersion = API_VERSION_1_3 }
      , enabledLayerNames     = V.fromList layersToEnable
      , enabledExtensionNames = V.fromList extensionsToEnable
      , flags = if os == "darwin" then INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR else zeroBits
      }

  withInstance instanceCreateInfo Nothing mkAcquire
