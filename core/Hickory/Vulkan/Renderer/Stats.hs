{-# LANGUAGE PatternSynonyms #-}

module Hickory.Vulkan.Renderer.Stats where

data Stats = Stats
  { numLitDraws :: Int
  , numGBuffer :: Int
  , numGBufferInstances :: Int
  , numGBufferPostCullInstances :: Int
  , numCastingShadows :: Int
  , numInstancesPerCascade :: [Int]
  , numDirect :: Int
  , numOverlayDraws :: Int
  , logMessages :: String
  }
