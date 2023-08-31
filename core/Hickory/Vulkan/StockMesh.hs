{-# LANGUAGE OverloadedLists #-}

module Hickory.Vulkan.StockMesh where

import Acquire.Acquire (Acquire)
import qualified Data.Vector.Storable as SV
import Hickory.Vulkan.Mesh (withBufferedMesh)
import Data.Word (Word32)
import Linear (V3 (..), V2 (..), normalize)
import Data.Foldable (toList)
import Hickory.Vulkan.Types (VulkanResources, BufferedMesh, Mesh (..), Attribute (..))

withSquareMesh :: VulkanResources -> Acquire BufferedMesh
withSquareMesh vulkanResources = withBufferedMesh vulkanResources $ Mesh
  { vertices =
    [ (Position, [ -0.5, -0.5, 0.0
                 ,  0.5, -0.5, 0.0
                 ,  0.5,  0.5, 0.0
                 , -0.5,  0.5, 0.0
                 ])
    , (TextureCoord, [ 0.0, 1.0
                     , 1.0, 1.0
                     , 1.0, 0.0
                     , 0.0, 0.0
                     ])
    , (Normal, [ 0.0, 0.0, 1.0
               , 0.0, 0.0, 1.0
               , 0.0, 0.0, 1.0
               , 0.0, 0.0, 1.0
               ])
    ]
  , indices = Just [0, 2, 1, 2, 0, 3]
  , morphTargets = []
  , minPosition = V3 (-0.5) (-0.5) 0
  , maxPosition = V3 0.5 0.5 0
  }

withCubeMesh :: VulkanResources -> Acquire BufferedMesh
withCubeMesh vulkanResources = withBufferedMesh vulkanResources $ Mesh
  { vertices =
    [ (Position, floats)
    , (TextureCoord, tcs)
    , (Normal, normals)
    ]
  , indices = Just indices
  , morphTargets = []
  , minPosition = V3 (-0.5) (-0.5) (-0.5)
  , maxPosition = V3 0.5 0.5 0.5
  }
  where
  --   7
  --  / \
  -- 4   6
  -- |\ /|
  -- 0 5 2
  --  \|/
  --   1
  floats :: SV.Vector Float
  floats = SV.fromList . concatMap toList $ verts
    where
    h     = 0.5
    l     = -h
    p0    = V3 l l l
    p1    = V3 h l l
    p2    = V3 h h l
    p3    = V3 l h l
    p4    = V3 l l h
    p5    = V3 h l h
    p6    = V3 h h h
    p7    = V3 l h h
    verts :: [V3 Float]
    verts = [p0, p1, p2, p3, p4, p5, p6, p7]
  normals :: SV.Vector Float
  normals = SV.fromList . concatMap toList $ verts
    where
    h     = 0.5
    l     = -h
    p0    = normalize $ V3 l l l
    p1    = normalize $ V3 h l l
    p2    = normalize $ V3 h h l
    p3    = normalize $ V3 l h l
    p4    = normalize $ V3 l l h
    p5    = normalize $ V3 h l h
    p6    = normalize $ V3 h h h
    p7    = normalize $ V3 l h h
    verts :: [V3 Float]
    verts = [p0, p1, p2, p3, p4, p5, p6, p7]
  indices :: SV.Vector Word32
  indices = SV.fromList
    [ 0, 1, 5
    , 0, 5, 4
    , 1, 2, 6
    , 1, 6, 5
    , 2, 3, 7
    , 2, 7, 6
    , 3, 0, 4
    , 3, 4, 7
    , 5, 6, 7
    , 5, 7, 4
    , 0, 3, 2
    , 0, 2, 1
    ]
  tcs :: SV.Vector Float
  tcs = SV.fromList . concatMap toList $ ([ll, lr, ur, ul, ll, lr, ur, ul] :: [V2 Float])
    where
    ll = V2 0 0
    ul = V2 0 1
    ur = V2 1 1
    lr = V2 1 0
