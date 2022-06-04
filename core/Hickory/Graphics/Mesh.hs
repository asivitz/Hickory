{-# LANGUAGE OverloadedStrings #-}

module Hickory.Graphics.Mesh where

import Data.Foldable (toList)
import Graphics.GL.Compatibility41 as GL
import Hickory.Graphics.GLSupport
import Hickory.Graphics.VAO (createIndexedVAO, VAO(..), createDirectVAO, VertexGroup (..), Attachment (..))
import Hickory.Math.Vector
import Linear (V3(..))
import qualified Data.Vector.Storable as SV
import Hickory.Graphics.Shader (Shader)

-- Stock cube mesh

cubeFloats :: SV.Vector GLfloat
cubeFloats = SV.fromList . concatMap toList $ verts
 where
  h     = 0.5
  l     = -h
  p1    = V3 l l l
  p2    = V3 h l l
  p3    = V3 h h l
  p4    = V3 l h l
  p5    = V3 l l h
  p6    = V3 h l h
  p7    = V3 h h h
  p8    = V3 l h h
  verts = [p1, p2, p3, p4, p5, p6, p7, p8]

cubeIndices :: SV.Vector GLuint
cubeIndices = SV.fromList [6, 7, 5, 4, 0, 7, 3, 6, 2, 5, 1, 0, 2, 3]

mkCubeVAOObj :: Shader -> IO VAO
mkCubeVAOObj shader = createIndexedVAO shader [VertexGroup [Attachment "position" 3]] (cubeFloats, cubeIndices) TriangleStrip

mkInvertedCubeVAOObj :: Shader -> IO VAO
mkInvertedCubeVAOObj shader = createIndexedVAO shader [VertexGroup [Attachment "position" 3]] (cubeFloats, SV.reverse cubeIndices) TriangleStrip

-- Stock square mesh

mkSquareVerts :: (SV.Storable t, SV.Storable t1, Num t, Fractional t1) => t1 -> t1 -> (SV.Vector t1, SV.Vector t, DrawType)
mkSquareVerts texW texH = (SV.fromList floats, SV.fromList indices, TriangleFan)
  where h = 0.5
        l = -h
        floats = [l, h, 0, texH,
                  l, l, 0, 0,
                  h, l, texW, 0,
                  h, h, texW, texH]
        indices = [0,1,2,3]

mkSquareVAOObj :: Scalar -> Shader -> IO VAO
mkSquareVAOObj sideLength shader =
  createIndexedVAO
    shader
    [VertexGroup [Attachment "position" 2, Attachment "normal" 3, Attachment "texCoords" 2]]
    (floats, indices)
    TriangleFan
  where h = realToFrac sideLength
        l = -h
        floats = SV.fromList [l, h, 0, 0, 1, 0, 1,
                            l, l, 0, 0, 1, 0, 0,
                            h, l, 0, 0, 1, 1, 0,
                            h, h, 0, 0, 1, 1, 1]
        indices = SV.fromList [0,1,2,3]

mkUntexturedSquareVAO :: Shader -> IO VAO
mkUntexturedSquareVAO shader =
  createIndexedVAO
    shader
    [VertexGroup [Attachment "position" 2]]
    (SV.fromList floats, SV.fromList indices)
    TriangleFan

  where
  h = 0.5
  l = -h
  floats = [l, h,
            l, l,
            h, l,
            h, h]
  indices = [0,1,2,3]

mkPolygonVAO :: Shader -> DrawType -> [V3 GLfloat] -> IO VAO
mkPolygonVAO shader dt points = do
  let v = SV.concat (map (\(V3 x y z) -> SV.fromList [x,y,z]) points)
  createDirectVAO shader
    [VertexGroup [Attachment "position" 3]]
    (v, length points)
    dt
