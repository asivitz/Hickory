{-# LANGUAGE DeriveGeneric #-}

module Hickory.ModelLoading.Packed where

import Graphics.GL.Compatibility41
import Data.Vector.Storable as V
import GHC.Generics (Generic)
import Data.Binary
import Hickory.Graphics (Shader, createIndexedVAO, VertexGroup (..), Attachment (..), DrawType (..))
import Hickory.Graphics.VAO (VAO)
import Data.Vector.Binary ()
import Control.Monad ((<=<))

-- |All data is per-vertex
data ModelData = ModelData
  { vertices         :: Vector GLfloat
  , normals          :: Vector GLfloat
  , uvs              :: Vector GLfloat
  , material_indices :: Vector GLuint
  , bone_indices     :: Vector GLuint
  , face_indices     :: Vector GLuint
  } deriving Generic

instance Binary ModelData

pack :: ModelData -> (Vector GLfloat, Vector GLuint)
pack ModelData {..} = case remainder of
  0 -> (V.concat $ f <$> [0..(numVerts-1)], face_indices)
  _ -> error "Vertices not divisible by 3. Cannot pack."
  where
  (numVerts, remainder) = V.length vertices `quotRem` 3
  f i = fromList
    [ vertices ! (i * 3)
    , vertices ! (i * 3 + 1)
    , vertices ! (i * 3 + 2)
    , uvs ! (i * 2)
    , uvs ! (i * 2 + 1)
    , normals ! (i * 3)
    , normals ! (i * 3 + 1)
    , normals ! (i * 3 + 2)
    , realToFrac $ material_indices ! i
    , realToFrac $ bone_indices ! i
    ]

buildModelVAO :: Shader -> ModelData -> IO VAO
buildModelVAO shader model =
  createIndexedVAO
    shader
    [VertexGroup [Attachment "position" 3, Attachment "texCoords" 2, Attachment "normal" 3, Attachment "materialIndex" 1, Attachment "boneIndex" 1]]
    (pack model)
    Triangles

loadVAOFromFile :: Shader -> FilePath -> IO VAO
loadVAOFromFile shader = buildModelVAO shader <=< decodeFile

writeToFile :: FilePath -> ModelData -> IO ()
writeToFile = encodeFile
