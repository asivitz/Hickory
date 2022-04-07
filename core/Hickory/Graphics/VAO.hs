{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Hickory.Graphics.VAO (VAOCache, deleteVAOConfigs, createIndexedVAO, createDirectVAO, VAO(..), VAOConfig(..)) where

import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Vector.Storable as V
import Graphics.GL.Compatibility41
import Hickory.Graphics.GLSupport (VAOId, VBOId, alloc1, bufferIndices, bufferVertices, buildVertexGroup, DrawType, VertexGroup)
import Hickory.Graphics.Shader (Shader)

data VAOConfig = VAOConfig
  { vaoId :: !VAOId
  , indexVBO :: Maybe VBOId
  , vertices :: ![VBOId]
  , shader :: Shader
  }
  deriving (Show)

data VAO = VAO
  { vaoConfig :: VAOConfig
  , count :: Int
  , drawType :: DrawType
  } deriving (Show)

type VAOCache = Map.HashMap Text VAO

createIndexedVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createIndexedVAOConfig sh vertexgroups = do
  vao' <- alloc1 glGenVertexArrays
  glBindVertexArray vao'

  index_vbo <- alloc1 glGenBuffers
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER index_vbo

  buffers <- mapM (buildVertexGroup sh) vertexgroups

  pure VAOConfig
    { vaoId = vao'
    , indexVBO = Just index_vbo
    , vertices = buffers
    , shader = sh
    }

createDirectVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createDirectVAOConfig sh vertexgroups = do
  vao' <- alloc1 glGenVertexArrays
  glBindVertexArray vao'

  buffers <- mapM (buildVertexGroup sh) vertexgroups

  pure VAOConfig
    { vaoId = vao'
    , indexVBO = Nothing
    , vertices = buffers
    , shader = sh
    }

deleteVAOConfigs :: [VAOConfig] -> IO ()
deleteVAOConfigs vcs = do
  let vaos = map (\VAOConfig {..} -> vaoId) vcs
      vaosv = V.fromList vaos
      vbos = concatMap (\VAOConfig {..} -> maybe id (:) indexVBO vertices) vcs
      vbosv = V.fromList vbos
  V.unsafeWith vaosv $ glDeleteVertexArrays (fromIntegral $ V.length vaosv)
  V.unsafeWith vbosv $ glDeleteBuffers (fromIntegral $ V.length vbosv)

loadVerticesIntoIndexedVAOConfig :: VAOConfig -> V.Vector GLfloat -> V.Vector GLushort -> IO ()
loadVerticesIntoIndexedVAOConfig VAOConfig { vaoId, indexVBO = Just ivbo, vertices = (vbo:_) } vs indices = do
  glBindVertexArray vaoId
  bufferVertices vbo vs
  bufferIndices ivbo indices
loadVerticesIntoIndexedVAOConfig _ _ _ = error "VAOConfig missing buffers"

loadVerticesIntoDirectVAOConfig :: VAOConfig -> V.Vector GLfloat -> IO ()
loadVerticesIntoDirectVAOConfig VAOConfig { vaoId, indexVBO = Nothing, vertices = (vbo:_) } vs = do
  glBindVertexArray vaoId
  bufferVertices vbo vs
loadVerticesIntoDirectVAOConfig _ _ = error "Can't load into direct vao config"

createIndexedVAO :: Shader -> [VertexGroup] -> (V.Vector GLfloat, V.Vector GLushort) -> DrawType -> IO VAO
createIndexedVAO shader vgroups (verts,indices) drawType = do
  vc <- createIndexedVAOConfig shader vgroups
  loadVerticesIntoIndexedVAOConfig vc verts indices
  pure $ VAO vc (V.length indices) drawType

createDirectVAO :: Shader -> [VertexGroup] -> (V.Vector GLfloat, Int) -> DrawType -> IO VAO
createDirectVAO shader vgroups (verts, numVerts) drawType = do
  vc <- createDirectVAOConfig shader vgroups
  loadVerticesIntoDirectVAOConfig vc verts
  pure $ VAO vc numVerts drawType
