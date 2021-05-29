{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Hickory.Graphics.VAO where

import Control.Monad.State.Strict (MonadIO, MonadState, gets, liftIO, modify)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Vector.Storable as V
import Graphics.GL.Compatibility41
import Hickory.Graphics.GLSupport (VAO, VBO, alloc1, bufferIndices, bufferVertices, buildVertexGroup, DrawType, VertexGroup)
import Hickory.Graphics.Shader (Shader)

data VAOConfig = VAOConfig {
  vao :: !VAO,
  indexVBO :: Maybe VBO,
  vertices :: ![VBO],
  shader :: Shader
  }
  deriving (Show)

data VAOObj = VAOObj
  { vaoConfig :: VAOConfig
  , count :: Int
  , drawType :: DrawType
  } deriving (Show)

type VAOCache = Map.HashMap Text VAOObj

loadVao :: (MonadState VAOCache m, MonadIO m) => Text -> IO VAOConfig -> (VAOConfig -> IO VAOObj) -> m VAOObj
loadVao k create load = do
  vaoConfig <- gets (Map.lookup k) >>= \case
    Nothing -> liftIO create
    Just (VAOObj vc _ _) -> pure vc
  newVAO <- liftIO $ load vaoConfig
  modify $ Map.insert k newVAO
  pure newVAO

createIndexedVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createIndexedVAOConfig sh vertexgroups = do
  vao' <- alloc1 glGenVertexArrays
  glBindVertexArray vao'

  index_vbo <- alloc1 glGenBuffers
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER index_vbo

  buffers <- mapM (buildVertexGroup sh) vertexgroups

  return VAOConfig
    { vao = vao'
    , indexVBO = Just index_vbo
    , vertices = buffers
    , shader = sh
    }

createDirectVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createDirectVAOConfig sh vertexgroups = do
  vao' <- alloc1 glGenVertexArrays
  glBindVertexArray vao'

  buffers <- mapM (buildVertexGroup sh) vertexgroups

  return VAOConfig
    { vao = vao'
    , indexVBO = Nothing
    , vertices = buffers
    , shader = sh
    }

deleteVAOConfigs :: [VAOConfig] -> IO ()
deleteVAOConfigs vcs = do
  let vaos = map (\VAOConfig {..} -> vao) vcs
      vaosv = V.fromList vaos
      vbos = concatMap (\VAOConfig {..} -> maybe id (:) indexVBO vertices) vcs
      vbosv = V.fromList vbos
  V.unsafeWith vaosv $ glDeleteVertexArrays (fromIntegral $ V.length vaosv)
  V.unsafeWith vbosv $ glDeleteBuffers (fromIntegral $ V.length vbosv)

loadVerticesIntoIndexedVAOConfig :: VAOConfig -> V.Vector GLfloat -> V.Vector GLushort -> IO ()
loadVerticesIntoIndexedVAOConfig VAOConfig { vao, indexVBO = Just ivbo, vertices = (vbo:_) } vs indices = do
  glBindVertexArray vao
  bufferVertices vbo vs
  bufferIndices ivbo indices
loadVerticesIntoIndexedVAOConfig _ _ _ = error "VAOConfig missing buffers"

loadVerticesIntoDirectVAOConfig :: VAOConfig -> V.Vector GLfloat -> IO ()
loadVerticesIntoDirectVAOConfig VAOConfig { vao, indexVBO = Nothing, vertices = (vbo:_) } vs = do
  glBindVertexArray vao
  bufferVertices vbo vs
loadVerticesIntoDirectVAOConfig _ _ = error "Can't load into direct vao config"
