{-# LANGUAGE FlexibleContexts #-}

module Hickory.Graphics.VAO where

import Hickory.Graphics.Drawing (VAO, VBO, DrawType, VertexGroup, Shader, UniformBinding, TexID, getTexID)
import Control.Monad.State.Strict (execStateT, modify, MonadIO, gets, liftIO, MonadState)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Graphics.GL.Compatibility41
import Hickory.Graphics.GLSupport (makeVAO, makeVBO, buildVertexGroup, bindVAO, bufferVertices, bufferIndices, drawElements, glenumForDrawType, bindUniform)
import Hickory.Graphics.Shader (useShader)

data VAOConfig = VAOConfig {
  vao :: !VAO,
  indexVBO :: !VBO,
  vertices :: ![VBO],
  shader :: Shader
  } deriving (Show)

data VAOObj = VAOObj
  { vaoConfig :: VAOConfig
  , count :: Int
  , drawType :: DrawType
  } deriving (Show)

type VAOCache = Map.Map Text VAOObj

loadVao :: (MonadState VAOCache m, MonadIO m) => Text -> IO VAOConfig -> (VAOConfig -> IO VAOObj) -> m ()
loadVao k create load = do
  vaoConfig <- gets (Map.lookup k) >>= \case
    Nothing -> liftIO create
    Just (VAOObj vc _ _) -> pure vc
  newVAO <- liftIO $ load vaoConfig
  modify $ Map.insert k newVAO

createVAOConfig :: Shader -> [VertexGroup] -> IO VAOConfig
createVAOConfig sh vertexgroups = do
  vao' <- makeVAO
  glBindVertexArray vao'

  index_vbo <- makeVBO
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER index_vbo

  buffers <- mapM (buildVertexGroup sh) vertexgroups

  return VAOConfig
    { vao = vao'
    , indexVBO = index_vbo
    , vertices = buffers
    , shader = sh
    }

withVAOConfig :: Shader -> VAOConfig -> IO () -> IO ()
withVAOConfig _ VAOConfig { vao } action = glBindVertexArray vao >> action

loadVerticesIntoVAOConfig :: VAOConfig -> [GLfloat] -> [GLushort] -> IO ()
loadVerticesIntoVAOConfig VAOConfig { vao, indexVBO = ivbo, vertices = (vbo:_) } vs indices = do
  bindVAO vao
  bufferVertices vbo vs
  bufferIndices ivbo indices
loadVerticesIntoVAOConfig _ _ _ = error "VAOConfig missing buffers"

drawCommand :: [UniformBinding] -> Maybe TexID -> VAOConfig -> GLint -> DrawType -> IO ()
drawCommand uniformBindings texid vaoconfig@VAOConfig { shader} numitems drawType = do
  useShader shader
  case texid of
      Just t -> glBindTexture GL_TEXTURE_2D (getTexID t)
      _ -> return ()

  mapM_ (bindUniform shader) uniformBindings

  withVAOConfig shader vaoconfig $
      drawElements (glenumForDrawType drawType) numitems GL_UNSIGNED_SHORT
