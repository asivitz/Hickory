{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module Hickory.Graphics.Drawing where

import Control.Lens (ifor_)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Graphics.GL.Compatibility41
import Hickory.Graphics.Shader (useShader)
import Hickory.Graphics.Textures (TexID(..))
import Hickory.Graphics.GLSupport (DrawType, drawElements, glenumForDrawType)
import Hickory.Graphics.VAO (VAOConfig(..), VAO(..))
import Hickory.Graphics.ShaderMonad (runShaderT, ShaderT)

bindTextures :: MonadIO m => [TexID] -> m ()
bindTextures texids =
  ifor_ texids \i t -> do
    glActiveTexture $ GL_TEXTURE0 + fromIntegral i
    glBindTexture GL_TEXTURE_2D (getTexID t)

drawCommand :: VAOConfig -> GLint -> DrawType -> IO ()
drawCommand VAOConfig { vaoId, indexVBO } numitems drawType = do
  glBindVertexArray vaoId

  case indexVBO of
    Just _ -> drawElements (glenumForDrawType drawType) numitems GL_UNSIGNED_INT
    Nothing -> glDrawArrays (glenumForDrawType drawType) 0 numitems

drawVAO :: MonadIO m =>  VAO -> ShaderT m () -> m ()
drawVAO (VAO _ 0 _) _f = pure ()
drawVAO (VAO vaoConfig@VAOConfig { shader } numitems drawType) f = do
  liftIO $ useShader shader
  runShaderT shader f
  liftIO $ drawCommand vaoConfig (fromIntegral numitems) drawType
