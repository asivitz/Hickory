{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module Hickory.Graphics.Drawing where

import Control.Lens (ifor_)

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Vector.Storable as SV
import Graphics.GL.Compatibility41
import Hickory.Graphics.Shader (useShader)
import Hickory.Graphics.Textures (TexID(..))
import Hickory.Graphics.GLSupport (DrawType, drawElements, glenumForDrawType)
import Hickory.Graphics.VAO (VAOConfig(..), VAOObj(..), loadVerticesIntoIndexedVAOConfig)
import Hickory.Graphics.Types (DrawSpec(..))
import Hickory.Graphics.ShaderMonad (runShaderT, ShaderT)

bindTextures :: MonadIO m => [TexID] -> m ()
bindTextures texids =
  ifor_ texids \i t -> do
    glActiveTexture $ GL_TEXTURE0 + fromIntegral i
    glBindTexture GL_TEXTURE_2D (getTexID t)

drawCommand :: VAOConfig -> GLint -> DrawType -> IO ()
drawCommand VAOConfig { vao, indexVBO } numitems drawType = do
  glBindVertexArray vao

  case indexVBO of
    Just _ -> drawElements (glenumForDrawType drawType) numitems GL_UNSIGNED_SHORT
    Nothing -> glDrawArrays (glenumForDrawType drawType) 0 numitems

{-
drawSpec :: [TexID] -> DrawSpec -> IO ()
drawSpec texs spec = case spec of
  VAO (VAOObj _ 0 _) -> pure ()
  VAO (VAOObj vaoConfig numitems drawType) -> do
    drawCommand texs vaoConfig (fromIntegral numitems) drawType
  DynVAO vaoConfig (verts, indices, drawType) -> do
    loadVerticesIntoIndexedVAOConfig vaoConfig verts indices
    drawCommand texs vaoConfig (fromIntegral $ SV.length indices) drawType
    -}

drawVAO :: MonadIO m =>  VAOObj -> ShaderT m () -> m ()
drawVAO (VAOObj _ 0 _) _f = pure ()
drawVAO (VAOObj vaoConfig@VAOConfig { shader } numitems drawType) f = do
  liftIO $ useShader shader
  runShaderT shader f
  liftIO $ drawCommand vaoConfig (fromIntegral numitems) drawType
