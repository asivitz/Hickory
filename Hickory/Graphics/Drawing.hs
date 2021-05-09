{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module Hickory.Graphics.Drawing where

import Control.Lens (ifor_)

import qualified Data.Vector.Storable as SV
import Graphics.GL.Compatibility41
import Hickory.Graphics.Shader (useShader)
import Hickory.Graphics.Textures (TexID(..))
import Hickory.Graphics.GLSupport (DrawType, UniformBinding, bindUniform, drawElements, glenumForDrawType)
import Hickory.Graphics.VAO (VAOConfig(..), VAOObj(..), withVAOConfig, loadVerticesIntoVAOConfig)
import Hickory.Graphics.Types (DrawSpec(..))

drawCommand :: [UniformBinding] -> [TexID] -> VAOConfig -> GLint -> DrawType -> IO ()
drawCommand uniformBindings texids vaoconfig@VAOConfig { shader } numitems drawType = do
  useShader shader
  ifor_ texids \i t -> do
    glActiveTexture $ GL_TEXTURE0 + fromIntegral i
    glBindTexture GL_TEXTURE_2D (getTexID t)

  mapM_ (bindUniform shader) uniformBindings

  withVAOConfig shader vaoconfig $
    drawElements (glenumForDrawType drawType) numitems GL_UNSIGNED_SHORT

drawSpec :: [UniformBinding] -> [TexID] -> DrawSpec -> IO ()
drawSpec uniforms texs spec = case spec of
  VAO (VAOObj _ 0 _) -> pure ()
  VAO (VAOObj vaoConfig numitems drawType) -> do
    drawCommand uniforms texs vaoConfig (fromIntegral numitems) drawType
  DynVAO vaoConfig (verts, indices, drawType) -> do
    loadVerticesIntoVAOConfig vaoConfig verts indices
    drawCommand uniforms texs vaoConfig (fromIntegral $ SV.length indices) drawType

