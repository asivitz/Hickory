module Hickory.Graphics.Framebuffer
  ( Framebuffer(..)
  , mkColorDepthFramebuffer
  , mkFramebufferWith
  , deleteFramebuffer
  ) where

import Graphics.GL.Compatibility41
import Hickory.Graphics.GLSupport (alloc1, withArrayLen)
import Hickory.Graphics.Textures (TexID(..), mkTextureWith)
import Foreign.Ptr (nullPtr)
import Hickory.Types (Size(..))
import Control.Monad (when)

data Framebuffer = Framebuffer
  { framebuffer  :: GLuint
  , colorTexture :: TexID
  , depthTexture :: TexID
  }

deleteFramebuffer :: Framebuffer -> IO ()
deleteFramebuffer Framebuffer {..} = do
  withArrayLen [framebuffer] glDeleteFramebuffers
  withArrayLen [getTexID colorTexture, getTexID depthTexture] glDeleteTextures


mkFramebufferWith :: (GLuint -> IO a) -> IO a
mkFramebufferWith f = do
  framebuffer <- alloc1 glGenFramebuffers
  glBindFramebuffer GL_FRAMEBUFFER framebuffer

  a <- f framebuffer

  glCheckFramebufferStatus GL_FRAMEBUFFER >>= \status -> when ( status /= GL_FRAMEBUFFER_COMPLETE) $
    print ("Warning: Framebuffer not complete!" :: String)

  glBindFramebuffer GL_FRAMEBUFFER 0

  pure a

mkFramebufferTexture :: Size GLsizei -> GLenum -> IO TexID
mkFramebufferTexture (Size w h) attachment = mkTextureWith \tex -> do
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGB) w h 0 GL_RGB GL_UNSIGNED_BYTE nullPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)

  glFramebufferTexture2D GL_FRAMEBUFFER attachment GL_TEXTURE_2D tex 0

mkFramebufferDepthTexture :: Size GLsizei -> IO TexID
mkFramebufferDepthTexture (Size w h) = mkTextureWith \tex -> do
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_DEPTH_COMPONENT24) w h 0 GL_DEPTH_COMPONENT GL_FLOAT nullPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)

  glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D tex 0

mkColorDepthFramebuffer :: Size GLsizei -> IO Framebuffer
mkColorDepthFramebuffer size@Size {..} = mkFramebufferWith \framebuffer -> do
  colorTexture <- mkFramebufferTexture size GL_COLOR_ATTACHMENT0
  depthTexture <- mkFramebufferDepthTexture size

  glViewport 0 0 width height
  pure Framebuffer {..}
