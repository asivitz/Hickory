module Hickory.Graphics.Framebuffer
  ( Framebuffer(..)
  , mkColorDepthFramebuffer
  , mkFramebufferWith
  , deleteFramebuffer
  ) where

import Graphics.GL.Compatibility41
import Hickory.Graphics.GLSupport (alloc1, withArrayLen, checkForErrors)
import Hickory.Graphics.Textures (TexID(..), mkTextureWith)
import Foreign.Ptr (nullPtr)
import Hickory.Types (Size(..))
import Text.Printf (printf)
import GHC.Stack (HasCallStack)

data Framebuffer = Framebuffer
  { framebuffer  :: !GLuint
  , colorTexture :: !TexID
  , depthTexture :: !TexID
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

  let warn s = putStrLn $ "Warning: Framebuffer not complete. " ++ s
  glCheckFramebufferStatus GL_FRAMEBUFFER >>= \case
    GL_FRAMEBUFFER_COMPLETE  -> pure ()
    GL_FRAMEBUFFER_UNDEFINED                     -> warn "Target is the default framebuffer, but the default framebuffer does not exist."
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT         -> warn "One of the framebuffer attachment points are framebuffer incomplete."
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> warn "Framebuffer does not have at least one image attached to it."
    GL_FRAMEBUFFER_UNSUPPORTED                   -> warn "Depth and stencil attachments, if present, are not the same renderbuffer, or if the combination of internal formats of the attached images violates an implementation-dependent set of restrictions."
    GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE        -> warn "The value of GL_RENDERBUFFER_SAMPLES is not the same for all attached renderbuffers or, if the attached images are a mix of renderbuffers and textures, the value of GL_RENDERBUFFER_SAMPLES is not zero."
    s -> warn $ printf "Unknown framebuffer status. %d" s

  glBindFramebuffer GL_FRAMEBUFFER 0

  checkForErrors

  pure $! a

mkFramebufferTexture :: Size GLsizei -> GLenum -> IO TexID
mkFramebufferTexture (Size w h) attachment = mkTextureWith \tex -> do
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGB8) w h 0 GL_RGB GL_UNSIGNED_BYTE nullPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)

  glFramebufferTexture2D GL_FRAMEBUFFER attachment GL_TEXTURE_2D tex 0

mkFramebufferDepthTexture :: Size GLsizei -> IO TexID
mkFramebufferDepthTexture (Size w h) = mkTextureWith \tex -> do
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_DEPTH_COMPONENT24) w h 0 GL_DEPTH_COMPONENT GL_UNSIGNED_INT nullPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)

  glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D tex 0

mkColorDepthFramebuffer :: Size GLsizei -> IO Framebuffer
mkColorDepthFramebuffer size@Size {..} = mkFramebufferWith \framebuffer -> do
  colorTexture <- mkFramebufferTexture size GL_COLOR_ATTACHMENT0
  depthTexture <- mkFramebufferDepthTexture size

  glViewport 0 0 width height
  pure Framebuffer {..}
