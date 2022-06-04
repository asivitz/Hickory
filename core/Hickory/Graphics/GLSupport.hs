{-# LANGUAGE BlockArguments #-}

{-
module Graphics.GLSupport ( module Graphics.GL.ARB.ES2Compatibility ) where
import Graphics.GL.ARB.ES2Compatibility
-}

module Hickory.Graphics.GLSupport
  ( bufferVertices,
    bufferIndices,
    configGLState,
    clearScreen,
    clearDepth,
    VAOId,
    VBOId,
    alloc1,
    DrawType(..),
    withArrayLen,
    checkForErrors
  )
where

import Data.Bits
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peek, sizeOf)
import Graphics.GL.Compatibility32 as GL
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Marshal as FM
import GHC.Stack (HasCallStack)
import Text.Printf (printf)

-- Types

type VAOId = Word32

type VBOId = Word32

data DrawType
  = TriangleFan
  | TriangleStrip
  | Triangles
  deriving (Show, Eq)

-- Utilities

alloc1 :: (Integral i, Storable b) => (i -> Ptr b -> IO a) -> IO b
alloc1 f = alloca \p -> f 1 p >> peek p

withArrayLen :: (Storable a, Integral i) => [a] -> (i -> Ptr a -> IO b) -> IO b
withArrayLen l f = FM.withArrayLen l $ f . fromIntegral

bufferVertices :: VBOId -> V.Vector GLfloat -> IO ()
bufferVertices vbo floats = do
  glBindBuffer GL_ARRAY_BUFFER vbo
  bufferData GL_ARRAY_BUFFER floats GL_STREAM_DRAW

bufferIndices :: VBOId -> V.Vector GLuint -> IO ()
bufferIndices vbo ints = do
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER vbo
  bufferData GL_ELEMENT_ARRAY_BUFFER ints GL_STREAM_DRAW


bufferData :: Storable a => GLenum -> V.Vector a -> GLenum -> IO ()
bufferData bufType vec usageType =
  V.unsafeWith vec \ptr ->
    glBufferData
      bufType
      (fromIntegral (V.length vec * sizeOf (V.head vec)))
      ptr
      usageType

configGLState :: GLfloat -> GLfloat -> GLfloat -> IO ()
configGLState r g b = do
  glClearColor r g b 1
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  glActiveTexture GL_TEXTURE0

  glDisable GL_DITHER
  glDisable GL_STENCIL_TEST

  -- glEnable GL_PROGRAM_POINT_SIZE -- for OSX
  glEnable GL_DEPTH_TEST

  glEnable GL_BLEND

  checkForErrors

clearScreen :: IO ()
clearScreen = do
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

clearDepth :: IO ()
clearDepth = do
  glClear GL_DEPTH_BUFFER_BIT

checkForErrors :: HasCallStack => IO ()
checkForErrors = glGetError >>= \case
  GL_INVALID_ENUM                      -> err "GL_INVALID_ENUM"
  GL_INVALID_FRAMEBUFFER_OPERATION     -> err "GL_INVALID_FRAMEBUFFER_OPERATION"
  -- GL_INVALID_FRAMEBUFFER_OPERATION_EXT -> err "GL_INVALID_FRAMEBUFFER_OPERATION_EXT"
  -- GL_INVALID_FRAMEBUFFER_OPERATION_OES -> err "GL_INVALID_FRAMEBUFFER_OPERATION_OES"
  GL_INVALID_OPERATION                 -> err "GL_INVALID_OPERATION"
  GL_INVALID_VALUE                     -> err "GL_INVALID_VALUE"
  GL_NO_ERROR                          -> pure ()
  GL_OUT_OF_MEMORY                     -> err "GL_OUT_OF_MEMORY"
  GL_STACK_OVERFLOW                    -> err "GL_STACK_OVERFLOW"
  GL_STACK_UNDERFLOW                   -> err "GL_STACK_UNDERFLOW"
  -- GL_TABLE_TOO_LARGE                   -> err "GL_TABLE_TOO_LARGE"
  -- GL_TABLE_TOO_LARGE_EXT               -> err "GL_TABLE_TOO_LARGE_EXT"
  -- GL_TEXTURE_TOO_LARGE_EXT             -> err "GL_TEXTURE_TOO_LARGE_EXT"
  other -> err $ printf "Unknown error %d" other
  where
  err s = error $ "GL Error: " ++ s
