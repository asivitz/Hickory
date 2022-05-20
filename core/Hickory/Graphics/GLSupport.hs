{-# LANGUAGE BlockArguments #-}

{-
module Graphics.GLSupport ( module Graphics.GL.ARB.ES2Compatibility ) where
import Graphics.GL.ARB.ES2Compatibility
-}

module Hickory.Graphics.GLSupport
  ( Shader (..),
    bufferVertices,
    bufferIndices,
    configGLState,
    clearScreen,
    clearDepth,
    buildVertexGroup,
    drawElements,
    glenumForDrawType,
    VAOId,
    VBOId,
    alloc1,
    DrawType(..),
    VertexGroup(..),
    Attachment(..),
    withArrayLen
  )
where

import Control.Monad (when)
import Data.Bits
import qualified Data.Foldable as Fold
import Data.Int (Int32)
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import Graphics.GL.Compatibility41 as GL
import Hickory.Graphics.Shader
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Marshal as FM

-- Types

type VAOId = Word32

type VBOId = Word32

data DrawType
  = TriangleFan
  | TriangleStrip
  | Triangles
  deriving (Show, Eq)

data Attachment = Attachment String Int32

newtype VertexGroup = VertexGroup [Attachment]

instance Show VertexGroup where show _ = "Vertex Group"

-- Utilities

alloc1 :: (Integral i, Storable b) => (i -> Ptr b -> IO a) -> IO b
alloc1 f = alloca \p -> f 1 p >> peek p

withArrayLen :: (Storable a, Integral i) => [a] -> (i -> Ptr a -> IO b) -> IO b
withArrayLen l f = FM.withArrayLen l $ f . fromIntegral

bufferVertices :: VBOId -> V.Vector GLfloat -> IO ()
bufferVertices vbo floats = do
  glBindBuffer GL_ARRAY_BUFFER vbo
  bufferData GL_ARRAY_BUFFER floats GL_STREAM_DRAW

bufferIndices :: VBOId -> V.Vector GLushort -> IO ()
bufferIndices vbo ints = do
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER vbo
  bufferData GL_ELEMENT_ARRAY_BUFFER ints GL_STREAM_DRAW

-- VAO / VBO

glenumForDrawType :: DrawType -> GLenum
glenumForDrawType dt = case dt of
  TriangleStrip -> GL_TRIANGLE_STRIP
  TriangleFan -> GL_TRIANGLE_FAN
  Triangles -> GL_TRIANGLES

attachVertexArray :: GLint -> GLint -> GLint -> GLint -> IO ()
attachVertexArray attrLoc len stride offset =
  if attrLoc < 0
    then print ("Can't attach vertex array: Bad attribute location" :: String)
    else do
      enableVertexAttribArray attrLoc
      vertexAttribPointer attrLoc len GL_FLOAT GL_FALSE (stride * fsize) (fromIntegral $ offset * fsize)
  where
    fsize :: GLint
    fsize = fromIntegral $ sizeOf (0 :: GLfloat)

buildVertexGroup :: Shader -> VertexGroup -> IO VBOId
buildVertexGroup shader group = do
  vbo <- alloc1 glGenBuffers
  attachVertexGroup shader vbo group
  pure vbo

attachVertexGroup :: Shader -> VBOId -> VertexGroup -> IO ()
attachVertexGroup shader vbo (VertexGroup attachments) = do
  glBindBuffer GL_ARRAY_BUFFER vbo

  let stride = sum $ map (\(Attachment _a l) -> l) attachments

  _ <-
    Fold.foldlM
      ( \offset (Attachment a l) -> do
          loc <- getAttribLocation (program shader) a
          when (loc >= 0) do
            attachVertexArray loc l stride offset
          return (offset + l)
      )
      0
      attachments
  pure ()

enableVertexAttribArray :: GLint -> IO ()
enableVertexAttribArray = glEnableVertexAttribArray . fromIntegral

disableVertexAttribArray :: GLint -> IO ()
disableVertexAttribArray = glDisableVertexAttribArray . fromIntegral

drawElements :: GLenum -> GLsizei -> GLenum -> IO ()
drawElements a b c = glDrawElements a b c nullPtr

vertexAttribPointer :: GLint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLint -> IO ()
vertexAttribPointer a b c d e f = glVertexAttribPointer (fromIntegral a) b c d e (plusPtr nullPtr (fromIntegral f))

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

  glEnable GL_PROGRAM_POINT_SIZE -- for OSX
  glEnable GL_DEPTH_TEST

  glEnable GL_BLEND

clearScreen :: IO ()
clearScreen = do
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

clearDepth :: IO ()
clearDepth = do
  glClear GL_DEPTH_BUFFER_BIT
