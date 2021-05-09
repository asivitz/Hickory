{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-
module Graphics.GLSupport ( module Graphics.GL.ARB.ES2Compatibility ) where
import Graphics.GL.ARB.ES2Compatibility
-}

module Hickory.Graphics.GLSupport --( module Graphics.GL.Compatibility41)
    (
     bindVAO,
     Shader(..),
     bufferVertices,
     bufferIndices,
     configGLState,
     clearScreen,
     clearDepth,
     makeVAO,
     makeVBO,
     buildVertexGroup,
     drawElements,
     glenumForDrawType,
     bindUniform
     )
    where

import Graphics.GL.Compatibility41 as GL
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import Hickory.Math.Matrix
import Data.Bits
import Data.List (genericLength)
import Hickory.Graphics.Drawing
import Hickory.Graphics.Shader
import qualified Data.Foldable as Fold
import Linear (V4(..), V3(..), M44, M33, transpose)
import qualified Data.Vector.Storable as V
import Control.Monad (when)

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

data BufDataType = BufFloat | BufUShort

bufferVertices :: VBO -> V.Vector GLfloat -> IO ()
bufferVertices vbo floats = do
  glBindBuffer GL_ARRAY_BUFFER vbo
  bufferData GL_ARRAY_BUFFER floats GL_STREAM_DRAW BufFloat

bufferIndices :: VBO -> V.Vector GLushort -> IO ()
bufferIndices vbo ints = do
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER vbo
  bufferData GL_ELEMENT_ARRAY_BUFFER ints GL_STREAM_DRAW BufUShort

-- VAO / VBO

glenumForDrawType dt = case dt of
  TriangleStrip -> GL_TRIANGLE_STRIP
  TriangleFan -> GL_TRIANGLE_FAN
  Triangles -> GL_TRIANGLES

bindUniform :: Shader -> UniformBinding -> IO ()
bindUniform shader (UniformBinding name uniVal) =
  case retrieveLoc name shader of
      Just loc -> case uniVal of
                      Matrix4Uniform mat -> uniformMatrix4fv loc mat
                      Matrix3Uniform mat -> uniformMatrix3fv loc mat
                      QuadFUniform vec   -> uniform4fv loc vec
                      TripleFUniform vec -> uniform3fv loc vec
                      SingleFUniform f   -> glUniform1f loc (realToFrac f)
                      SingleIUniform i   -> glUniform1i loc (fromIntegral i)
      Nothing -> pure () -- print $ "Uniform named " ++ name ++ " not found in shader"

attachVertexArray :: GLint -> GLint -> GLint -> GLint -> IO ()
attachVertexArray attrLoc len stride offset =
  if attrLoc < 0
  then print ("Can't attach vertex array: Bad attribute location" :: String)
  else do
      enableVertexAttribArray attrLoc
      vertexAttribPointer attrLoc len GL_FLOAT GL_FALSE (stride * fsize) (fromIntegral $ offset * fsize)
    where fsize :: GLint
          fsize = fromIntegral $ sizeOf (0 :: GLfloat)

buildVertexGroup :: Shader -> VertexGroup -> IO VBO
buildVertexGroup shader group = do
  vbo <- makeVBO
  attachVertexGroup shader vbo group
  pure vbo

attachVertexGroup :: Shader -> VBO -> VertexGroup -> IO ()
attachVertexGroup shader vbo (VertexGroup attachments) = do
  glBindBuffer GL_ARRAY_BUFFER vbo

  let stride = sum $ map (\(Attachment a l) -> l) attachments

  _ <- Fold.foldlM (\offset (Attachment a l) -> do
          loc <- getAttribLocation (program shader) a
          when (loc < 0) do
            print $ "Cannot find attribute " ++ a ++ " in shader"
          attachVertexArray loc l stride offset
          return (offset + l))
      0
      attachments
  pure ()

enableVertexAttribArray = glEnableVertexAttribArray . fromIntegral

disableVertexAttribArray :: GLint -> IO ()
disableVertexAttribArray = glDisableVertexAttribArray . fromIntegral

drawElements :: GLenum -> GLsizei -> GLenum -> IO ()
drawElements a b c = glDrawElements a b c nullPtr

uniform4fv :: GLint -> [V4 Double] -> IO ()
uniform4fv loc vs =
        withVec4s vs $ \ptr ->
            glUniform4fv loc (genericLength vs) (castPtr ptr)

uniform3fv :: GLint -> [V3 Double] -> IO ()
uniform3fv loc vs =
        withVec3s vs $ \ptr ->
            glUniform3fv loc (genericLength vs) (castPtr ptr)

withVec4s :: [V4 Double] -> (Ptr GLfloat -> IO b) -> IO b
withVec4s vecs f = withArray (map (fmap realToFrac) vecs :: [V4 GLfloat]) (f . castPtr)

withVec3s :: [V3 Double] -> (Ptr GLfloat -> IO b) -> IO b
withVec3s vecs f = withArray (map (fmap realToFrac) vecs :: [V3 GLfloat]) (f . castPtr)

{-withVec4s vecs f = withVec4 (vecs !! 0) (f . castPtr) --withArray vecs (f . castPtr)-}

withMat44s :: [Mat44] -> (Ptr GLfloat -> IO b) -> IO b
withMat44s mats f = withArray (map (\mat -> (fmap (fmap realToFrac) $ transpose mat :: M44 GLfloat)) mats) (f . castPtr)

withMat33s :: [Mat33] -> (Ptr GLfloat -> IO b) -> IO b
withMat33s mats f = withArray (map (\mat -> (fmap (fmap realToFrac) $ transpose mat :: M33 GLfloat)) mats) (f . castPtr)

uniformMatrix4fv :: GLint -> [Mat44] -> IO ()
uniformMatrix4fv loc mats =
        withMat44s mats $ \ptr ->
            glUniformMatrix4fv loc (genericLength mats) GL_FALSE (castPtr ptr)

uniformMatrix3fv :: GLint -> [Mat33] -> IO ()
uniformMatrix3fv loc mats =
        withMat33s mats $ \ptr ->
            glUniformMatrix3fv loc (genericLength mats) GL_FALSE (castPtr ptr)

vertexAttribPointer :: GLint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLint -> IO ()
vertexAttribPointer a b c d e f = glVertexAttribPointer (fromIntegral a) b c d e (plusPtr nullPtr (fromIntegral f))

bufferData bufType vec usageType _ =
  V.unsafeWith vec \ptr ->
    glBufferData bufType
                  (fromIntegral (V.length vec * sizeOf (V.head vec)))
                  ptr
                  usageType

-- VAO

makeVAO :: IO VAO
makeVAO = withNewPtr (glGenVertexArrays 1)

makeVBO :: IO VAO
makeVBO = withNewPtr (glGenBuffers 1)

bindVAO :: VAO -> IO ()
bindVAO = glBindVertexArray

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
