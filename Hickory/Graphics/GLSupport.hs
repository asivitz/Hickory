{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
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

#if defined(ghcjs_HOST_OS)
import Data.Word
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign
import Data.JSString (pack, JSString)
import Graphics.GL.Compatibility41 as GL (GLenum, GLfloat, GLint, GLuint, GLushort, GLboolean, GLsizei, GLintptr,
                                         pattern GL_SRC_ALPHA,
                                         pattern GL_ONE_MINUS_SRC_ALPHA,
                                         pattern GL_ONE,
                                         pattern GL_TEXTURE0,
                                         pattern GL_DITHER,
                                         pattern GL_STENCIL_TEST,
                                         pattern GL_PROGRAM_POINT_SIZE,
                                         pattern GL_BLEND,
                                         pattern GL_COLOR_BUFFER_BIT,
                                         pattern GL_DEPTH_BUFFER_BIT,
                                         pattern GL_FRAGMENT_SHADER,
                                         pattern GL_VERTEX_SHADER,
                                         pattern GL_LINK_STATUS,
                                         pattern GL_COMPILE_STATUS,
                                         pattern GL_SHADER_COMPILER,
                                         pattern GL_INFO_LOG_LENGTH,
                                         pattern GL_LINEAR_MIPMAP_LINEAR,
                                         pattern GL_TRUE,
                                         pattern GL_TEXTURE_MIN_FILTER,
                                         pattern GL_TEXTURE_2D,
                                         pattern GL_LINEAR,
                                         pattern GL_TEXTURE_MAG_FILTER,
                                         pattern GL_UNSIGNED_BYTE,
                                         pattern GL_ELEMENT_ARRAY_BUFFER,
                                         pattern GL_ARRAY_BUFFER,
                                         pattern GL_FALSE,
                                         pattern GL_FLOAT,
                                         pattern GL_UNSIGNED_SHORT,
                                         pattern GL_TRIANGLES,
                                         pattern GL_TRIANGLE_FAN,
                                         pattern GL_TRIANGLE_STRIP,
                                         pattern GL_STREAM_DRAW)
#else
import Graphics.GL.Compatibility41 as GL
#endif
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
import Linear (V4(..), M44, M33, transpose)

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)

data BufDataType = BufFloat | BufUShort

bufferVertices :: VBO -> [GLfloat] -> IO ()
bufferVertices vbo floats = do
        glBindBuffer GL_ARRAY_BUFFER vbo
        bufferData GL_ARRAY_BUFFER floats GL_STREAM_DRAW BufFloat
        return ()

bufferIndices :: VBO -> [GLushort] -> IO ()
bufferIndices vbo ints = do
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER vbo
        bufferData GL_ELEMENT_ARRAY_BUFFER ints GL_STREAM_DRAW BufUShort
        return ()

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
                            QuadFUniform vec -> uniform4fv loc vec
            Nothing -> print $ "Uniform named " ++ name ++ " not found in shader"

attachVertexArray :: GLint -> GLint -> GLint -> GLint -> IO ()
attachVertexArray attrLoc len stride offset = do
        if (attrLoc < 0)
            then (print ("Can't attach vertex array: Bad attribute location" :: String))
            else do
                enableVertexAttribArray attrLoc
                vertexAttribPointer attrLoc len GL_FLOAT GL_FALSE (stride * fsize) (fromIntegral $ offset * fsize)
    where fsize :: GLint
          fsize = fromIntegral $ sizeOf (0 :: GLfloat)

buildVertexGroup :: Shader -> VertexGroup -> IO VBO
buildVertexGroup shader group = do
        vbo <- makeVBO
        attachVertexGroup shader vbo group
        return vbo

attachVertexGroup :: Shader -> VBO -> VertexGroup -> IO ()
attachVertexGroup shader vbo (VertexGroup attachments) = do
        glBindBuffer GL_ARRAY_BUFFER vbo

        let stride = sum $ map (\(Attachment a l) -> l) attachments

        _ <- Fold.foldlM (\offset (Attachment a l) -> do
                attachVertexArray (a shader) l stride offset
                return (offset + l))
            0
            attachments
        return ()

#if defined(ghcjs_HOST_OS)
foreign import javascript safe "gl.clearColor($1,$2,$3,$4)" glClearColor :: Float -> Float -> Float -> Float -> IO ()
foreign import javascript safe "gl.blendFunc($1,$2)" glBlendFunc :: GLenum -> GLenum -> IO ()
foreign import javascript safe "gl.clear($1)" glClear :: GLuint -> IO ()
foreign import javascript safe "gl.activeTexture($1)" glActiveTexture :: GLenum -> IO ()
foreign import javascript safe "gl.disable($1)" glDisable :: GLenum -> IO ()
foreign import javascript safe "gl.enable($1)" glEnable :: GLenum -> IO ()
foreign import javascript safe "\
    var ext = gl.getExtension('OES_vertex_array_object'); \
    ext.bindVertexArrayOES($1); \
    " glBindVertexArray :: VAO -> IO ()
foreign import javascript safe "gl.bindBuffer($1,$2)" glBindBuffer :: GLenum -> VBO -> IO ()
foreign import javascript safe "gl.bindTexture($1,$2)" glBindTexture :: GLenum -> JSVal -> IO ()
foreign import javascript safe "gl.enableVertexAttribArray($1)" enableVertexAttribArray :: GLint -> IO ()
foreign import javascript safe "gl.disableVertexAttribArray($1)" disableVertexAttribArray :: GLint -> IO ()
foreign import javascript safe "gl.drawElements($1, $2, $3, 0);" glDrawElements :: GLenum -> GLsizei -> GLenum -> IO ()
drawElements = glDrawElements

foreign import javascript safe "gl.vertexAttribPointer($1, $2, $3, $4, $5, $6);" glVertexAttribPointer :: GLint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ()
vertexAttribPointer a b c d e f = glVertexAttribPointer a b c d e (fromIntegral f)

foreign import javascript safe " \
    var ext = gl.getExtension('OES_vertex_array_object'); \
    $r = ext.createVertexArrayOES(); \
    " makeVAO :: IO VAO

foreign import javascript safe " \
    var ext = gl.getExtension('OES_vertex_array_object'); \
    ext.bindVertexArrayOES($1); \
    " bindVAO :: VAO -> IO ()

foreign import javascript safe "$r = gl.createBuffer();" makeVBO :: IO VBO

foreign import javascript safe "gl.bufferData($1, new Float32Array($2), $3);" glBufferFloatData :: GLenum -> JSVal -> GLenum -> IO ()
foreign import javascript safe "gl.bufferData($1, new Int16Array($2), $3);" glBufferUShortData :: GLenum -> JSVal -> GLenum -> IO ()

bufferData bufType lst usageType bufDataType = do
        arr <- toJSVal lst
        case bufDataType of
            BufFloat -> glBufferFloatData bufType arr usageType
            BufUShort -> glBufferUShortData bufType arr usageType

foreign import javascript safe "if ($1) { gl.uniform4fv($1, new Float32Array($2)); }" glUniform4fv :: UniformLoc -> JSVal -> IO ()

uniform4fv :: UniformLoc -> V4 Double -> IO ()
uniform4fv loc vec = do
        let lst = toList vec
        arr <- toJSVal lst
        glUniform4fv loc arr

foreign import javascript safe "if ($1) { gl.uniformMatrix4fv($1, false, new Float32Array($2)); }" glUniformMatrix4fv :: UniformLoc -> JSVal -> IO ()

uniformMatrix4fv :: UniformLoc -> Mat44 -> IO ()
uniformMatrix4fv loc mat = do
        let lst = (concatMap toList . toList . transpose) mat
        arr <- toJSVal lst
        glUniformMatrix4fv loc arr

#else

enableVertexAttribArray = glEnableVertexAttribArray . fromIntegral

disableVertexAttribArray :: GLint -> IO ()
disableVertexAttribArray = glDisableVertexAttribArray . fromIntegral

drawElements :: GLenum -> GLsizei -> GLenum -> IO ()
drawElements a b c = glDrawElements a b c nullPtr

uniform4fv :: GLint -> [V4 Double] -> IO ()
uniform4fv loc vs =
        withVec4s vs $ \ptr ->
            glUniform4fv loc (genericLength vs) (castPtr ptr)

withVec4s :: [V4 Double] -> (Ptr GLfloat -> IO b) -> IO b
withVec4s vecs f = withArray (map (fmap realToFrac) vecs :: [V4 GLfloat]) (f . castPtr)

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

bufferData bufType lst usageType _ =
        withArrayLen lst $ \len ptr ->
            glBufferData bufType
                         (fromIntegral (len * sizeOf (head lst)))
                         ptr
                         usageType

-- VAO

makeVAO :: IO VAO
makeVAO = withNewPtr (glGenVertexArrays 1)

makeVBO :: IO VAO
makeVBO = withNewPtr (glGenBuffers 1)

bindVAO :: VAO -> IO ()
bindVAO = glBindVertexArray

#endif

configGLState :: GLfloat -> GLfloat -> GLfloat -> IO ()
configGLState r g b = do
        glClearColor r g b 1
#if defined(ghcjs_HOST_OS)
        glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA
#else
        glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
#endif
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
        glClear (GL_DEPTH_BUFFER_BIT)
