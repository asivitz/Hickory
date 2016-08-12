{-# LANGUAGE PatternSynonyms #-}

{-
module Graphics.GLSupport ( module Graphics.GL.ARB.ES2Compatibility ) where
import Graphics.GL.ARB.ES2Compatibility
-}

module GLInterface.GLSupport --( module Graphics.GL.Compatibility41)
    (genTexture,
     TexID(..),
     getTexID,
     bindVAO,
     GL.GLenum,
     GL.GLint,
     GL.GLuint,
     GL.GLfloat,
     GL.GLushort,
     pattern GL.GL_TEXTURE_2D,
     pattern GL.GL_TEXTURE_MAG_FILTER,
     pattern GL.GL_LINEAR,
     pattern GL.GL_TEXTURE_MIN_FILTER,
     pattern GL.GL_LINEAR_MIPMAP_LINEAR,
     pattern GL.GL_UNSIGNED_BYTE,
     pattern GL.GL_FLOAT,
     pattern GL.GL_RGB,
     pattern GL.GL_RGBA,
     pattern GL.GL_FALSE,
     pattern GL.GL_TRUE,
     GL.glTexParameteri,
     GL.glGenerateMipmap,
     GL.glTexImage2D,
     GL.glBindTexture,
     GL.glVertexAttribPointer,
     GL.glEnableVertexAttribArray,
     GL.glUseProgram,
     GL.glGetUniformLocation,
     GL.glGetAttribLocation,
     GL.glDeleteShader,
     GL.glCreateProgram,
     GL.glAttachShader,
     GL.glLinkProgram,
     GL.glCompileShader,
     pattern GL.GL_LINK_STATUS,
     pattern GL.GL_VERTEX_SHADER,
     pattern GL.GL_FRAGMENT_SHADER,
     pattern GL.GL_COMPILE_STATUS,
     GL.glGetProgramInfoLog,
     GL.glGetShaderInfoLog,
     GL.glGetBooleanv,
     GL.glGetShaderiv,
     GL.glGetProgramiv,
     pattern GL.GL_INFO_LOG_LENGTH,
     pattern GL.GL_SHADER_COMPILER,
     GL.glCreateShader,
     GL.glShaderSource,
     GL.glGenBuffers,
     GL.glGenVertexArrays,
     pattern GL.GL_STREAM_DRAW,
     pattern GL.GL_ARRAY_BUFFER,
     pattern GL.GL_ELEMENT_ARRAY_BUFFER,
     GL.glBindBuffer,
     pattern GL.GL_UNSIGNED_SHORT,
     GL.glDrawElements,
     GL.glBindVertexArray,
     GL.glUniformMatrix4fv,
     GL.glUniform4fv,
     GL.glBufferData,
     GL.glUnmapBuffer,
     pattern GL.GL_TRIANGLE_FAN,
     pattern GL.GL_TRIANGLE_STRIP,
     pattern GL.GL_TRIANGLES,
     pattern GL.GL_DEPTH_BUFFER_BIT,
     pattern GL.GL_COLOR_BUFFER_BIT,
     GL.glClear ,
     pattern GL.GL_PROGRAM_POINT_SIZE,
     pattern GL.GL_DEPTH_TEST,
     pattern GL.GL_STENCIL_TEST,
     pattern GL.GL_BLEND,
     pattern GL.GL_DITHER,
     GL.glDisable ,
     GL.glEnable ,
     GL.glActiveTexture ,
     pattern GL.GL_TEXTURE0,
     pattern GL.GL_ONE_MINUS_SRC_ALPHA,
     pattern GL.GL_SRC_ALPHA,
     GL.glBlendFunc,
     GL.glClearColor
     )
    where

import qualified Graphics.GL.Compatibility41 as GL
import Foreign.Marshal.Alloc
import Foreign.Storable

newtype TexID = TexID GL.GLint deriving (Eq, Ord, Show)

getTexID (TexID num) = num

genTexture = alloca $ \p ->
    do
        GL.glGenTextures 1 p
        peek p

bindVAO :: GL.GLuint -> IO ()
bindVAO = GL.glBindVertexArray

