module Textures.GLLoad where

import GLInterface.GLSupport

-- create linear filtered texture
loadGLTex format w h ptr = do
    tex <- genTexture
    glBindTexture GL_TEXTURE_2D tex

    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral format)
        (fromIntegral w) (fromIntegral h)
        0 format GL_UNSIGNED_BYTE ptr

    glGenerateMipmap GL_TEXTURE_2D

    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
    return $ Just $ TexID (fromIntegral tex)
