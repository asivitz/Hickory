{-# LANGUAGE NamedFieldPuns #-}

module Hickory.Graphics.Textures (loadTexture, loadTexture', loadTextures) where

import Data.Vector.Storable(unsafeWith)

import Hickory.Graphics.GLSupport
import Hickory.Graphics.Drawing

import Hickory.Utils.Utils

import Codec.Picture
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Foreign.Marshal.Alloc
import Foreign.Storable

#if defined(ghcjs_HOST_OS)
import Data.JSString (pack, JSString)

foreign import javascript safe " \
    var tex = gl.createTexture(); \
    tex.image = new Image(); \
    tex.image.onload = function() { \
        gl.bindTexture(gl.TEXTURE_2D, tex); \
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, tex.image); \
        gl.generateMipmap(gl.TEXTURE_2D); \
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR); \
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR); \
    }; \
    tex.image.src = $1 + '/images/' + $2; \
    $r = tex;" loadTexture'' :: JSString -> JSString -> IO TexID

loadTexture' a b = loadTexture'' (pack a) (pack b)
loadTexture a b = loadTexture' a b >>= return . Just
#else
import Graphics.GL.Compatibility41 as GL
-- Textures
genTexture = alloca $ \p ->
    do
        glGenTextures 1 p
        peek p

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

loadTextureFromPath :: String -> IO (Maybe TexID)
loadTextureFromPath path = do
        res <- readPng path
        case res of
            Left s -> print s >> return Nothing
            Right image -> case image of
                -- TODO: Refactor and handle more cases
                ImageRGBA8 (Image w h dat) ->
                    unsafeWith dat $ \ptr -> loadGLTex GL_RGBA w h ptr
                ImageRGB8 (Image w h dat) ->
                    unsafeWith dat $ \ptr -> loadGLTex GL_RGB w h ptr
                _ -> error "Error loading texture: Unknown image format"

loadTexture :: String -> String -> IO (Maybe TexID)
loadTexture resPath image = do
        let prefix = resPath ++ "/images/"
            ipath = prefix ++ image
        tid <- loadTextureFromPath ipath
        whenNothing tid $ print ("Couldn't load texture: " ++ image)
        return tid

loadTexture' :: String -> String -> IO TexID
loadTexture' path image = do
        tex <- loadTexture path image
        case tex of
            Just t -> return t
            Nothing -> error ("Can't load texture " ++ image)
#endif

loadTextures :: String -> [String] -> IO (HashMap.HashMap Text.Text TexID)
loadTextures path images = do
        loaded <- mapM (loadTexture' path) images
        return $ HashMap.fromList $ zip (map Text.pack images) loaded
