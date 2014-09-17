{-# LANGUAGE NamedFieldPuns #-}

module Systems.Textures where

import Graphics.GLUtils
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Array.Storable
import Graphics.Rendering.OpenGL.Raw.Core31

import Utils.Utils

import Codec.Image.PNG

loadTextureFromPath :: String -> IO (Maybe TexID)
loadTextureFromPath path = do
        res <- loadPNGFile path
        case res of
            Left s -> print s >> return Nothing
            Right image -> do
                let (w, h) = dimensions image
                withStorableArray (imageData image) $ \pd -> do
                    tex <- alloca $ \p -> do
                                glGenTextures 1 p
                                peek p
                    let glLinear  = fromIntegral gl_LINEAR
                        format = if hasAlphaChannel image then gl_RGBA else gl_RGB
                    -- create linear filtered texture
                    glBindTexture gl_TEXTURE_2D tex

                    glTexImage2D gl_TEXTURE_2D 0 (fromIntegral format)
                        (fromIntegral w) (fromIntegral h)
                        0 format gl_UNSIGNED_BYTE pd

                    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glLinear
                    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glLinear
                    return $ Just $ TexID (fromIntegral tex)

loadTexture :: String -> String -> IO (Maybe TexID)
loadTexture resPath image = do
        let prefix = resPath ++ "/images/"
            ipath = prefix ++ image
        tid <- loadTextureFromPath ipath
        whenNothing tid $ print ("Couldn't load texture: " ++ image)
        return tid
