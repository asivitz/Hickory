{-# LANGUAGE NamedFieldPuns #-}

module Textures.Textures (loadTexture) where

import Graphics.GLUtils
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Vector.Storable(unsafeWith)

import Graphics.Rendering.OpenGL.Raw.Core31

import Utils.Utils

import Codec.Picture

loadTextureFromPath :: String -> IO (Maybe TexID)
loadTextureFromPath path = do
        res <- readPng path
        case res of
            Left s -> print s >> return Nothing
            Right image -> case image of
                -- TODO: Refactor and handle more cases
                ImageRGBA8 (Image w h dat) ->
                    unsafeWith dat $ \ptr -> do
                        tex <- alloca $ \p -> do
                                    glGenTextures 1 p
                                    peek p
                        let format = gl_RGBA
                        -- create linear filtered texture
                        glBindTexture gl_TEXTURE_2D tex

                        glTexImage2D gl_TEXTURE_2D 0 (fromIntegral format)
                            (fromIntegral w) (fromIntegral h)
                            0 format gl_UNSIGNED_BYTE ptr

                        glGenerateMipmap(gl_TEXTURE_2D)

                        glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_LINEAR)
                        glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_LINEAR_MIPMAP_LINEAR)
                        return $ Just $ TexID (fromIntegral tex)
                ImageRGB8 (Image w h dat) ->
                    unsafeWith dat $ \ptr -> do
                        tex <- alloca $ \p -> do
                                    glGenTextures 1 p
                                    peek p
                        let format = gl_RGB
                        -- create linear filtered texture
                        glBindTexture gl_TEXTURE_2D tex

                        glTexImage2D gl_TEXTURE_2D 0 (fromIntegral format)
                            (fromIntegral w) (fromIntegral h)
                            0 format gl_UNSIGNED_BYTE ptr

                        glGenerateMipmap(gl_TEXTURE_2D)

                        glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_LINEAR)
                        glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_LINEAR_MIPMAP_LINEAR)
                        return $ Just $ TexID (fromIntegral tex)

loadTexture :: String -> String -> IO (Maybe TexID)
loadTexture resPath image = do
        let prefix = resPath ++ "/images/"
            ipath = prefix ++ image
        tid <- loadTextureFromPath ipath
        whenNothing tid $ print ("Couldn't load texture: " ++ image)
        return tid
