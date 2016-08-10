{-# LANGUAGE NamedFieldPuns #-}

module Textures.Textures (loadTexture, loadTexture', loadTextures) where

import Graphics.GLUtils
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Vector.Storable(unsafeWith)

import Graphics.GLSupport

import Utils.Utils

import Codec.Picture
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

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
                        let format = GL_RGBA
                        -- create linear filtered texture
                        glBindTexture GL_TEXTURE_2D tex

                        glTexImage2D GL_TEXTURE_2D 0 (fromIntegral format)
                            (fromIntegral w) (fromIntegral h)
                            0 format GL_UNSIGNED_BYTE ptr

                        glGenerateMipmap GL_TEXTURE_2D

                        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
                        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
                        return $ Just $ TexID (fromIntegral tex)
                ImageRGB8 (Image w h dat) ->
                    unsafeWith dat $ \ptr -> do
                        tex <- alloca $ \p -> do
                                    glGenTextures 1 p
                                    peek p
                        let format = GL_RGB
                        -- create linear filtered texture
                        glBindTexture GL_TEXTURE_2D tex

                        glTexImage2D GL_TEXTURE_2D 0 (fromIntegral format)
                            (fromIntegral w) (fromIntegral h)
                            0 format GL_UNSIGNED_BYTE ptr

                        glGenerateMipmap GL_TEXTURE_2D

                        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
                        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
                        return $ Just $ TexID (fromIntegral tex)
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

loadTextures :: String -> [String] -> IO (HashMap.HashMap Text.Text TexID)
loadTextures path images = do
        loaded <- mapM (loadTexture' path) images
        return $ HashMap.fromList $ zip (map Text.pack images) loaded
