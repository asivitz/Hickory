{-# LANGUAGE NamedFieldPuns #-}

module Textures.Textures (loadTexture, loadTexture', loadTextures) where

import Data.Vector.Storable(unsafeWith)

import GLInterface.GLSupport
import Graphics.Drawing

import Utils.Utils

import Codec.Picture
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

#if defined(ghcjs_HOST_OS)
foreign import javascript safe "
    var tex = gl.createTexture(); \
    tex.image = new Image(); \
    tex.image.onload = function() { \
        gl.bindTexture(gl.TEXTURE_2D, tex); \
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, tex.image); \
        gl.generateMipmap(GL_TEXTURE_2D); \
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR); \
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR); \
    }; \
    tex.image.src = $1 + "/images/" + $2; \
    $r = tex;" loadTexture' :: String -> String -> IO TexID
#else
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
