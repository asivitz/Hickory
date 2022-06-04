{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns, BlockArguments #-}

module Hickory.Graphics.Textures
  (loadTexture, loadTexture', loadTextures, texLoadDefaults, TexLoadOptions(..), TexID(..), mkTextureWith ) where

import Data.Vector.Storable(unsafeWith)
import Hickory.Utils.Utils
import Codec.Picture
import Codec.Picture.Extra (flipVertically)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Ptr (Ptr)

import Hickory.Graphics.GLSupport (alloc1, checkForErrors)
import Graphics.GL.Compatibility30 as GL
import GHC.Stack (HasCallStack)

newtype TexID = TexID { getTexID :: Word32 } deriving (Show)

data TexLoadOptions = TexLoadOptions
  { wrap :: GLenum
  , magFilter :: GLenum
  , minFilter :: GLenum
  , flipY :: Bool
  }

mkTextureWith :: HasCallStack => (GLuint -> IO ()) -> IO TexID
mkTextureWith f = do
  tex <- alloc1 glGenTextures
  glBindTexture GL_TEXTURE_2D tex
  f tex
  checkForErrors
  pure $ TexID tex


loadGLTex :: HasCallStack => Word32 -> GLenum -> Int -> Int -> TexLoadOptions -> Ptr a -> IO TexID
loadGLTex internalFormat format w h TexLoadOptions { wrap, magFilter, minFilter } ptr = mkTextureWith \_tex -> do
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral internalFormat)
      (fromIntegral w) (fromIntegral h)
      0 format GL_UNSIGNED_BYTE ptr

  glGenerateMipmap GL_TEXTURE_2D

  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral magFilter)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral minFilter)

  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral wrap)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral wrap)

loadTextureFromPath :: HasCallStack => String -> TexLoadOptions -> IO (Maybe TexID)
loadTextureFromPath path loadOpts = do
  res <- readPng path

  case res of
    Left s -> print s >> pure Nothing
    Right image -> Just <$> case image of
        -- TODO: Refactor and handle more cases
        ImageRGBA8 (doFlip -> Image w h dat) ->
            unsafeWith dat $ \ptr -> loadGLTex GL_RGBA8 GL_RGBA w h loadOpts ptr
        ImageRGB8 (doFlip -> Image w h dat) ->
            unsafeWith dat $ \ptr -> loadGLTex GL_RGB8 GL_RGB w h loadOpts ptr
        _ -> error "Error loading texture: Unknown image format"
  where
  doFlip :: forall a. Pixel a => Image a -> Image a
  doFlip img = if flipY loadOpts then flipVertically img else img

loadTexture :: String -> String -> TexLoadOptions -> IO (Maybe TexID)
loadTexture resPath image loadOpts = do
  let prefix = resPath ++ "/images/"
      ipath = prefix ++ image
  tid <- loadTextureFromPath ipath loadOpts
  whenNothing tid $ print ("Couldn't load texture: " ++ image)
  return tid

loadTexture' :: String -> (String, TexLoadOptions) -> IO TexID
loadTexture' path (image, opts) = do
  tex <- loadTexture path image opts
  case tex of
      Just t -> return t
      Nothing -> error ("Can't load texture " ++ image)

loadTextures :: String -> [(String, TexLoadOptions)] -> IO (HashMap.HashMap Text.Text TexID)
loadTextures path images = do
  loaded <- mapM (loadTexture' path) images
  pure $ HashMap.fromList $ zip (map (Text.pack . fst) images) loaded

texLoadDefaults :: TexLoadOptions
texLoadDefaults = TexLoadOptions { wrap = GL_REPEAT, minFilter = GL_LINEAR_MIPMAP_LINEAR, magFilter = GL_LINEAR, flipY = True }
