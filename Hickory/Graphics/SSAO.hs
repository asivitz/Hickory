module Hickory.Graphics.SSAO where

import Graphics.GL.Compatibility41
import Hickory.Graphics.GLSupport (alloc1)
import Foreign.Marshal.Array (withArray)
import Control.Monad (forM)
import System.Random (randomRIO)
import Linear (V3(..), normalize)
import Hickory.Graphics.Textures (TexID(..))

noiseTexture :: IO TexID
noiseTexture = do
  arr :: [V3 Float] <- forM [0..16 :: Int] \_ -> do
    let gen = randomRIO (0,1)
    normalize <$> (V3 <$> gen <*> gen <*> gen)

  tex <- alloc1 glGenTextures
  glBindTexture GL_TEXTURE_2D tex
  withArray arr $ \ptr ->
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA16F) 4 4 0 GL_RGB GL_FLOAT ptr

  glTexParameteri GL_TEXTURE_2D  GL_TEXTURE_MIN_FILTER  (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D  GL_TEXTURE_MAG_FILTER  (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D  GL_TEXTURE_WRAP_S  (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D  GL_TEXTURE_WRAP_T  (fromIntegral GL_REPEAT)
  pure $ TexID tex
