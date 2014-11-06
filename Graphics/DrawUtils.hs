{-# LANGUAGE NamedFieldPuns #-}

module Graphics.DrawUtils where

import Types.Types

import Graphics.GLUtils
import Graphics.Shader
import Math.Matrix
import Math.Vector
import Math.VectorMatrix

import Graphics.Drawing
import Types.Color

data DrawSpec = Square FSize Color TexID Shader
              | SolidSquare FSize Color Shader
              deriving (Show)

drawSpec :: Vector3 -> Layer -> DrawSpec -> IO ()
drawSpec pos layer (Square (Size w h) color tex shader) = 
      addDrawCommand model color color tex shader layer (realToFrac . v3z $ pos) True >> return ()
         where model = mat44Scale w h 1 $ mat44TranslateV pos mat44Identity
drawSpec pos layer (SolidSquare (Size w h) color shader) = 
      addDrawCommand model color color nullTex shader layer (realToFrac . v3z $ pos) True >> return ()
         where model = mat44Scale w h 1 $ mat44TranslateV pos mat44Identity

drawSpecRot pos rot layer (Square (Size w h) color tex shader) = 
      addDrawCommand model color color tex shader layer (realToFrac . v3z $ pos) True
         where model = mat44Scale w h 1 $ mat44Rotate 0 0 1 (realToFrac rot) $ mat44TranslateV pos mat44Identity

data ParticleShader = ParticleShader Shader UniformLoc

{-
reserveParticleShader :: IORef SysData -> SysMonad c IO (Maybe ParticleShader)
reserveParticleShader  draw = do
        shader <- reserveShader' draw ("ParticleShader.vsh", "ParticleShader.fsh")
        case shader of
            Nothing -> return Nothing
            Just s -> do
                loc <- liftIO $ getUniformLoc s "size"
                return $ Just $ ParticleShader s (UniformLoc loc)
                -}
