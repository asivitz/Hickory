{-# LANGUAGE NamedFieldPuns #-}

module Graphics.DrawUtils where

import Types.Types

import Graphics.GLUtils
import Graphics.Shader
import Math.Matrix
import Math.Vector
import Math.VectorMatrix
import Data.List
import Data.Maybe

import Graphics.Drawing
import Types.Color

data DrawSpec = Square Color (Maybe TexID) Shader
              deriving (Show)

drawSpec :: Mat44 -> RenderLayer -> DrawSpec -> IO ()
drawSpec mat layer (Square color tex shader) =
      addDrawCommand mat color color (fromMaybe nullTex tex) shader layer (realToFrac depth) True >> return ()
    where depth = v4z $ mat44MulVec4 mat (v4 0 0 0 1)

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

sizePosMat :: Size Float -> V3 -> Mat44
sizePosMat (Size w h) pos = mat44Scale w h 1 $ mat44TranslateV pos mat44Identity

sizePosRotMat :: Real a => Size Float -> V3 -> a -> Mat44
sizePosRotMat (Size w h) pos rot = mat44Scale w h 1 $ mat44Rotate 0 0 1 (realToFrac rot) $ mat44TranslateV pos mat44Identity

data RenderTree = RSquare Mat44 Color (Maybe TexID) Shader
                | List [RenderTree]
                | NoRender
                deriving (Show)

{-rtDepth :: RenderTree -> Scalar-}
{-rtDepth (RSquare _ (Vector3 _ _ z) _ _ _) = z-}
{-rtDepth _ = 0-}

renderTree :: RenderLayer -> RenderTree -> IO ()
renderTree _ NoRender = return ()
renderTree layer (RSquare mat color tex shader) = drawSpec mat layer (Square color tex shader)
renderTree layer (List children) = mapM_ (renderTree layer) children -- (sortOn rtDepth children)

