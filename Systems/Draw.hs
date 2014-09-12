{-# LANGUAGE NamedFieldPuns #-}

module Systems.Draw where

import Types.Types

import Graphics.GLUtils
import Math.Matrix
import Math.Vector
import Math.VectorMatrix

import Camera.Camera

import Foreign.C.String
import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31

drawSpec :: Vector3 -> Label -> DrawSpec -> IO ()
drawSpec pos label (Square (Size w h) color tex shader) = 
      addDrawCommand model color color tex shader label (realToFrac . v3z $ pos) True >> return ()
         where model = mat44Scale w h 1 $ mat44TranslateV pos mat44Identity
drawSpec pos label (SolidSquare (Size w h) color shader) = 
      addDrawCommand model color color nullTex shader label (realToFrac . v3z $ pos) True >> return ()
         where model = mat44Scale w h 1 $ mat44TranslateV pos mat44Identity

renderCommandsWithCamera :: Camera -> Label -> Float -> IO ()
renderCommandsWithCamera cam label aspect = renderCommands matrix label
    where matrix = cameraMatrix cam aspect

    {-
    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    renderCommandsWithCamera uicam backgroundLabel ar
    renderCommands worldMatrix' worldLabel
    renderCommandsWithCamera uicam uiLabel ar

    resetRenderer
    -}

loadShader resPath vert frag = do
   let prefix = resPath ++ "/Shaders/"
   let (vsp, fsp) = ( prefix ++ vert, prefix ++ frag)

   shader <- loadShaderFromPaths vsp fsp
   return shader

data ParticleShader = ParticleShader Shader UniformLoc

getUniformLoc (Shader s) name =
        withCString name $ \ptrname ->
            glGetUniformLocation s ptrname

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
