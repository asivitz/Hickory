{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systems.Draw (make, createVAOConfig, indexVAOConfig, reserveShader, releaseShader, drawSpec) where
import Control.Monad.State

import Engine.System
import Engine.World
import Engine.Component

import Types.Types

import Utils.Resources
import Utils.Utils

import Graphics.GLUtils
import Math.Matrix
import Math.Vector
import Math.VectorMatrix
import Data.IORef

import Camera.Camera

import Foreign.C.String
import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Data.Bits

data SysData = SysData { 
             shaders :: RefStore (String,String) Shader,
             vanillaShader :: Maybe Shader,
             worldMatrix :: Mat44
             }

empty = SysData { shaders = emptyRefStore,
                vanillaShader = Nothing,
                worldMatrix = mat44Identity
                }

worldmat' draw = do
        SysData { worldMatrix } <- getSysData draw
        return worldMatrix

make :: SysMonad c IO (System c)
make = do
        draw <- liftIO $ newIORef empty

        registerResource drawnWorldMatrix (worldmat' draw)

        initS draw
        return $ System (run draw)

runDrawable :: Double -> Drawable -> DrawState -> SysMonad c IO Drawable
runDrawable delta dr@(Drawable spec) (DrawState pos) = do
      liftIO $ drawSpec pos worldLabel spec
      return dr

drawSpec :: Vector3 -> Label -> DrawSpec -> IO ()
drawSpec pos label (Square (Size w h) color tex shader) = 
      addDrawCommand model color color tex shader label 0.0 True >> return ()
         where model = mat44Scale w h 1 $ mat44TranslateV pos mat44Identity
drawSpec pos label (SolidSquare (Size w h) color shader) = 
      addDrawCommand model color color nullTex shader label 0.0 True >> return ()
         where model = mat44Scale w h 1 $ mat44TranslateV pos mat44Identity

renderCommandsWithCamera :: Camera -> Label -> Float -> IO ()
renderCommandsWithCamera cam label aspect = renderCommands matrix label
    where matrix = cameraMatrix cam aspect

run draw delta = 
        do
            RPC { _screenSize, _worldCamera, _uiCamera } <- getRPC
            updateCompsM2 (runDrawable delta) drawables drawStates
            sd <- getSysData draw

            ss <- _screenSize

            mworldCam <- _worldCamera
            muiCam <- _uiCamera

            whenMaybe2 mworldCam muiCam $ \worldcam uicam -> do

                let ar = aspectRatio ss
                    worldMatrix' = cameraMatrix worldcam ar

                putSysData draw sd { worldMatrix = worldMatrix' }

                liftIO $ do
                    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
                    renderCommandsWithCamera uicam backgroundLabel ar
                    renderCommands worldMatrix' worldLabel
                    renderCommandsWithCamera uicam uiLabel ar

                    resetRenderer

                return ()

initS draw = do
        registerResource reserveShader (reserveShader' draw)
        
        {-liftIO $ GLFW.swapInterval 0-}

        liftIO $ do
            initRenderer
            glClearColor 1 0 0 1
            glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
            glActiveTexture gl_TEXTURE0
            
            glEnable gl_PROGRAM_POINT_SIZE -- for OSX

        vanilla <- reserveShader' draw ("Shader.vsh", "Shader.fsh")

        sd <- getSysData draw
        
        putSysData draw sd {
                        vanillaShader = vanilla
                        }

reserveShader' :: IORef SysData -> (String,String) -> SysMonad c IO (Maybe Shader)
reserveShader' draw vertfragpair = do
   mydata@SysData { shaders } <- getSysData draw
   (newshaders, shader) <- liftIO $ reserve shaders vertfragpair (\(v,f) -> loadShader v f)
   putSysData draw mydata { shaders = newshaders }
   return shader

releaseShader :: IORef SysData -> (String,String) -> SysMonad c IO ()
releaseShader draw pair = do
   mydata@SysData { shaders } <- getSysData draw
   newshaders <- liftIO $ release shaders pair deleteShader
   putSysData draw mydata { shaders = newshaders }

deleteShader :: Shader -> IO ()
deleteShader shader = return ()

data ParticleShader = ParticleShader Shader UniformLoc

getUniformLoc (Shader s) name =
        withCString name $ \ptrname ->
            glGetUniformLocation s ptrname

reserveParticleShader :: IORef SysData -> SysMonad c IO (Maybe ParticleShader)
reserveParticleShader  draw = do
        shader <- reserveShader' draw ("ParticleShader.vsh", "ParticleShader.fsh")
        case shader of
            Nothing -> return Nothing
            Just s -> do
                loc <- liftIO $ getUniformLoc s "size"
                return $ Just $ ParticleShader s (UniformLoc loc)
