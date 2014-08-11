{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systems.Draw (SysData(..), make, makeDrawData, empty, nullSize, createVAOConfig, indexVAOConfig, reserveShader, releaseShader, drawSpec) where
import Control.Monad.State

import Engine.Entity
import Engine.System
import Engine.Event
import Engine.Component

import Types.Color
import Types.Types

import Utils.Resources
import Utils.Utils

import Graphics.GLUtils
import Math.Matrix
import Math.Vector
import Math.VectorMatrix
import Data.IORef
import Data.Traversable
import qualified Graphics.UI.GLFW          as GLFW

import Camera.Camera

import qualified Systems.WorldCamera as WorldCamera
import qualified Systems.UICamera as UICamera

import Foreign.C.String
import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Graphics.GLFWUtils
import Data.Bits

data SysData = SysData { 
             window :: Maybe (GLFW.Window),
             screenSize :: Size Int,
             shaders :: RefStore (String,String) Shader,
             vanillaShader :: Maybe Shader,
             worldMatrix :: Mat44
             }

empty = SysData { window = Nothing,
                screenSize = nullSize, 
                shaders = emptyRefStore,
                vanillaShader = Nothing,
                worldMatrix = mat44Identity
                }

makeDrawData :: IO (IORef SysData)
makeDrawData = do
        win <- buildWindow 400 400 "Hi hi!"
        newIORef empty { window = win }

make draw worldcamera uicamera = System (run draw worldcamera uicamera) (initS draw)

runDrawable :: Double -> Entity -> Drawable -> SysMonad IO Drawable
runDrawable delta e dr@(Drawable spec) = do
      ds <- compForEnt e
      liftIO $ whenMaybe ds $ \(DrawState pos) -> drawSpec pos worldLabel spec
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

run draw worldcamera uicamera delta = 
        do
            RPC { quit } <- getRPC
            upCompsM (runDrawable delta) drawables
            sd@SysData {
                    screenSize,
                    window,
                    vanillaShader
                    } <- getSysData draw

            WorldCamera.SysData { WorldCamera.camera = worldcam } <- getSysData worldcamera
            UICamera.SysData { UICamera.camera = uicam } <- getSysData uicamera

            let ar = aspectRatio screenSize
                worldMatrix' = cameraMatrix worldcam ar

            putSysData draw sd { worldMatrix = worldMatrix' }

            liftIO $ do
                glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
                renderCommandsWithCamera uicam backgroundLabel ar
                renderCommands worldMatrix' worldLabel
                renderCommandsWithCamera uicam uiLabel ar

                resetRenderer

                traverse GLFW.swapBuffers window
                GLFW.pollEvents

            q <- liftIO $ traverse GLFW.windowShouldClose window
            when (maybe False id q) $ do
                sequence_ quit
                liftIO $ do
                    traverse GLFW.destroyWindow window
                    GLFW.terminate

            return ()

initS draw = do
        SysData {window = win} <- getSysData draw
        
        (fbWidth, fbHeight) <- case win of
            Nothing -> return (0,0)
            Just w -> liftIO $ GLFW.getFramebufferSize w

        {-liftIO $ GLFW.swapInterval 0-}

        liftIO $ do
            initRenderer
            glClearColor 1 0 0 1
            glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
            glActiveTexture gl_TEXTURE0
            
            glEnable gl_PROGRAM_POINT_SIZE -- for OSX

        vanilla <- reserveShader draw ("Shader.vsh", "Shader.fsh")

        sd <- getSysData draw
        
        putSysData draw sd {
                        screenSize = (Size fbWidth fbHeight),
                        vanillaShader = vanilla
                        }

reserveShader :: IORef SysData -> (String,String) -> SysMonad IO (Maybe Shader)
reserveShader draw vertfragpair = do
   mydata@SysData { shaders } <- getSysData draw
   (newshaders, shader) <- liftIO $ reserve shaders vertfragpair (\(v,f) -> loadShader v f)
   putSysData draw mydata { shaders = newshaders }
   return shader

releaseShader :: IORef SysData -> (String,String) -> SysMonad IO ()
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

reserveParticleShader :: IORef SysData -> SysMonad IO (Maybe ParticleShader)
reserveParticleShader  draw = do
        shader <- reserveShader draw ("ParticleShader.vsh", "ParticleShader.fsh")
        case shader of
            Nothing -> return Nothing
            Just s -> do
                loc <- liftIO $ getUniformLoc s "size"
                return $ Just $ ParticleShader s (UniformLoc loc)
