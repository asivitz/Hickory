{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systems.Draw (SysData(..), make, makeDrawData, empty, nullSize, createVAOConfig, indexVAOConfig, reserveShader, releaseShader) where
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
import Data.IORef
import Data.Traversable
import qualified Graphics.UI.GLFW          as GLFW

import qualified Systems.Textures as Textures
import qualified Systems.Camera as Camera

import Foreign.C.String
import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Graphics.GLFWUtils

data SysData = SysData { 
             window :: Maybe (GLFW.Window),
             screenSize :: Size Int,
             worldCamera :: Mat44,
             worldProjection :: Mat44,
             shaders :: RefStore (String,String) Shader,
             vanillaShader :: Maybe Shader
             }

empty = SysData { window = Nothing,
                screenSize = nullSize, 
                worldCamera = mat44Identity,
                worldProjection = mat44Identity,
                shaders = emptyRefStore,
                vanillaShader = Nothing }

{-empty = SysData { screenSize = (Size 0 0), window = fromC nullPtr }-}

makeDrawData :: IO (IORef SysData)
makeDrawData = do
        win <- buildWindow 400 400 "Hi hi!"
        newIORef empty { window = win }

make draw texes camera = System (run draw camera) (handleEv draw) (initS draw texes)

runDrawable :: Double -> Entity -> Drawable -> SysMonad IO Drawable
runDrawable delta e dr@(Square size color tex shader) = do
      ds <- compForEnt e
      liftIO $ whenMaybe ds $ \(DrawState pos) -> drawSquare pos size color tex shader
      return dr

drawSquare :: Vec -> FSize -> Color -> TexID -> Shader -> IO ()
drawSquare (V2 x y) (Size w h) color tex shader = 
      addDrawCommand model color color tex shader worldLabel 0.0 True >> return ()
         where model = mat44Scale w h 1 $ mat44Translate x y 0 $ mat44Identity

handleEv draw _ = return ()

run draw camera delta = 
        do
            upCompsM (runDrawable delta) drawables
            SysData {
                    screenSize = (Size w h),
                    window,
                    worldCamera,
                    worldProjection,
                    vanillaShader
                    } <- getSysData draw

            {-let model = mat44Scale 30 30 1 mat44Identity-}

            liftIO $ do
                renderCommands worldProjection worldLabel
                {-renderCommands uiProjection 1 -}
                resetRenderer

                traverse GLFW.swapBuffers window
                GLFW.pollEvents

            q <- liftIO $ traverse GLFW.windowShouldClose window
            when (maybe False id q) $ do
                broadcast Quit
                liftIO $ do
                    traverse GLFW.destroyWindow window
                    GLFW.terminate

            return ()

initS draw texes = do
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
        
        let ortho = mat44Ortho (0 :: Float) (fromIntegral fbWidth) (0 :: Float) (fromIntegral fbHeight) (-20) 1
        putSysData draw sd {
                        screenSize = (Size fbWidth fbHeight),
                        worldProjection = ortho,
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
