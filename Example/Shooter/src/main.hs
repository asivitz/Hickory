{-# LANGUAGE NamedFieldPuns #-}
 
import Engine.Run
import Engine.World
import Bootstrap.Bootstrap
import Graphics.GLFWUtils
import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Data.Bits
import qualified Graphics.UI.GLFW as GLFW
import Types.Types
import Camera.Camera
import Math.Vector
import qualified Systems.Draw as Draw
import qualified Systems.GLFWPlatform as GLFWPlatform
import Types.Color
import Utils.Utils

data Resources = Resources {
               solidShader :: Maybe Shader
               }

loadShader' resPath vert frag = do
   let prefix = resPath ++ "/Shaders/"
   let (vsp, fsp) = ( prefix ++ vert, prefix ++ frag)

   shader <- loadShader vsp fsp
   return shader
        

loadResources :: String -> IO Resources
loadResources path = do
        solid <- loadShader' path "Shader.vsh" "SolidColor.fsh"
        return $ Resources solid

render (Resources solidShader) model = do
        print "Rendering!"

        whenMaybe solidShader $ \sh -> 
            Draw.drawSpec (v3 300 300 (5)) uiLabel (SolidSquare (Size 50 50) white sh)

glfwRender :: GLFW.Window -> Size Int -> (Model -> IO ()) -> Model -> IO ()
glfwRender win scrSize render model = do
        render model

        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        let ar = aspectRatio scrSize
            matrix = cameraMatrix (_camera model) ar
        renderCommands matrix uiLabel

        resetRenderer
        GLFW.swapBuffers win

main :: IO ()
main = do 
          withWindow 640 480 "MVC!" $ \win -> do
              initRenderer
              glClearColor 0.3 0.5 0 1
              glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
              glActiveTexture gl_TEXTURE0
                
              glEnable gl_PROGRAM_POINT_SIZE -- for OSX

              (width, height) <- GLFW.getFramebufferSize win

              resources <- loadResources "Example/HFreecell/resources"
              let cam = Camera (Ortho 800 (-20) 1) (Route pZero Nothing)

              grabInputFunc <- GLFWPlatform.makeGrabInput win

              run (glfwRender win (Size width height) (render resources)) grabInputFunc (newModel cam)
