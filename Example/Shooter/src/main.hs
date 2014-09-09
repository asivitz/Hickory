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

render :: GLFW.Window -> Size Int -> Resources -> Model -> IO ()
render win scrSize (Resources solidShader) Model { _camera } = do
        print "Rendering!"

        whenMaybe solidShader $ \sh -> 
            Draw.drawSpec (v3 300 300 (5)) uiLabel (SolidSquare (Size 50 50) white sh)

        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        let ar = aspectRatio scrSize
            matrix = cameraMatrix _camera ar
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
              run (render win (Size width height) resources) (newModel cam)
