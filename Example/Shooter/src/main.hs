{-# LANGUAGE NamedFieldPuns #-}
 
import Engine.Scene.Scene
import Types.Types
import Systems.Draw
import Types.Color
import Utils.Utils
import Engine.Scene.Input
import Math.Vector
import Math.VectorMatrix
import Camera.Camera
import Graphics.Drawing
import GLFW.Run

data Resources = Resources {
               solidShader :: Maybe Shader
               }

loadResources :: String -> IO Resources
loadResources path = do
        solid <- loadShader path "Shader.vsh" "SolidColor.fsh"
        return $ Resources solid

data Model = Model { playerPos :: V2,
                     bullets :: [V2]
                     }

genCamera (Size w h) = 
        let proj = Ortho (realToFrac w) 1 100 True
            in Camera proj pZero

instance SceneModel Model where
        calcCameraMatrix size model = cameraMatrix (genCamera size) (aspectRatio size)

makeScene resPath = do
        makeSceneOperator (Model pZero [])
                          (loadResources resPath)
                          (\ri ie del m -> (m, []))
                          (\re ri mdl -> return ()) 
                          worldLabel

main :: IO ()
main = do
        operator <- makeScene "resources"
         
        glfwMain (Size 480 640)
            [operator]
            operator
            id
