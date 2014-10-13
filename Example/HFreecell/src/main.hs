{-# LANGUAGE NamedFieldPuns #-}
 
import Engine.Scene.Run
import GLFW.Run
import Engine.Scene.Scene
import Types.Types
import Freecell.Events
import qualified Freecell.GameScene as GameScene
import qualified Freecell.MenuScene as MenuScene

{-import qualified Freecell.Systems.Game as FCGame-}
{-import qualified Freecell.Systems.Menu as FCMenu-}
{-import qualified Freecell.Systems.Draw as FCDraw-}


main :: IO ()
main = do
                          
        operators <- sequence [GameScene.makeScene >>= makeSceneOperator, 
                              MenuScene.makeScene >>= makeSceneOperator]
         
        glfwMain (Size 640 480)
            operators
            (operators !! 1)
            RawEvent
