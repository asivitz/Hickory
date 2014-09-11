{-# LANGUAGE NamedFieldPuns #-}
 
import Engine.Run
import Engine.Input
import qualified Freecell.GameScene as GameScene

{-import qualified Freecell.Systems.Game as FCGame-}
{-import qualified Freecell.Systems.Menu as FCMenu-}
{-import qualified Freecell.Systems.Draw as FCDraw-}


main :: IO ()
main = do
                          
        scenes <- sequence [GameScene.makeScene]
        glfwMain scenes
                 GameScene.RawEvent
