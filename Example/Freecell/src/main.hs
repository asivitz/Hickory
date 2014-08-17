import Data.IORef

import Engine.Run
import Bootstrap.Bootstrap
import Context.Game
import qualified Systems.FreeCellGame as FCGame
import qualified Systems.FreeCellMenu as FCMenu

main :: IO ()
main = do 
          ((platform, draw, textures, drawtext, worldcamera, uicamera, menus), coreSystems) <- coreData
          fcgame <- newIORef FCGame.empty

          let fcgameSys = FCGame.make fcgame
              fcmenuSys = FCMenu.make 
              systems = (coreSystems ++ [fcgameSys, fcmenuSys])
          run systems emptyGameContext
