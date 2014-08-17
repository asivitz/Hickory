import Engine.Run
import Bootstrap.Bootstrap
import Context.Game
import qualified Systems.FreeCellGame as FCGame
import qualified Systems.FreeCellMenu as FCMenu
import Engine.World
import Control.Monad.State

initSystems = do
        core <- coreSystems

        fcgame <- FCGame.make
        fcmenu <- FCMenu.make 
        return ([fcgame, fcmenu] ++ core)

main :: IO ()
main = do 
          (systems, w) <- runStateT initSystems (emptyWorld emptyGameContext)
          run w systems
