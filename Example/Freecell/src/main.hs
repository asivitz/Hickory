import Engine.Run
import Bootstrap.Bootstrap
import Freecell.Context.Game
import qualified Freecell.Systems.Game as FCGame
import qualified Freecell.Systems.Menu as FCMenu
import Engine.World
import Control.Monad.State
import Engine.System

initSystems = do
        core <- coreSystems

        fcgame <- FCGame.make
        fcmenu <- FCMenu.make 
        return ([fcgame, fcmenu] ++ core)

resources = "Example/Freecell/resources/"

main :: IO ()
main = do 
          let w = registerResourceToWorld (emptyWorld emptyGameContext) resourcesPath (return resources)
              
          (systems, w') <- runStateT initSystems w
          run w' systems
