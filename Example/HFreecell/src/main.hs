import Engine.Run
import Bootstrap.Bootstrap
import Freecell.Context.Game
import qualified Freecell.Systems.Game as FCGame
import qualified Freecell.Systems.Menu as FCMenu
import qualified Freecell.Systems.Draw as FCDraw
import Engine.World
import Control.Monad.State
import Engine.System

initSystems = do
        core <- coreSystems

        fcgame <- FCGame.make
        fcmenu <- FCMenu.make 
        fcdraw <- FCDraw.make 
        return ([fcgame, fcmenu, fcdraw] ++ core)

resources = "Example/HFreecell/resources/"

main :: IO ()
main = do 
          let w = registerResourceToWorld sysCon (emptyWorld emptyGameContext) resourcesPath (return resources)
              
          (systems, w') <- runStateT initSystems w
          run w' systems
