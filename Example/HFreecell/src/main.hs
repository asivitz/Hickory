import Engine.Run
import Bootstrap.Bootstrap
import Freecell.Context.GameContext
import qualified Freecell.Systems.Game as FCGame
import qualified Freecell.Systems.Menu as FCMenu
import qualified Freecell.Systems.Draw as FCDraw
import Engine.World
import Engine.System

initSystems = do
        core <- coreSystems
        game <- sequence [
                         FCGame.make, 
                         FCMenu.make, 
                         FCDraw.make
                         ]

        return $ game ++ core

resources = "Example/HFreecell/resources/"

main :: IO ()
main = do 
        let w = registerResourceToWorld sysCon (emptyWorld emptyGameContext) resourcesPath (return resources)

        initAndRun w initSystems
