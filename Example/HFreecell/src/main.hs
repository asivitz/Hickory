import Engine.Run
import Bootstrap.Bootstrap
import Freecell.Context.GameContext
import qualified Freecell.Systems.Game as FCGame
import qualified Freecell.Systems.Menu as FCMenu
import qualified Freecell.Systems.Draw as FCDraw

initSystems = do
        core <- coreSystems
        game <- sequence [
                         FCGame.make, 
                         FCMenu.make, 
                         FCDraw.make
                         ]

        return $ game ++ core

main :: IO ()
main = do 
        let w = newWorldWithResourcesPath emptyGameContext "Example/HFreecell/resources/"

        initAndRun w initSystems
