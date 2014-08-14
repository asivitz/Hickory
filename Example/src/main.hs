import Data.IORef

import Engine.Run
import Bootstrap.Bootstrap
import Context.Game
import qualified Systems.Simple as Simple

main :: IO ()
main = do 
          ((platform, draw, textures, drawtext, worldcamera, uicamera, menus), coreSystems) <- coreData
          simple <- newIORef Simple.empty

          let simpleSys = Simple.make simple textures drawtext draw menus
              systems = (coreSystems ++ [simpleSys])
          run systems emptyGameContext platform
