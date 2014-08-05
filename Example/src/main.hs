import Data.IORef

import Engine.Run
import Bootstrap.Bootstrap
import qualified Systems.Simple as Simple

main :: IO ()
main = do 
          ((draw, textures, drawtext, worldcamera, uicamera), coreSystems) <- coreData
          simple <- newIORef Simple.empty

          let simpleSys = Simple.make simple textures drawtext draw
              systems = (coreSystems ++ [simpleSys])
          run systems
