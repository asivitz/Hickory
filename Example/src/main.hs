import Data.IORef

import Engine.Run
import qualified Systems.Simple as Simple
import qualified Systems.FPSCounter as FPSCounter
import qualified Systems.DrawState as DrawState
import qualified Systems.Draw as Draw
import qualified Systems.Textures as Textures
import qualified Systems.Input as Input
import qualified Systems.DrawText as DrawText

main :: IO ()
main = do 
        draw <- Draw.makeDrawData
        fps <- newIORef FPSCounter.empty
        simple <- newIORef Simple.empty
        {-ds <- newIORef DrawState.empty-}
        textures <- newIORef Textures.empty
        iosys <- Input.makeIO draw
        drawtext <- newIORef DrawText.empty

        let systems = [
                    FPSCounter.make fps,
                    DrawState.make,
                    Textures.make textures,
                    DrawText.make drawtext draw,
                    Simple.make simple fps textures drawtext draw,
                    Draw.make draw textures,
                    iosys
                    ]

        run systems
