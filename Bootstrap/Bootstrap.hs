module Bootstrap.Bootstrap where

import Data.IORef

import qualified Systems.FPSCounter as FPSCounter
import qualified Systems.DrawState as DrawState
import qualified Systems.Draw as Draw
import qualified Systems.Textures as Textures
import qualified Systems.Input as Input
import qualified Systems.DrawText as DrawText
import qualified Systems.WorldCamera as WorldCamera
import qualified Systems.UICamera as UICamera
import qualified Systems.Menus as Menus

coreData = do
        draw <- Draw.makeDrawData
        fps <- newIORef FPSCounter.empty
        textures <- newIORef Textures.empty
        iosys <- Input.makeIO draw
        drawtext <- newIORef DrawText.empty
        worldcamera <- newIORef WorldCamera.empty
        uicamera <- newIORef UICamera.empty
        menus <- newIORef Menus.empty

        let systems = [
                    FPSCounter.make fps,
                    DrawState.make,
                    Textures.make textures,
                    DrawText.make drawtext draw,
                    Menus.make menus draw drawtext,
                    WorldCamera.make worldcamera,
                    UICamera.make uicamera,
                    Draw.make draw worldcamera uicamera,
                    iosys
                    ]

        return ((draw, textures, drawtext, worldcamera, uicamera, menus), systems)
