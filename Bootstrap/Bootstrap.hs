module Bootstrap.Bootstrap where

import Engine.World
import Engine.System
import qualified Systems.FPSCounter as FPSCounter
import qualified Systems.DrawState as DrawState
import qualified Systems.Textures as Textures
import qualified Systems.DrawText as DrawText
import qualified Systems.WorldCamera as WorldCamera
import qualified Systems.UICamera as UICamera
import qualified Systems.Menus as Menus
import qualified Systems.GLFWPlatform as GLFWPlatform

coreSystems :: Show c => SysMonad c IO [System c]
coreSystems = do
        drawstate <- DrawState.make
        fps <- FPSCounter.make
        textures <- Textures.make
        drawtext <- DrawText.make
        worldcamera <- WorldCamera.make
        uicamera <- UICamera.make
        menus <- Menus.make
        return [fps, drawstate, textures, menus, drawtext, worldcamera, uicamera]
