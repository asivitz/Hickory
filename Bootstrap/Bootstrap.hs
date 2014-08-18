module Bootstrap.Bootstrap where

import qualified Systems.FPSCounter as FPSCounter
import qualified Systems.DrawState as DrawState
import qualified Systems.Draw as Draw
import qualified Systems.Textures as Textures
import qualified Systems.DrawText as DrawText
import qualified Systems.WorldCamera as WorldCamera
import qualified Systems.UICamera as UICamera
import qualified Systems.Menus as Menus
import qualified Systems.GLFWPlatform as GLFWPlatform

coreSystems = do
        (platform_pre, platform_post) <- GLFWPlatform.make
        drawstate <- DrawState.make
        draw <- Draw.make
        fps <- FPSCounter.make
        textures <- Textures.make
        drawtext <- DrawText.make
        worldcamera <- WorldCamera.make
        uicamera <- UICamera.make
        menus <- Menus.make
        return [platform_pre, fps, drawstate, textures, menus, drawtext, worldcamera, uicamera, draw, platform_post]
