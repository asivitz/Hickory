{-# LANGUAGE NamedFieldPuns #-}

module Systems.Simple (empty, make, SysData) where

import Control.Monad.State
import Graphics.GLUtils

import Engine.System
import Engine.Event
import Engine.Component
import Types.Types
import Types.Color
import Utils.Utils
import Math.Vector
import Math.VectorMatrix

import Graphics.DrawText
import Graphics.Drawing
import Menus.Menus
import Menus.Construction
import qualified Systems.Textures as Textures
import qualified Systems.Draw as Draw
import qualified Systems.DrawText as DrawText
import qualified Systems.Menus as Menus

make simple texes dt draw menus = System (run simple dt draw) (handleEv simple draw) (initS simple dt texes draw menus)

starTex = "filled_star.png"

handleEv simple draw event =
        case event of
            PrintAll -> do
                mydata <- getSysData simple
                liftIO $ print mydata
            (SpawnEnt pos) -> spawnEnt pos simple draw
            (Error msg) -> liftIO $ putStrLn ("ERROR: " ++ msg)
            (InputTouchUp pos touchid) -> do
                Draw.SysData { Draw.screenSize, Draw.worldMatrix } <- getSysData draw
                let pos' = lerpUnproject pos (-5) worldMatrix (viewportFromSize screenSize)
                liftIO $ print pos'
                broadcast $ SpawnEnt pos'
            (InputTouchLoc pos touchid) -> liftIO $ print $ "Input ping " ++ (show pos)
            _ -> return ()

spawnEnt pos simple draw = do
   sd <- getSysData simple
   case sd of
       SysData (Just tid) _ (Just vanilla) -> do
           liftIO $ print "Spawning ent"
           e <- spawnEntity
           addComp e $ DrawState pos
           addComp e $ NewtonianMover (v3 1 0 0) (v3 0 0 0)
           addComp e $ Drawable $ Square (Size 1 1) (rgb 0.3 0.6 0.8) tid vanilla
       _ -> return ()

run simple dt draw delta =
      do
          {-SysData { printer } <- getSysData simple-}
          {-Draw.SysData { Draw.screenSize } <- getSysData draw-}
          return ()

        {-
            whenMaybe starConfig $ \config ->
                liftIO $ drawPoints config [170,240, 300, 20] (10.0 :: Double)
            
            whenMaybe bgtex $ \tex ->
                whenMaybe vanillaShader $ \s -> do
                    dc <- liftIO $ addDrawCommand model white white tex s worldLabel 0.0 False
                    liftIO $ setTCCommand dc (fromList [0, 0, 0.1, 1.0]) 
                    -}

font = "goudy_bookletter_1911"

initS simple dt texes draw menus = do
        tid <- Textures.reserveTex texes starTex
        {-bgtid <- Textures.reserveTex texes "bg.png"-}

        {-
        vaoConfig <- case particles of
                         Nothing -> return Nothing
                         Just p -> liftIO $ createVAOConfig p [(VertexGroup [(Attachment sp_ATTR_POSITION 2)])] >>= return . Just
                         -}

        pid <- DrawText.reservePrinter dt texes font
        nilla <- Draw.reserveShader draw ("Shader.vsh", "Shader.fsh")
        putSysData simple SysData { texid = tid, printer = pid, vanilla = nilla }
        Menus.pushScreen menus draw texes dt mainMenu

data SysData = SysData { texid :: Maybe TexID, printer :: Maybe Int, vanilla :: Maybe Shader } deriving (Show)

empty = SysData Nothing Nothing Nothing

mainMenu :: MenuScreen Scalar
mainMenu = MenuScreen [simpleMenuButton 0 "Start" (PushScreen subMenu),
                      simpleMenuButton 1 "Next" RefreshScreen,
                      simpleMenuButton 2 "Then" RefreshScreen
                      ]
                      0.5

subMenu = MenuScreen [simpleMenuButton 0 "Back" PopScreen] 0.5

simpleMenuButton :: Int -> String -> ScreenAction Scalar -> UIElement Scalar
simpleMenuButton idx txt action = UIElement (Just (Button (RRect (center 0, beg 40) (end 40, beg 30)) (Nothing, Just action))) $ 
    MenuRenderSpec ([], [font], []) $ \(MenuResources _ [pid] _) ->
        \fraction incoming ->
            let frac' = constrainInterval fraction idx in
            [(beg (40 * (realToFrac (1 + idx))), center 0, 
                TextMenuDrawCommand pid DrawText.textcommand { text = txt, fontSize = 6, color = rgba 1 1 1 frac' })]
