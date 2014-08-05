{-# LANGUAGE MultiParamTypeClasses #-}
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

import Graphics.DrawText
import Graphics.Drawing
import qualified Systems.Textures as Textures
import qualified Systems.Draw as Draw
import qualified Systems.DrawText as DrawText

make simple texes dt draw = System (run simple dt draw) (handleEv simple draw) (initS simple dt texes draw)

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
           addComp e $ Square (Size 1 1) (rgb 0.3 0.6 0.8) tid vanilla
       _ -> return ()

run simple dt draw delta =
      do
          SysData { printer } <- getSysData simple
          Draw.SysData { Draw.screenSize } <- getSysData draw
          whenMaybe printer $ \p ->
            DrawText.drawText dt p uiLabel DrawText.textcommand { text = "Hello World!", fontSize = 6, pos = (screenPos screenSize SMiddle SCenter), color = white }
          return ()

        {-
            whenMaybe starConfig $ \config ->
                liftIO $ drawPoints config [170,240, 300, 20] (10.0 :: Double)
            
            whenMaybe bgtex $ \tex ->
                whenMaybe vanillaShader $ \s -> do
                    dc <- liftIO $ addDrawCommand model white white tex s worldLabel 0.0 False
                    liftIO $ setTCCommand dc (fromList [0, 0, 0.1, 1.0]) 
                    -}


initS simple dt texes draw = do
        tid <- Textures.reserveTex texes starTex
        {-bgtid <- Textures.reserveTex texes "bg.png"-}

        {-
        vaoConfig <- case particles of
                         Nothing -> return Nothing
                         Just p -> liftIO $ createVAOConfig p [(VertexGroup [(Attachment sp_ATTR_POSITION 2)])] >>= return . Just
                         -}

        pid <- DrawText.reservePrinter dt texes "goudy_bookletter_1911"
        nilla <- Draw.reserveShader draw ("Shader.vsh", "Shader.fsh")
        putSysData simple SysData { texid = tid, printer = pid, vanilla = nilla }

data SysData = SysData { texid :: Maybe TexID, printer :: Maybe Int, vanilla :: Maybe Shader } deriving (Show)

empty = SysData Nothing Nothing Nothing
