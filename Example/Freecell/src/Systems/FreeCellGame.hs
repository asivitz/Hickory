{-# LANGUAGE NamedFieldPuns #-}

module Systems.FreeCellGame (empty, make, SysData) where

import Control.Monad.State
import Graphics.GLUtils

import Engine.System
import Engine.World

import Graphics.Drawing
import qualified Systems.Draw as Draw
import Graphics.Rendering.OpenGL.Raw.Core31

make fcgame draw = System (run fcgame draw) (initS fcgame draw)

starTex = "filled_star.png"

inputTouchUp' fcgame draw pos touchid = do
        return False

inputTouchLoc' pos touchid = do
        liftIO $ print $ "Input ping " ++ (show pos)
        return False

printAll' fcgame = do
        mydata <- getSysData fcgame
        liftIO $ print mydata

run fcgame draw delta =
      do
          return ()

initS fcgame draw = do
        liftIO $ glClearColor 0 0.5 0 1
        registerEvent printAll (printAll' fcgame)
        registerEvent inputTouchUp (inputTouchUp' fcgame draw)
        registerEvent inputTouchLoc (inputTouchLoc')

        RPC { _reserveTex, _reservePrinter } <- getRPC

        tid <- _reserveTex starTex
        nilla <- Draw.reserveShader draw ("Shader.vsh", "Shader.fsh")

        putSysData fcgame SysData { texid = tid, vanilla = nilla }

data SysData = SysData { texid :: Maybe TexID, vanilla :: Maybe Shader } deriving (Show)

empty = SysData Nothing Nothing
