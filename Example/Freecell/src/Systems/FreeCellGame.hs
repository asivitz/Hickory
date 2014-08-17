{-# LANGUAGE NamedFieldPuns #-}

module Systems.FreeCellGame (make) where

import Control.Monad.State
import Graphics.GLUtils

import Engine.System
import Engine.World

import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Data.IORef

make :: SysMonad c IO (System c)
make = do
        fcgame <- liftIO $ newIORef empty
        initS fcgame
        return $ System (run fcgame)

starTex = "filled_star.png"

inputTouchUp' fcgame pos touchid = do
        return False

inputTouchLoc' pos touchid = do
        liftIO $ print $ "Input ping " ++ (show pos)
        return False

printAll' fcgame = do
        mydata <- getSysData fcgame
        liftIO $ print mydata

run fcgame delta =
      do
          return ()

initS fcgame = do
        liftIO $ glClearColor 0 0.5 0 1
        registerEvent printAll (printAll' fcgame)
        registerEvent inputTouchUp (inputTouchUp' fcgame)
        registerEvent inputTouchLoc (inputTouchLoc')

        RPC { _reserveTex, _reservePrinter, _reserveShader } <- getRPC

        tid <- _reserveTex starTex
        nilla <- _reserveShader ("Shader.vsh", "Shader.fsh")

        putSysData fcgame SysData { texid = tid, vanilla = nilla }

data SysData = SysData { texid :: Maybe TexID, vanilla :: Maybe Shader } deriving (Show)

empty = SysData Nothing Nothing
