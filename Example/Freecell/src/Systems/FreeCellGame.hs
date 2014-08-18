{-# LANGUAGE NamedFieldPuns #-}

module Systems.FreeCellGame (make) where

import Control.Monad.State
import Graphics.GLUtils

import Engine.System
import Engine.World
import Engine.Component

import Types.Types
import Types.Color
import Utils.Utils
import Utils.Projection

import Graphics.Drawing
import Graphics.Rendering.OpenGL.Raw.Core31
import Data.IORef
import Context.Game
import Engine.GameContext
import Math.Vector

make :: SysMonad EXGameContext IO (System EXGameContext)
make = do
        fcgame <- liftIO $ newIORef empty
        initS fcgame
        return $ System (run fcgame)

cardTex = "card.png"

pickSelectable pos (e, (DrawState p), (Selectable size)) =
        posInRect (v3tov2 pos) (Rect (v3tov2 p) size)

inputTouchUp' fcgame pos touchid = do
        unproj <- doLerpUnproject pos (-5)

        comps <- zipComps2 drawStates selectables

        let selected = filter (pickSelectable unproj) comps

        liftIO $ print selected
        return False

inputTouchLoc' pos touchid = do
        liftIO $ print $ "Input ping " ++ (show pos)
        return False

printAll' fcgame = do
        mydata <- getSysData fcgame
        liftIO $ print mydata

newGame' fcgame = do
        liftIO $ print "New Game"

        spawnCard fcgame (v3 0 0 (-5))

spawnCard fcgame pos = do
        SysData { cardtex, vanilla } <- getSysData fcgame

        let scale = 2

        e <- spawnEntity
        addComp e drawStates $ DrawState pos
        addComp e selectables $ Selectable (Size (0.726 * scale) scale)
        whenMaybe2 cardtex vanilla $ \tid sh ->
            addComp e drawables $ Drawable $ Square (convertSize $ Size scale scale) white tid sh

run fcgame delta =
      do
          return ()

initS fcgame = do
        liftIO $ glClearColor 0 0.5 0 1
        registerEvent printAll (printAll' fcgame)
        registerEvent inputTouchUp (inputTouchUp' fcgame)
        registerEvent inputTouchLoc (inputTouchLoc')
        registerGameEvent newGame (newGame' fcgame)

        RPC { _reserveTex, _reservePrinter, _reserveShader } <- getRPC

        tid <- _reserveTex cardTex
        nilla <- _reserveShader ("Shader.vsh", "Shader.fsh")

        putSysData fcgame SysData { cardtex = tid, vanilla = nilla }

data SysData = SysData { cardtex :: Maybe TexID, vanilla :: Maybe Shader } deriving (Show)

empty = SysData Nothing Nothing
