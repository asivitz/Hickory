{-# LANGUAGE NamedFieldPuns #-}

module Freecell.Systems.Draw (make) where

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
import Freecell.Context.Game
import Engine.GameContext
import Math.Vector
import Utils.System
import qualified Systems.Draw as Draw

make :: SysMonad EXGameContext IO (System EXGameContext)
make = do
        draw <- liftIO $ newIORef empty
        initS draw
        return $ System (run draw)

cardTex = "card.png"

drawCard ::  TexID -> Shader -> DrawState -> SysMonad c IO DrawState
drawCard texid shader ds@(DrawState pos) = do
        let spec = Square (Size 2 2) white texid shader
        liftIO $ Draw.drawSpec pos worldLabel spec
        return ds

run draw delta =
      do
          SysData { cardtex, vanilla } <- getSysData draw
          whenMaybe2 cardtex vanilla $ \tid sh ->
            updateCompsM (drawCard tid sh) drawStates
          return ()

initS draw = do
        liftIO $ glClearColor 0 0.5 0 1

        RPC { _reserveTex, _reservePrinter, _reserveShader } <- getRPC

        tid <- _reserveTex cardTex
        nilla <- _reserveShader ("Shader.vsh", "Shader.fsh")

        putSysData draw SysData { cardtex = tid, vanilla = nilla }

data SysData = SysData { cardtex :: Maybe TexID, vanilla :: Maybe Shader } deriving (Show)

empty = SysData Nothing Nothing

