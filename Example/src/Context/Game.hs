{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Context.Game where

import Engine.World
import Control.Lens hiding (Context)

type GameComponentStore = [Int]

emptyGameComponentStore = []

type EXGameContext = Context GameComponentStore GameRPC

data GameRPC = GameRPC {
         _test :: [SysMonad EXGameContext IO ()]
         }

makeLenses ''GameRPC

emptyGameRPC = GameRPC { 
               _test = []
               }

emptyGameContext = Context emptyGameComponentStore emptyGameRPC
