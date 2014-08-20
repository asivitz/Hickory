{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Context.Game where

import Engine.World
import Engine.Component
import Data.HashMap.Strict as HashMap
import Control.Lens hiding (Context)

data Card = Card Int deriving Show

data GameComponentStore = GameComponentStore { 
                    _cards :: CompMap Card
                    } deriving (Show)

makeLenses ''GameComponentStore

emptyGameComponentStore = GameComponentStore { 
                                     _cards = empty
                                     }

type EXGameContext = Context GameComponentStore GameRPC

data GameRPC = GameRPC {
         _test :: [SysMonad EXGameContext IO ()],
         _newGame :: [SysMonad EXGameContext IO ()]
         }

makeLenses ''GameRPC

emptyGameRPC = GameRPC { 
               _test = [],
               _newGame = []
               }

emptyGameContext = Context emptyGameComponentStore emptyGameRPC
