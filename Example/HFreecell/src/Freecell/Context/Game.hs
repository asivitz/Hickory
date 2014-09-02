{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Freecell.Context.Game where

import Engine.World
import Engine.Component
import Data.HashMap.Strict as HashMap
import Control.Lens hiding (Context)
import Freecell.Utils
import FreeCell

data GameComponentStore = GameComponentStore { 
                    _cards :: CompMap UICard
                    } deriving (Show)

makeLenses ''GameComponentStore

emptyGameComponentStore = GameComponentStore { 
                                     _cards = empty
                                     }

type EXGameContext = Context GameComponentStore GameRPC

data GameRPC = GameRPC {
         _test :: [SysMonad EXGameContext IO ()],
         _newGame :: [SysMonad EXGameContext IO ()],
         _lostGame :: [SysMonad EXGameContext IO ()],
         _wonGame :: [SysMonad EXGameContext IO ()],
         _getGame :: SysMonad EXGameContext IO (Maybe Board)
         }

makeLenses ''GameRPC

emptyGameRPC = GameRPC { 
               _test = [],
               _newGame = [],
               _lostGame = [],
               _wonGame = [],
               _getGame = return Nothing
               }

emptyGameContext = Context emptyGameComponentStore emptyGameRPC
