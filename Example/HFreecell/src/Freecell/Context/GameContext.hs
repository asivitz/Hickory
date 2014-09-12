{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Freecell.Context.GameContext where

import Engine.World
import Engine.Component.Component
import Data.HashMap.Strict as HashMap
import Control.Lens hiding (Context)
import Freecell.Utils
import FreeCell

data GameComponentStore = GameComponentStore { 
                    _cards :: CompMap UICard
                    } deriving (Show)

emptyGameComponentStore = GameComponentStore { 
                                     _cards = empty
                                     }

type GameContext = Context GameComponentStore GameRSC

data GameRSC = GameRSC {
         _test :: [SysMonad GameContext IO ()],
         _newGame :: [SysMonad GameContext IO ()],
         _lostGame :: [SysMonad GameContext IO ()],
         _wonGame :: [SysMonad GameContext IO ()],
         _getGame :: SysMonad GameContext IO (Maybe Board)
         }

emptyGameRSC = GameRSC { 
               _test = [],
               _newGame = [],
               _lostGame = [],
               _wonGame = [],
               _getGame = return Nothing
               }

emptyGameContext = Context emptyGameComponentStore emptyGameRSC

makeLenses ''GameComponentStore
makeLenses ''GameRSC

