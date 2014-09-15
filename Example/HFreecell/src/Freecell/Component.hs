{-# LANGUAGE TemplateHaskell #-}

module Freecell.Component where

import Engine.Component.Component
import FreeCell
import Control.Lens
import Data.HashMap.Strict
import Engine.Component.Model

data ComponentStore = ComponentStore { 
                    _drawStates :: CompMap DrawState,
                    _newtonianMovers :: CompMap NewtonianMover,
                    _drawables :: CompMap Drawable,
                    _selectables :: CompMap Selectable,
                    _mouseDrags :: CompMap MouseDrag,
                    _cardComps :: CompMap Card
                    } deriving (Show)

makeLenses ''ComponentStore

emptyComponentStore = ComponentStore { 
                                     _drawStates = empty, 
                                     _newtonianMovers = empty, 
                                     _drawables = empty,
                                     _selectables = empty,
                                     _mouseDrags = empty,
                                     _cardComps = empty
                                     }

data GameModel = GameModel {
               _curBoard :: Board
               }

makeLenses ''GameModel

getBoard model = view (game . curBoard) model
setBoard model board = set (game. curBoard) board model

