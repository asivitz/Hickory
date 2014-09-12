{-# LANGUAGE TemplateHaskell #-}

module Freecell.Component where

import Engine.Component.Component
import FreeCell
import Control.Lens
import Data.HashMap.Strict

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
               _gameBoard :: Board
               }

