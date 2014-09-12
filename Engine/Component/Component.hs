{-# LANGUAGE TemplateHaskell #-}

module Engine.Component.Component where

import Engine.Component.Entity
import Data.HashMap.Strict as HashMap
import Math.Vector
import Graphics.Drawing
import Control.Lens
import Types.Types

data DrawState = DrawState V3 deriving (Show)
data NewtonianMover = NewtonianMover V3 V3 deriving (Show)
data Drawable = Drawable DrawSpec deriving (Show)
data Selectable = Selectable (Size Scalar) deriving Show
data MouseDrag = MouseDrag V3 deriving Show

type CompMap c = HashMap Entity c

data ComponentStore = ComponentStore { 
                    _drawStates :: CompMap DrawState,
                    _newtonianMovers :: CompMap NewtonianMover,
                    _drawables :: CompMap Drawable,
                    _selectables :: CompMap Selectable,
                    _mouseDrags :: CompMap MouseDrag
                    } deriving (Show)

makeLenses ''ComponentStore

emptyComponentStore = ComponentStore { 
                                     _drawStates = empty, 
                                     _newtonianMovers = empty, 
                                     _drawables = empty,
                                     _selectables = empty,
                                     _mouseDrags = empty
                                     }
