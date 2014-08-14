{-# LANGUAGE TemplateHaskell #-}

module Engine.Component where

import Engine.Entity
import Data.HashMap.Strict as HashMap
import Math.Vector
import Graphics.Drawing
import Control.Lens

data DrawState = DrawState V3 deriving (Show)
data NewtonianMover = NewtonianMover V3 V3 deriving (Show)
data Drawable = Drawable DrawSpec deriving (Show)

data ComponentStore = ComponentStore { 
                    _drawStates :: HashMap Entity DrawState,
                    _newtonianMovers :: HashMap Entity NewtonianMover,
                    _drawables :: HashMap Entity Drawable
                    } deriving (Show)

makeLenses ''ComponentStore

emptyComponentStore = ComponentStore { 
                                     _drawStates = empty, 
                                     _newtonianMovers = empty, 
                                     _drawables = empty }
