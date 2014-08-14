{-# LANGUAGE TemplateHaskell #-}

module Engine.Component where

import Engine.Entity
import Data.HashMap.Strict as HashMap
import Math.Vector
import Graphics.Drawing
import Control.Lens

class Show c => Component c where
      getComponents :: ComponentStore -> HashMap Entity c
      updateComponents :: ComponentStore -> HashMap Entity c -> ComponentStore
      addComponent :: ComponentStore -> Entity -> c -> ComponentStore
      addComponent store ent comp = updateComponents store $ HashMap.insert ent comp (getComponents store)

data DrawState = DrawState V3 deriving (Show)
data NewtonianMover = NewtonianMover V3 V3 deriving (Show)
data Drawable = Drawable DrawSpec deriving (Show)

instance Component DrawState where
      getComponents store = _drawStates store
      updateComponents store comps = store { _drawStates = comps }

instance Component NewtonianMover where
      getComponents store = _newtonianMovers store
      updateComponents store comps = store { _newtonianMovers = comps }

instance Component Drawable where
      getComponents store = _drawables store
      updateComponents store comps = store { _drawables = comps }

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
