module Engine.Component where

import Engine.Entity
import Data.HashMap.Strict as HashMap
import Types.Types
import Math.Vector
import Types.Color
import Graphics.GLUtils
import Graphics.Drawing

class Show c => Component c where
      getComponents :: ComponentStore -> HashMap Entity c
      updateComponents :: ComponentStore -> HashMap Entity c -> ComponentStore
      addComponent :: ComponentStore -> Entity -> c -> ComponentStore
      addComponent store ent comp = updateComponents store $ HashMap.insert ent comp (getComponents store)

data DrawState = DrawState V3 deriving (Show)
data NewtonianMover = NewtonianMover V3 V3 deriving (Show)
data Drawable = Square FSize Color TexID Shader deriving (Show)

instance Component DrawState where
      getComponents store = drawStates store
      updateComponents store comps = store { drawStates = comps }

instance Component NewtonianMover where
      getComponents store = newtonianMovers store
      updateComponents store comps = store { newtonianMovers = comps }

instance Component Drawable where
      getComponents store = drawables store
      updateComponents store comps = store { drawables = comps }

data ComponentStore = ComponentStore { 
                    drawStates :: HashMap Entity DrawState,
                    newtonianMovers :: HashMap Entity NewtonianMover,
                    drawables :: HashMap Entity Drawable
                    } deriving (Show)

emptyComponentStore = ComponentStore { 
                                     drawStates = empty, 
                                     newtonianMovers = empty, 
                                     drawables = empty }
