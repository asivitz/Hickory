{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.World where

import Engine.Entity
import Engine.Component
import Math.Vector
import Control.Monad.State
import Graphics.GLUtils
import Graphics.DrawText
import Control.Lens

data World c = World {
           entitySet :: EntitySet,
           systemContext :: SystemContext c,
           gameContext :: c

           } deriving (Show)

type SysMonad c m r = StateT (World c) m r

data RPC c = RPC {
         _inputTouchUp :: [V2 -> Int -> SysMonad c IO ()],
         _inputTouchDown :: [V2 -> Int -> SysMonad c IO ()],
         _inputTouchLoc :: [V2 -> Int -> SysMonad c IO ()],
         _reserveTex :: String -> SysMonad c IO (Maybe TexID),
         _reservePrinter :: String -> SysMonad c IO (Maybe PrinterID),
         _printAll :: [SysMonad c IO ()],
         _quit :: [SysMonad c IO ()]
         }

instance Show (RPC c) where
        show rpc = "RPC"

emptyRPC = RPC { 
               _inputTouchUp = [],
               _inputTouchDown = [],
               _inputTouchLoc = [],
               _reserveTex = \_ -> return Nothing,
               _reservePrinter = \_ -> return Nothing,
               _printAll = [],
               _quit = []
               }

data SystemContext c = SystemContext ComponentStore (RPC c) deriving (Show)

emptySystemContext = SystemContext emptyComponentStore emptyRPC


emptyWorld :: c -> World c
emptyWorld gc = World { entitySet = newEntitySet,
                   systemContext = emptySystemContext,
                   gameContext = gc
                 }

addNewEntity :: World c -> (Entity, World c)
addNewEntity w = let es = entitySet w
                     (ent, new_es) = genEntity es
                     in (ent, w { entitySet = new_es })


makeLenses ''RPC
