{-# LANGUAGE MultiParamTypeClasses #-}

module Engine.Event where

import Engine.World
import Math.Vector
import Control.Monad.State

data RPC = RPC {
         inputTouchUp :: [V2 -> Int -> SysMonad IO ()]
         }

emptyRPC = RPC { inputTouchUp = [] }

type SysMonad m r = StateT (World, RPC, EventStore) m r

data Event = PrintAll 
           | SpawnEnt V3
           | Error String
           {-| InputTouchDown V2 Int-}
           {-| InputTouchUp V2 Int-}
           | InputTouchLoc V2 Int
           | Quit

type EventStore = [Event]

mergeEventStores :: EventStore -> EventStore -> EventStore
mergeEventStores a b = a ++ b

addToEventStore :: EventStore -> Event -> EventStore
addToEventStore es e = e : es

emptyEventStore :: [Event]
emptyEventStore = []
