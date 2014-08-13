{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.Event where

import Engine.World
import Engine.Component
import Math.Vector
import Control.Monad.State
import Graphics.GLUtils
import Graphics.DrawText
import Control.Lens

data SystemContext c = SystemContext ComponentStore (RPC c) c

type SysMonad c m r = StateT (World, SystemContext c) m r

data RPC c = RPC {
         _inputTouchUp :: [V2 -> Int -> SysMonad c IO ()],
         _inputTouchDown :: [V2 -> Int -> SysMonad c IO ()],
         _inputTouchLoc :: [V2 -> Int -> SysMonad c IO ()],
         _reserveTex :: String -> SysMonad c IO (Maybe TexID),
         _reservePrinter :: String -> SysMonad c IO (Maybe PrinterID),
         _printAll :: [SysMonad c IO ()],
         _quit :: [SysMonad c IO ()]
         }

makeLenses ''RPC

emptyRPC = RPC { 
               _inputTouchUp = [],
               _inputTouchDown = [],
               _inputTouchLoc = [],
               _reserveTex = \_ -> return Nothing,
               _reservePrinter = \_ -> return Nothing,
               _printAll = [],
               _quit = []
               }


emptySystemContext uc = SystemContext emptyComponentStore emptyRPC uc
