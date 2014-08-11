{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.Event where

import Engine.World
import Math.Vector
import Control.Monad.State
import Graphics.GLUtils
import Graphics.DrawText
import Control.Lens

type SysMonad m r = StateT (World, RPC) m r

data RPC = RPC {
         _inputTouchUp :: [V2 -> Int -> SysMonad IO ()],
         _inputTouchDown :: [V2 -> Int -> SysMonad IO ()],
         _inputTouchLoc :: [V2 -> Int -> SysMonad IO ()],
         _reserveTex :: String -> SysMonad IO (Maybe TexID),
         _reservePrinter :: String -> SysMonad IO (Maybe PrinterID),
         _printAll :: [SysMonad IO ()],
         _quit :: [SysMonad IO ()]
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
