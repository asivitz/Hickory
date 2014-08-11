{-# LANGUAGE MultiParamTypeClasses #-}

module Engine.Event where

import Engine.World
import Math.Vector
import Control.Monad.State
import Graphics.GLUtils
import Graphics.DrawText

data RPC = RPC {
         inputTouchUp :: [V2 -> Int -> SysMonad IO ()],
         inputTouchDown :: [V2 -> Int -> SysMonad IO ()],
         inputTouchLoc :: [V2 -> Int -> SysMonad IO ()],
         reserveTex :: String -> SysMonad IO (Maybe TexID),
         reservePrinter :: String -> SysMonad IO (Maybe PrinterID),
         printAll :: [SysMonad IO ()],
         quit :: [SysMonad IO ()]
         }

emptyRPC = RPC { inputTouchUp = [],
               inputTouchDown = [],
               inputTouchLoc = [],
               reserveTex = \_ -> return Nothing,
               reservePrinter = \_ -> return Nothing,
               printAll = [],
               quit = []
               }

type SysMonad m r = StateT (World, RPC) m r
