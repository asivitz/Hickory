{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.World where

import Engine.Entity
import Engine.Component
import Math.Vector
import Control.Monad.State
import Graphics.GLUtils
import Graphics.DrawText
import Graphics.Drawing
import Menus.Menus
import Control.Lens hiding (Context)

data World c = World {
           entitySet :: EntitySet,
           systemContext :: SystemContext c,
           gameContext :: c

           } deriving (Show)

type SysMonad c m r = StateT (World c) m r

data RPC c = RPC {
           _inputTouchUp :: [V2 -> Int -> SysMonad c IO Bool],
           _inputTouchDown :: [V2 -> Int -> SysMonad c IO Bool],
           _inputTouchLoc :: [V2 -> Int -> SysMonad c IO Bool],
           _reserveTex :: String -> SysMonad c IO (Maybe TexID),
           _releaseTex :: String -> SysMonad c IO (),
           _reservePrinter :: String -> SysMonad c IO (Maybe PrinterID),
           _reserveShader :: (String, String) -> SysMonad c IO (Maybe Shader),
           _pushMenuScreen :: MenuScreen Scalar (SysMonad c IO ()) -> SysMonad c IO (),
           _drawText :: PrinterID -> Label -> PositionedTextCommand -> SysMonad c IO (),
           _printAll :: [SysMonad c IO ()],
           _running :: IO Bool
           }

instance Show (RPC c) where
        show rpc = "RPC"

emptyRPC = RPC { 
               _inputTouchUp = [],
               _inputTouchDown = [],
               _inputTouchLoc = [],
               _reserveTex = \_ -> return Nothing,
               _releaseTex = \_ -> return (),
               _reservePrinter = \_ -> return Nothing,
               _reserveShader = \_ -> return Nothing,
               _pushMenuScreen = \_ -> return (),
               _drawText = \_ _ _ -> return (),
               _printAll = [],
               _running = return True
               }

data Context compStore rpc = Context compStore rpc

instance Show c => Show (Context c r) where
        show (Context c r) = "Context -- CompStore: " ++ (show c)

type SystemContext c = Context ComponentStore (RPC c)

emptySystemContext = Context emptyComponentStore emptyRPC


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
