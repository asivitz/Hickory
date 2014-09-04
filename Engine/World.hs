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
import Types.Types
import Camera.Camera
import Math.Matrix
import Platform.IPhone
import qualified Graphics.UI.GLFW as GLFW

data World c = World {
           _entitySet :: EntitySet,
           _systemContext :: SystemContext c,
           _gameContext :: c
           } deriving (Show)

type SysMonad c m r = StateT (World c) m r

data RSC c = RSC {
           _resourcesPath :: IO String,
           _inputTouchUp :: [V2 -> Int -> SysMonad c IO Bool],
           _inputTouchDown :: [V2 -> Int -> SysMonad c IO Bool],
           _inputTouchLoc :: [V2 -> Int -> SysMonad c IO Bool],
           _inputKeyUp :: [GLFW.Key -> SysMonad c IO Bool],
           _inputKeyDown :: [GLFW.Key -> SysMonad c IO Bool],
           _reserveTex :: String -> SysMonad c IO (Maybe TexID),
           _releaseTex :: String -> SysMonad c IO (),
           _reservePrinter :: String -> SysMonad c IO (Maybe PrinterID),
           _reserveShader :: (String, String) -> SysMonad c IO (Maybe Shader),
           _pushMenuScreen :: MenuScreen Scalar (SysMonad c IO ()) -> SysMonad c IO (),
           _drawText :: PrinterID -> Label -> PositionedTextCommand -> SysMonad c IO (),
           _printAll :: [SysMonad c IO ()],
           _running :: IO Bool,
           _screenSize :: SysMonad c IO (Size Int),
           _worldCamera :: SysMonad c IO (Maybe Camera),
           _setWorldProjection :: Projection -> SysMonad c IO (),
           _drawnWorldMatrix :: SysMonad c IO Mat44,
           _uiCamera :: SysMonad c IO (Maybe Camera),
           _spawnedEntity :: [Entity -> SysMonad c IO ()]
           }

instance Show (RSC c) where
        show rsc = "RSC"

emptyRSC = RSC { 
               _resourcesPath = resourcesPath,
               _inputTouchUp = [],
               _inputTouchDown = [],
               _inputTouchLoc = [],
               _inputKeyDown = [],
               _inputKeyUp = [],
               _reserveTex = \_ -> return Nothing,
               _releaseTex = \_ -> return (),
               _reservePrinter = \_ -> return Nothing,
               _reserveShader = \_ -> return Nothing,
               _pushMenuScreen = \_ -> return (),
               _drawText = \_ _ _ -> return (),
               _printAll = [],
               _running = return True,
               _screenSize = return nullSize,
               _worldCamera = return Nothing,
               _setWorldProjection = \_ -> return (),
               _drawnWorldMatrix = return mat44Identity,
               _uiCamera = return Nothing,
               _spawnedEntity = []
               }

data Context compStore rsc = Context 
                           { 
                           _compStore :: compStore,
                           _remoteSystemCalls :: rsc 
                           }

instance Show c => Show (Context c r) where
        show (Context c r) = "Context -- CompStore: " ++ show c

type SystemContext c = Context ComponentStore (RSC c)

emptySystemContext = Context emptyComponentStore emptyRSC


emptyWorld :: c -> World c
emptyWorld gc = World { _entitySet = newEntitySet,
                   _systemContext = emptySystemContext,
                   _gameContext = gc
                 }

addNewEntity :: World c -> (Entity, World c)
addNewEntity w = let es = _entitySet w
                     (ent, new_es) = genEntity es
                     in (ent, w { _entitySet = new_es })

deleteEntitiesFromWorld :: World c -> [Entity] -> World c
deleteEntitiesFromWorld w ents = let es = _entitySet w
                                     es' = deleteEntities es ents
                                     in w { _entitySet = es' }

makeLenses ''RSC
makeLenses ''World
makeLenses ''Context

sysCon :: Lens' (World c) (SystemContext c)
sysCon = systemContext

gameCon :: Lens' (World c) c
gameCon = gameContext

