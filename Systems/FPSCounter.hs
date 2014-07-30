{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systems.FPSCounter (empty, make, SysData(..)) where
import Control.Monad.State
import Text.Printf
import System.IO
import Data.IORef

import Engine.System
import Engine.Event

reportInterval = 5.0 :: Double

make fps = System (run fps) (handleEv fps) nullInit

data SysData = SysData { 
             time :: Double,
             frames :: Int
            } deriving (Show)

empty = SysData 0.0 0

handleEv :: IORef SysData -> Event -> SysMonad IO ()
handleEv fps (PrintAll) = do
      mydata <- getSysData fps
      liftIO $ print mydata

handleEv fps _ = return ()

run :: IORef SysData -> Double -> SysMonad IO ()
run fps delta = 
      do
         SysData { time, frames } <- getSysData fps
         putSysData fps (SysData (time + delta) (frames + 1))

         when (time > reportInterval) $ do
            liftIO $ printf "%.2f FPS\n" ((fromIntegral frames) / time)
            liftIO $ hFlush stdout
            putSysData fps empty
