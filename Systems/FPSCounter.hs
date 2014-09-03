{-# LANGUAGE NamedFieldPuns #-}

module Systems.FPSCounter (make) where
import Control.Monad.State
import Text.Printf
import System.IO

import Engine.System
import Engine.World
import Data.IORef

import Utils.System

reportInterval = 5.0 :: Double

make :: Show c => SysMonad c IO (System c)
make = do
        fps <- liftIO $ newIORef empty
        registerEvent systemContext printAll $ do
            (printSysData fps)
            w <- getWorld
            liftIO $ print w

        return $ System (run fps)

data SysData = SysData { 
             time :: !Double,
             frames :: !Int
            } deriving (Show)

empty = SysData 0.0 0

run fps delta = 
      do
         SysData { time, frames } <- getSysData fps
         putSysData fps (SysData (time + delta) (frames + 1))

         when (time > reportInterval) $ do
            liftIO $ printf "%.2f FPS\n" ((fromIntegral frames) / time)
            liftIO $ hFlush stdout
            {-runEventId sysCon printAll-}
            putSysData fps empty
