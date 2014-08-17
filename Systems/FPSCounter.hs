{-# LANGUAGE NamedFieldPuns #-}

module Systems.FPSCounter (make) where
import Control.Monad.State
import Text.Printf
import System.IO

import Engine.System
import Engine.World
import Data.IORef

reportInterval = 5.0 :: Double

make :: SysMonad c IO (System c)
make = do
        fps <- liftIO $ newIORef empty
        registerEvent printAll (printAll' fps)

        return $ System (run fps)

data SysData = SysData { 
             time :: Double,
             frames :: Int
            } deriving (Show)

empty = SysData 0.0 0

printAll' fps = do
      mydata <- getSysData fps
      liftIO $ print mydata

run fps delta = 
      do
         SysData { time, frames } <- getSysData fps
         putSysData fps (SysData (time + delta) (frames + 1))

         when (time > reportInterval) $ do
            liftIO $ printf "%.2f FPS\n" ((fromIntegral frames) / time)
            liftIO $ hFlush stdout
            putSysData fps empty
