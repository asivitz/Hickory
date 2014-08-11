{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Run where

import Engine.System
import Engine.World
import Engine.Event
import Data.Time
import Control.Monad.State
import qualified Systems.Platform as Platform
import Data.IORef

{-
governFPS :: UTCTime -> IO ()
governFPS initialTime = do
   -- I have no idea when this stuff actually runs bc of laziness
   after_time <- getCurrentTime
   let elapsed = realToFrac (diffUTCTime after_time initialTime)
       millisecondsEarly = 16.66 - elapsed * 1000 :: Double

   when (millisecondsEarly > 0) $
      threadDelay $ floor (millisecondsEarly * 1000)
      -}

simulate :: World -> [System] -> RPC -> Double -> IO World
simulate world systems rpc delta = do
      (newWorld, _) <- execStateT (mapM_ (\s -> (runSys s) delta) systems) (world, rpc)
      return newWorld

iter :: World -> [System] -> IORef Platform.SysData -> RPC -> UTCTime -> IO ()
iter !world !systems !platform !rpc !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        newWorld <- simulate world systems rpc delta

        {-governFPS current_time-}

        Platform.SysData { Platform.running } <- readIORef platform
        
        when running $
            iter newWorld systems platform rpc current_time

run :: [System] -> IORef Platform.SysData -> IO ()
run systems platform = do
        ct <- getCurrentTime

        (w, rpc') <- execStateT (mapM_ initSys systems) (emptyWorld, emptyRPC)
        iter w systems platform rpc' ct
