{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Run where

import Engine.System
import Engine.World
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

simulate :: World c -> [System c] -> Double -> IO (World c)
simulate world systems delta = do
      newWorld <- execStateT (mapM_ (\s -> (runSys s) delta) systems) world
      return newWorld

iter :: World c -> [System c] -> IORef Platform.SysData -> UTCTime -> IO ()
iter !world !systems !platform !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        newWorld <- simulate world systems delta

        {-governFPS current_time-}

        Platform.SysData { Platform.running } <- readIORef platform
        
        when running $
            iter newWorld systems platform current_time

run :: [System c] -> c -> IORef Platform.SysData -> IO ()
run systems userContext platform = do
        ct <- getCurrentTime

        w <- execStateT (mapM_ initSys systems) (emptyWorld userContext)
        iter w systems platform ct
