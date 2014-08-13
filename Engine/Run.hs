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

simulate :: World -> [System c] -> SystemContext c -> Double -> IO (World, SystemContext c)
simulate world systems sc delta = do
      (newWorld, sc') <- execStateT (mapM_ (\s -> (runSys s) delta) systems) (world, sc)
      return (newWorld, sc')

iter :: World -> [System c] -> IORef Platform.SysData -> SystemContext c -> UTCTime -> IO ()
iter !world !systems !platform !sc !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        (newWorld, newContext) <- simulate world systems sc delta

        {-governFPS current_time-}

        Platform.SysData { Platform.running } <- readIORef platform
        
        when running $
            iter newWorld systems platform newContext current_time

run :: [System c] -> c -> IORef Platform.SysData -> IO ()
run systems userContext platform = do
        ct <- getCurrentTime

        (w, sc') <- execStateT (mapM_ initSys systems) (emptyWorld, emptySystemContext userContext)
        iter w systems platform sc' ct
