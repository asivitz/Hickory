{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Run where

import Engine.System
import Engine.World
import Data.Time
import Control.Monad.State

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
simulate world systems delta = execStateT (mapM_ (`runSys` delta) systems) world

iter :: World c -> [System c] -> UTCTime -> IO ()
iter !world !systems !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        newWorld <- simulate world systems delta

        shouldRun <- case newWorld of
                         World { _systemContext = (Context _ RSC { _running } ) } -> _running

        {-governFPS current_time-}

        when shouldRun $
            iter newWorld systems current_time

run :: World c -> [System c] -> IO ()
run world systems = do
        ct <- getCurrentTime

        iter world systems ct

initAndRun :: World r -> SysMonad r IO [System r] -> IO ()
initAndRun w initF = do
        (systems, w') <- runStateT initF w
        run w' systems

newWorldWithResourcesPath :: String -> Context cs rsc -> World (Context cs rsc)
newWorldWithResourcesPath path context =
        registerResourceToWorld sysCon (emptyWorld context) resourcesPath (return path)

