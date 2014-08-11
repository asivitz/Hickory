{-# LANGUAGE BangPatterns #-}

module Engine.Run where

import Engine.System
import Engine.World
import Engine.Event
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

simulate :: World -> [System] -> EventStore -> RPC -> Double -> IO (World, EventStore)
simulate world systems events rpc delta = do
      (newWorld, _, newEvents) <- execStateT (mapM_ (\s -> (runSys s) delta) systems) (world, rpc, emptyEventStore)
      (newWorld', _, newEvents') <- execStateT (mapM_ (handleEvents events) systems) (newWorld, rpc, newEvents)
      return (newWorld', newEvents')

iter :: World -> [System] -> EventStore -> RPC -> UTCTime -> IO ()
iter !world !systems !events !rpc !prev_time = do
      current_time <- getCurrentTime
      let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

      (newWorld, newEvents) <- simulate world systems events rpc delta

      {-governFPS current_time-}

      unless (any (\e -> case e of Quit -> True; _ -> False) newEvents) $ 
        iter newWorld systems newEvents rpc current_time

run :: [System] -> RPC -> IO ()
run systems rpc = do
        ct <- getCurrentTime

        (w, _, es) <- execStateT (mapM_ initSys systems) (emptyWorld, rpc, emptyEventStore)
        iter w systems es rpc ct
