{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Scene.Run where

import Data.Time
import Types.Types

import Data.Maybe
import Utils.Utils
import Engine.Scene.Input
import Engine.Scene.Scene

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

{-simulate :: World c -> [System c] -> Double -> IO (World c)-}
{-simulate world systems delta = execStateT (mapM_ (`runSys` delta) systems) world-}

mapWithOthers :: (a -> [a] -> b) -> [a] -> [b]
mapWithOthers fun lst = sub fun [] lst
    where sub f prevxs (x:xs) = f x (prevxs ++ xs) : sub f (x:prevxs) xs
          sub f _ [] = []

iter :: Show ie => IO () -> [SceneOperator ie] -> ([SceneOperator ie] -> IO ()) -> UTCTime -> IO ()
iter !stepInp !operators !renderFunc !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        stepInp

        sequence_ . mapWithOthers (\oper others -> do
             outEvents <- (_step oper) delta
             mapM_ (\otherOp -> mapM_ (\ie -> (_addEvent otherOp) ie) outEvents) others
             )
             $ operators

        renderFunc operators

        iter stepInp operators renderFunc current_time

run :: Show ie => IO () -> [SceneOperator ie] -> ([SceneOperator ie] -> IO ()) -> IO ()
run stepInp operators renderFunc = do
        ct <- getCurrentTime

        iter stepInp operators renderFunc ct
