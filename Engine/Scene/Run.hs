{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Scene.Run where

import Data.Time

import Engine.Scene.Scene

mapWithOthers :: (a -> [a] -> b) -> [a] -> [b]
mapWithOthers fun lst = sub fun [] lst
    where sub f prevxs (x:xs) = f x (prevxs ++ xs) : sub f (x:prevxs) xs
          sub f _ [] = []

runOneFrame :: Show ie => [SceneOperator ie] -> ([SceneOperator ie] -> IO ()) -> Double -> IO ()
runOneFrame operators renderFunc delta = do
        sequence_ . mapWithOthers (\oper others -> do
             outEvents <- (_step oper) delta
             mapM_ (\otherOp -> mapM_ (\ie -> (_addEvent otherOp) ie) outEvents) others
             )
             $ operators

        renderFunc operators

iter :: Show ie => IO () -> [SceneOperator ie] -> ([SceneOperator ie] -> IO ()) -> UTCTime -> IO ()
iter !stepInp !operators !renderFunc !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        stepInp 
        runOneFrame operators renderFunc delta

        iter stepInp operators renderFunc current_time

run :: Show ie => IO () -> [SceneOperator ie] -> ([SceneOperator ie] -> IO ()) -> IO ()
run stepInp operators renderFunc = do
        ct <- getCurrentTime

        iter stepInp operators renderFunc ct
