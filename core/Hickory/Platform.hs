module Hickory.Platform where

import Data.Time
import Data.IORef
import Hickory.Input

makeTimePoller :: IO (IO NominalDiffTime)
makeTimePoller = do
  initial_time <- getCurrentTime
  time <- newIORef initial_time
  return $ do
      new_time <- getCurrentTime
      atomicModifyIORef time (\prev_time -> (new_time, min 0.1 (diffUTCTime new_time prev_time)))

makeInputPoller :: ((RawInput -> IO ()) -> IO (IO ())) -> IO (IO [RawInput])
makeInputPoller inputSetup = do
  is <- newIORef []
  stepInp <- inputSetup (addRawInput is)

  return $ do
      stepInp
      atomicModifyIORef is ([],)
  where
  addRawInput stream event = atomicModifyIORef stream (\evs -> (evs ++ [event], ()))
