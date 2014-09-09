{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Run where

import Engine.System
import Engine.World
import Data.Time
import Control.Monad.State
import qualified Systems.GLFWPlatform as GLFWPlatform

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

iter :: (Model -> IO ()) -> (Double -> Model -> IO Model) -> Model -> UTCTime -> IO ()
iter !render !step !model !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        model' <- step delta model
        render model'

        iter render step model' current_time

run :: (Model -> IO ()) -> (Double -> Model -> IO Model) -> Model -> IO ()
run render step model = do
        ct <- getCurrentTime

        iter render step model ct

{-initAndRun :: World r -> SysMonad r IO [System r] -> IO ()-}
{-initAndRun w initF = do-}
        {-(systems, w') <- runStateT initF w-}
        {-run w' systems-}

{-newWorldWithResourcesPath :: Context cs rsc -> String -> World (Context cs rsc)-}
{-newWorldWithResourcesPath context path =-}
        {-registerResourceToWorld sysCon (emptyWorld context) resourcesPath (return path)-}

