{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Run where

import Engine.System
import Engine.Model
import Data.Time
import Control.Monad.State
import qualified Systems.GLFWPlatform as GLFWPlatform
import Math.Matrix
import Types.Types

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

iter :: RenderInfo -> (Model -> Mat44) -> (Mat44 -> Model -> IO ()) -> (RenderInfo -> Double -> Model -> IO Model) -> Model -> UTCTime -> IO ()
iter !ri@(RenderInfo _ ss) !matrixFunc !render !step !model !prev_time = do
        current_time <- getCurrentTime
        let delta = min 0.1 $ realToFrac (diffUTCTime current_time prev_time)

        model' <- step ri delta model
        let matrix' = matrixFunc model'
        render matrix' model'

        iter (RenderInfo matrix' ss) matrixFunc render step model' current_time

run :: Size Int -> (Size Int -> Model -> Mat44) -> (Mat44 -> Model -> IO ()) -> (RenderInfo -> Double -> Model -> IO Model) -> Model -> IO ()
run scrSize matrixFunc render step model = do
        ct <- getCurrentTime

        iter (RenderInfo (matrixFunc scrSize model) scrSize) (matrixFunc scrSize) render step model ct

{-initAndRun :: World r -> SysMonad r IO [System r] -> IO ()-}
{-initAndRun w initF = do-}
        {-(systems, w') <- runStateT initF w-}
        {-run w' systems-}

{-newWorldWithResourcesPath :: Context cs rsc -> String -> World (Context cs rsc)-}
{-newWorldWithResourcesPath context path =-}
        {-registerResourceToWorld sysCon (emptyWorld context) resourcesPath (return path)-}

