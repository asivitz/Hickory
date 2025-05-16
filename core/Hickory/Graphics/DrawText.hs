{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Hickory.Graphics.DrawText
  ( textcommand
  , loadDynamicVAO
  , DynamicVAOMonad(..)
  , DynamicVAOT(..)
  , runDynamicVAOT
  , withDynamicVAOs
  , squareIndices
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, lift, ask, MonadReader(..))
import Control.Monad.State.Strict (StateT, runStateT, modify, MonadState, mapStateT)
import Control.Monad.Trans (MonadTrans)
import Hickory.Graphics.VAO (VAO(..), deleteVAOConfigs)
import Hickory.Graphics.MatrixMonad (MatrixT)

import Hickory.Text.Types (TextCommand (..), XAlign (..), YAlign (..))

textcommand :: TextCommand
textcommand = TextCommand
  { text   = ""
  , align  = AlignCenter
  , valign = AlignMiddle
  , scrollFrame = Nothing
  }

squareIndices :: (Num a, Enum a, Ord a) => a -> ([a], a)
squareIndices numSquares = (indices, 6 * numSquares)
 where
  indices = concatMap (\((*4) -> i) -> [i, i+2, i+1, i+2, i+3, i+1])
    [0 .. (numSquares - 1)]

-- Creating new VAOs during render

class MonadIO m => DynamicVAOMonad m where
  recordVAO  :: VAO -> m ()
  default recordVAO
    :: forall t n.
       ( DynamicVAOMonad n
       , MonadTrans t
       , m ~ t n
       )
    => VAO -> m ()
  recordVAO = lift . recordVAO

instance DynamicVAOMonad m => DynamicVAOMonad (ReaderT r m) where
  recordVAO = lift . recordVAO

instance DynamicVAOMonad m => DynamicVAOMonad (MatrixT m)

newtype DynamicVAOT m a = DynamicVAOT { unDynamicVAOT :: StateT DynamicVAOs m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState DynamicVAOs, MonadTrans)

mapDynamicVAOT :: (m (a, DynamicVAOs) -> n (b, DynamicVAOs)) -> DynamicVAOT m a -> DynamicVAOT n b
mapDynamicVAOT f = DynamicVAOT . mapStateT f . unDynamicVAOT

instance MonadReader r m => MonadReader r (DynamicVAOT m) where
  ask = lift ask
  local f = mapDynamicVAOT id . local f

instance MonadIO m => DynamicVAOMonad (DynamicVAOT m) where
  recordVAO v = DynamicVAOT $ modify (v:)

type DynamicVAOs = [VAO]

runDynamicVAOT :: DynamicVAOT m a -> m (a, [VAO])
runDynamicVAOT = flip runStateT [] . unDynamicVAOT

loadDynamicVAO :: DynamicVAOMonad m => m VAO -> m VAO
loadDynamicVAO create = do
  vao <- create
  recordVAO vao
  pure vao

withDynamicVAOs :: MonadIO m => DynamicVAOT m a -> m a
withDynamicVAOs f = do
  (a, allocedVAOs) <- runDynamicVAOT f
  liftIO $ deleteVAOConfigs $ map (\(VAO vc _ _) -> vc) allocedVAOs
  pure a
