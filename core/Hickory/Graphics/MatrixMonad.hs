-- Monad transformer for building up a matrix transformation (e.g. for
-- a model matrix shader input)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hickory.Graphics.MatrixMonad where


import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(..), MonadReader, ask, local, lift, mapReaderT)
import Hickory.Math
import Linear ((!*!), identity)
import Control.Monad.Trans (MonadTrans)

class Monad m => MatrixMonad m where
  xform :: Mat44 -> m a -> m a
  askMatrix :: m Mat44

withXForm :: MatrixMonad m => Mat44 -> (Mat44 -> m a) -> m a
withXForm mat f = xform mat $ askMatrix >>= f

newtype MatrixT m a = MatrixT { unMatrixT :: ReaderT Mat44 m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

mapMatrixT :: (m a -> n b) -> MatrixT m a -> MatrixT n b
mapMatrixT f = MatrixT . mapReaderT f . unMatrixT

instance MonadReader r m => MonadReader r (MatrixT m) where
  ask = lift ask
  local f = mapMatrixT id . local f

runMatrixT :: MatrixT m a -> m a
runMatrixT = flip runReaderT identity . unMatrixT

instance Monad m => MatrixMonad (MatrixT m) where
  xform trans (MatrixT matf) = MatrixT $ local (!*! trans) matf
  askMatrix = MatrixT ask
