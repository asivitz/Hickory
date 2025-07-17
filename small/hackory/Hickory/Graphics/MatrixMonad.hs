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
import Control.Monad.Writer.Class (MonadWriter (..))

class Monad m => MatrixMonad m where
  xform :: Mat44 -> m a -> m a
  clearXform :: m a -> m a
  askMatrix :: m Mat44

withXform :: MatrixMonad m => Mat44 -> (Mat44 -> m a) -> m a
withXform mat f = xform mat $ askMatrix >>= f

newtype MatrixT m a = MatrixT { unMatrixT :: ReaderT Mat44 m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

mapMatrixT :: (m a -> n b) -> MatrixT m a -> MatrixT n b
mapMatrixT f = MatrixT . mapReaderT f . unMatrixT

instance MonadReader r m => MonadReader r (MatrixT m) where
  ask = lift ask
  local f = mapMatrixT id . local f

instance MonadWriter w m => MonadWriter w (MatrixT m) where
  tell   = lift . tell
  listen = mapMatrixT id . listen
  pass   = mapMatrixT id . pass

runMatrixT :: MatrixT m a -> m a
runMatrixT = flip runReaderT identity . unMatrixT

instance Monad m => MatrixMonad (MatrixT m) where
  xform trans (MatrixT matf) = MatrixT $ local (!*! trans) matf
  askMatrix = MatrixT ask
  clearXform (MatrixT matf) = MatrixT $ local (const identity) matf
