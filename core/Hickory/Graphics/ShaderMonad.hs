-- Monad transformer for binding inputs to a shader

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hickory.Graphics.ShaderMonad where


import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(..), MonadReader, ask, local, lift, mapReaderT)
import Control.Monad.Trans (MonadTrans)
import Hickory.Graphics.Shader (Shader)
import Hickory.Graphics.Uniforms (bindShaderUniform, Uniform)
import Hickory.Graphics.MatrixMonad (MatrixMonad(..))

class MonadIO m => ShaderMonad m where
  askShader :: m Shader

newtype ShaderT m a = ShaderT { unShaderT :: ReaderT Shader m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

mapShaderT :: (m a -> n b) -> ShaderT m a -> ShaderT n b
mapShaderT f = ShaderT . mapReaderT f . unShaderT

instance MonadReader r m => MonadReader r (ShaderT m) where
  ask = lift ask
  local f = mapShaderT id . local f

runShaderT :: Shader -> ShaderT m a -> m a
runShaderT sh = flip runReaderT sh . unShaderT

instance MonadIO m => ShaderMonad (ShaderT m) where
  askShader = ShaderT ask

bindUniform :: (Uniform a, ShaderMonad m) => String -> a -> m ()
bindUniform name val = askShader >>= bindShaderUniform name val

bindMatrix :: (ShaderMonad m, MatrixMonad m) => String -> m ()
bindMatrix name = do
  mat <- askMatrix
  askShader >>= bindShaderUniform name mat

instance MatrixMonad m => MatrixMonad (ShaderT m) where
  askMatrix = lift askMatrix
  xform mat = mapShaderT (xform mat)
