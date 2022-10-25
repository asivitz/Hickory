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
  ( Printer(..)
  , loadPrinter
  , textcommand
  , PrinterMonad(..)
  , PrinterT
  , runPrinterT
  , loadDynamicVAO
  , DynamicVAOMonad(..)
  , DynamicVAOT(..)
  , runDynamicVAOT
  , withDynamicVAOs
  , withPrinting
  , squareIndices
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, lift, ask, asks, MonadReader(..), mapReaderT)
import Control.Monad.State.Strict (StateT, runStateT, modify, MonadState, mapStateT)
import Control.Monad.Trans (MonadTrans)
import Hickory.Graphics.VAO (VAO(..), deleteVAOConfigs)
import Hickory.Graphics.MatrixMonad (MatrixT)

import Hickory.Graphics.Textures
import Hickory.Graphics.Shader (Shader)
import Hickory.Text.ParseJson (Font, makeFont)
import Hickory.Text.Types (TextCommand (..), XAlign (..), YAlign (..))
import qualified Data.ByteString.Lazy as BS

data Printer a = Printer Font TexID Shader

loadPrinter :: String -> Shader -> String -> IO (Maybe (Printer Int))
loadPrinter resPath shader name = do
  texid <- loadTexture resPath (name ++ ".png") texLoadDefaults { flipY = False }
  case texid of
    Nothing -> return Nothing
    Just tid -> do
      bs <- BS.readFile $ resPath ++ "/fonts/" ++ name ++ ".json"
      case makeFont bs of
        Left s -> do
            print $ "Error: Can't parse font file for " ++ name ++ ".fnt Msg: " ++ s
            return Nothing
        Right font -> return $ Just (Printer font tid shader)

textcommand :: TextCommand
textcommand = TextCommand
  { text   = ""
  , align  = AlignCenter
  , valign = AlignMiddle
  }

{-
printVAOObj :: Printer Int -> TextCommand -> IO VAO
printVAOObj (Printer font _ shader) textCommand = do
  let command              = PositionedTextCommand zero textCommand
      (numsquares, floats) = transformTextCommandsToVerts [command] font

  if not $ null floats
    then do
      let (indices, numBlockIndices) = squareIndices (fromIntegral numsquares)
      createIndexedVAO
        shader
        [VertexGroup [Attachment "position" 3,
                      Attachment "texCoords" 2,
                      Attachment "color" 4]]
        (V.fromList floats, V.fromList indices)
        TriangleStrip
    else error "Tried to print empty text command"
    -}

squareIndices :: (Num a, Enum a, Ord a) => a -> ([a], a)
squareIndices numSquares = (indices, 6 * numSquares)
 where
  indices = concatMap (\((*4) -> i) -> [i, i+2, i+1, i+2, i+3, i+1])
    [0 .. (numSquares - 1)]

-- Creating new VAOs during render

class Monad m => PrinterMonad m where
  getPrinter :: m (Printer Int)
  default getPrinter
    :: forall t n.
       ( PrinterMonad n
       , MonadTrans t
       , m ~ t n
       )
    => m (Printer Int)
  getPrinter = lift getPrinter

instance PrinterMonad m => PrinterMonad (ReaderT r m) where
  getPrinter = lift getPrinter

newtype PrinterT m a = PrinterT { unPrinterT :: ReaderT (Printer Int) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)
  deriving anyclass DynamicVAOMonad

mapPrinterT :: (m a -> n b) -> PrinterT m a -> PrinterT n b
mapPrinterT f = PrinterT . mapReaderT f . unPrinterT

instance MonadReader r m => MonadReader r (PrinterT m) where
  ask = lift ask
  local f = mapPrinterT id . local f

instance MonadIO m => PrinterMonad (PrinterT m) where
  getPrinter = PrinterT ask

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
instance PrinterMonad m => PrinterMonad (MatrixT m)

newtype DynamicVAOT m a = DynamicVAOT { unDynamicVAOT :: StateT DynamicVAOs m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState DynamicVAOs, MonadTrans)
  deriving anyclass PrinterMonad

mapDynamicVAOT :: (m (a, DynamicVAOs) -> n (b, DynamicVAOs)) -> DynamicVAOT m a -> DynamicVAOT n b
mapDynamicVAOT f = DynamicVAOT . mapStateT f . unDynamicVAOT

instance MonadReader r m => MonadReader r (DynamicVAOT m) where
  ask = lift ask
  local f = mapDynamicVAOT id . local f

instance MonadIO m => DynamicVAOMonad (DynamicVAOT m) where
  recordVAO v = DynamicVAOT $ modify (v:)

type DynamicVAOs = [VAO]

runPrinterT :: Printer Int -> PrinterT m a -> m a
runPrinterT printer = flip runReaderT printer . unPrinterT

runDynamicVAOT :: DynamicVAOT m a -> m (a, [VAO])
runDynamicVAOT = flip runStateT [] . unDynamicVAOT

loadDynamicVAO :: DynamicVAOMonad m => m VAO -> m VAO
loadDynamicVAO create = do
  vao <- create
  recordVAO vao
  pure vao

{-
drawText :: (DynamicVAOMonad m, PrinterMonad m, MatrixMonad m) => TextCommand -> m ()
drawText tc = do
  printer@(Printer _ tex _) <- getPrinter
  vao <- loadDynamicVAO do
    liftIO $ printVAOObj printer tc

  drawVAO vao do
    bindTextures [tex]
    bindMatrix "modelMat"
    -}

withDynamicVAOs :: MonadIO m => DynamicVAOT m a -> m a
withDynamicVAOs f = do
  (a, allocedVAOs) <- runDynamicVAOT f
  liftIO $ deleteVAOConfigs $ map (\(VAO vc _ _) -> vc) allocedVAOs
  pure a

withPrinting :: MonadReader r m => (r -> Printer Int) -> PrinterT m a -> m a
withPrinting getter f = do
  printer <- asks getter
  runPrinterT printer f
