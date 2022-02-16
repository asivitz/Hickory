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
  , createPrinterVAOConfig
  , PositionedTextCommand(..)
  , textcommand
  , printVAOObj
  , PrinterMonad(..)
  , PrinterT
  , runPrinterT
  , loadDynamicVAO
  , DynamicVAOMonad(..)
  , DynamicVAOT(..)
  , runDynamicVAOT
  , withDynamicVAOs
  , withPrinting
  , drawText
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, lift, ask, asks, MonadReader(..), mapReaderT)
import Control.Monad.State.Strict (StateT, runStateT, modify, MonadState, mapStateT)
import Control.Monad.Trans (MonadTrans)
import Hickory.Color
import Hickory.Graphics.GLSupport
import Hickory.Text.Text
import Hickory.Graphics.VAO (VAOConfig, createIndexedVAOConfig, VAOObj(..), loadVerticesIntoIndexedVAOConfig, deleteVAOConfigs)
import Hickory.Graphics.Drawing (drawVAO, bindTextures)
import Hickory.Graphics.ShaderMonad (bindMatrix)
import Hickory.Graphics.MatrixMonad (MatrixMonad, MatrixT)

import Hickory.Utils.Utils
import Hickory.Graphics.Textures
import Linear (zero)
import qualified Data.Vector.Storable as V

data Printer a = Printer (Font a) TexID Shader

instance Eq (Printer a) where
  Printer fa _tid _ == Printer fb _tidb _ = fontName fa == fontName fb

instance Show (Printer a) where
  show (Printer font tid _) = "Printer:" ++ fontName font ++ "/" ++ show tid

createPrinterVAOConfig :: Shader -> IO VAOConfig
createPrinterVAOConfig shader = createIndexedVAOConfig shader
            [VertexGroup [Attachment "position" 3,
                          Attachment "texCoords" 2,
                          Attachment "color" 4]]

loadPrinter :: String -> Shader -> String -> IO (Maybe (Printer Int))
loadPrinter resPath shader name = do
        texid <- loadTexture resPath (name ++ ".png") texLoadDefaults { flipY = False }
        case texid of
            Nothing -> return Nothing
            Just tid -> do
                text <- readFileAsText $ resPath ++ "/fonts/" ++ name ++ ".fnt"
                case makeFont text name of
                    Left s -> do
                        print $ "Error: Can't parse font file for " ++ name ++ ".fnt Msg: " ++ s
                        return Nothing
                    Right font -> return $ Just (Printer font tid shader)

textcommand :: TextCommand
textcommand = TextCommand
  { text = ""
  , fontSize = 4
  , align = AlignCenter
  , valign = Middle
  , color = black
  , leftBump = 0
  }

printVAOObj :: Printer Int -> TextCommand -> VAOConfig -> IO VAOObj
printVAOObj (Printer font _ _) textCommand vaoconfig = do
  let command              = PositionedTextCommand zero textCommand
      (numsquares, floats) = transformTextCommandsToVerts [command] font

  if not $ null floats
    then do
      let (indices, numBlockIndices) = squareIndices (fromIntegral numsquares)
      loadVerticesIntoIndexedVAOConfig vaoconfig (V.fromList floats) (V.fromList indices)

      return (VAOObj vaoconfig (fromIntegral numBlockIndices) TriangleStrip)
    else error "Tried to print empty text command"

squareIndices :: (Num a, Enum a, Ord a) => a -> ([a], a)
squareIndices numSquares = (indices, 4 * numSquares + 2 * (numSquares - 1))
 where
  indices = concatMap
    ( \i ->
      let items                       = [i * 4, i * 4 + 1, i * 4 + 2, i * 4 + 3]
          -- We need to start and end degenerate squares if
          -- we're not at the beginning/end
          withStartOfDegenerateSquare = if i < numSquares - 1 then items ++ [i * 4 + 3] else items
      in  if i > 0 then (i * 4) : withStartOfDegenerateSquare else withStartOfDegenerateSquare
    )
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
  recordVAO  :: VAOObj -> m ()
  default recordVAO
    :: forall t n.
       ( DynamicVAOMonad n
       , MonadTrans t
       , m ~ t n
       )
    => VAOObj -> m ()
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

type DynamicVAOs = [VAOObj]

runPrinterT :: Printer Int -> PrinterT m a -> m a
runPrinterT printer = flip runReaderT printer . unPrinterT

runDynamicVAOT :: DynamicVAOT m a -> m (a, [VAOObj])
runDynamicVAOT = flip runStateT [] . unDynamicVAOT

loadDynamicVAO :: DynamicVAOMonad m => m VAOObj -> m VAOObj
loadDynamicVAO create = do
  vao <- create
  recordVAO vao
  pure vao

drawText :: (DynamicVAOMonad m, PrinterMonad m, MatrixMonad m) => TextCommand -> m ()
drawText tc = do
  printer@(Printer _ tex shader) <- getPrinter
  vao <- loadDynamicVAO do
    vc <- liftIO $ createPrinterVAOConfig shader
    liftIO $ printVAOObj printer tc vc

  drawVAO vao do
    bindTextures [tex]
    bindMatrix "modelMat"

withDynamicVAOs :: MonadIO m => DynamicVAOT m a -> m a
withDynamicVAOs f = do
  (a, allocedVAOs) <- runDynamicVAOT f
  liftIO $ deleteVAOConfigs $ map (\(VAOObj vc _ _) -> vc) allocedVAOs
  pure a

withPrinting :: MonadReader r m => (r -> Printer Int) -> PrinterT m a -> m a
withPrinting getter f = do
  printer <- asks getter
  runPrinterT printer f
