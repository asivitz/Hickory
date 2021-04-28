{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

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
  , renderText
  , loadDynamicVAO
  , DynamicVAOMonad(..)
  , DynamicVAOT(..)
  , runDynamicVAOT
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, lift, ask)
import Control.Monad.State.Strict (StateT, runStateT, modify, MonadState)
import Control.Monad.Trans (MonadTrans)
import Hickory.Color
import Hickory.Graphics.GLSupport
import Hickory.Graphics.Drawing
import Hickory.Graphics.Types (DrawSpec(..), RenderTree(..))
import Hickory.Text.Text
import Hickory.Graphics.VAO (VAOConfig, createVAOConfig, VAOObj(..), loadVerticesIntoVAOConfig)

import Hickory.Utils.Utils
import Hickory.Graphics.Textures
import Graphics.GL.Compatibility41 as GL
import Linear (zero)
import qualified Data.Vector.Storable as V

data Printer a = Printer (Font a) TexID Shader

instance Eq (Printer a) where
        Printer fa tid _ == Printer fb tidb _ = fontName fa == fontName fb

instance Show (Printer a) where
        show (Printer font tid _) = "Printer:" ++ fontName font ++ "/" ++ show tid

createPrinterVAOConfig :: Shader -> IO VAOConfig
createPrinterVAOConfig shader = createVAOConfig shader
            [VertexGroup [Attachment "position" 3,
                          Attachment "texCoords" 2,
                          Attachment "color" 4]]

loadPrinter :: String -> Shader -> String -> IO (Maybe (Printer Int))
loadPrinter resPath shader name = do
        texid <- loadTexture resPath (name ++ ".png") GL_REPEAT
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
      loadVerticesIntoVAOConfig vaoconfig (V.fromList floats) (V.fromList indices)

      return (VAOObj vaoconfig (fromIntegral numBlockIndices) TriangleStrip)
    else error "Tried to print empty text command"

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

newtype DynamicVAOT m a = DynamicVAOT { unDynamicVAOT :: StateT DynamicVAOs m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState DynamicVAOs, MonadTrans)
  deriving anyclass PrinterMonad

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

renderText :: (DynamicVAOMonad m, PrinterMonad m) => TextCommand -> m RenderTree
renderText tc = do
  printer@(Printer _ tex shader) <- getPrinter
  vao <- loadDynamicVAO do
    vc <- liftIO $ createPrinterVAOConfig shader
    liftIO $ printVAOObj printer tc vc

  pure $ Primitive [] (Just tex) (VAO vao)
