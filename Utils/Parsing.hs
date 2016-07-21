{-# LANGUAGE GADTs #-}

module Utils.Parsing where

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Prim
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C
import Data.Scientific
import Control.Monad (void)

parseFromFile p file = runParser p file <$> TIO.readFile file

anyNumber :: (MonadParsec e s m, Token s ~ Char) => m Double
anyNumber = toRealFloat <$> L.number

floating :: (MonadParsec e s m, Token s ~ Char) => m Double
floating = label "floating" (read <$> f)
        where front = do
                chars <- many C.digitChar
                return $ if null chars then "0" else chars
              f = (++) <$> front <*> option " " fraction

fraction :: (MonadParsec e s m, Token s ~ Char) => m String
fraction = do
  void (C.char '.')
  d <- some C.digitChar
  return ('.' : d)
