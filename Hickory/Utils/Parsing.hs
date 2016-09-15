{-# LANGUAGE GADTs #-}

module Hickory.Utils.Parsing where

import Text.Megaparsec
import Text.Megaparsec.Prim
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C
import Data.Scientific
import Control.Monad (void)
import Hickory.Utils.Utils

parseFromFile p file = runParser p file <$> readFileAsText file

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
