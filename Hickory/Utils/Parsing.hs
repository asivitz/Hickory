{-# LANGUAGE GADTs #-}

module Hickory.Utils.Parsing where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Data.Scientific
import Control.Monad (void)
import Hickory.Utils.Utils
import Data.Text (Text)
import Hickory.Math.Vector
import Data.Void

type Parser = Parsec Void Text

parseFromFile :: Parsec e Text a -> String -> IO (Either (ParseErrorBundle Text e) a)
parseFromFile p file = runParser p file <$> readFileAsText file

anyNumber :: (MonadParsec e s m, Token s ~ Char) => m Double
anyNumber = toRealFloat <$> L.scientific

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

lstToV3 :: [a] -> V3 a
lstToV3 [x,y,z] = V3 x y z
lstToV3 _ = error "Wrong size list for V3"

lstToV2 :: [a] -> V2 a
lstToV2 [x,y] = V2 x y
lstToV2 _ = error "Wrong size list for V2"
