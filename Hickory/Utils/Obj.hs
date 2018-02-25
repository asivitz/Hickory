{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.Utils.Obj where

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe
import Hickory.Math.Vector
import Hickory.Utils.Parsing
import Data.Text (Text, pack)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser Text
identifier = lexeme $ pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '.' <|> char '_'))

anySignedNumber :: Parser Double
anySignedNumber = do
        n <- signed anyNumber
        optional (satisfy (== ' '))
        return n

number :: Parser Double
number = lexeme (signed anyNumber)

integer :: Parser Integer
integer = lexeme L.decimal

signed :: Num a => Parser a -> Parser a
signed p = do
        n <- optional (symbol "-")
        num <- p
        return $ if isJust n then negate num else num

type Face = V3 (Int, Int, Int) -- Faces are triples of indices into the other lists

data OBJ a = OBJ {
         vertices :: [V3 a],
         textureCoords :: [V2 a],
         normals :: [V3 a],
         faces :: [Face]
         }
         deriving (Show)

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") empty

parseVertex :: Parser (V3 Double)
parseVertex = lstToV3 <$> try (reserved "v" *> count 3 (lexeme anySignedNumber))

parseTextureCoord :: Parser (V2 Double)
parseTextureCoord = lstToV2 <$> try (reserved "vt" *> count 2 (lexeme anyNumber))

parseNormal :: Parser (V3 Double)
parseNormal = lstToV3 <$> try (reserved "vn" *> count 3 (lexeme anySignedNumber))

parseFace :: Parser Face
parseFace = lstToV3 <$> try (reserved "f" *> count 3 (lexeme parseTriple))
 where
  parseTriple = do
    v <- L.decimal
    char '/'
    t <- L.decimal
    char '/'
    n <- L.decimal
    pure (v, t, n)

parseOBJ :: Parser (OBJ Double)
parseOBJ = do
  sc
  reserved "mtllib" *> identifier *> reserved "o" *> identifier
  vs      <- many parseVertex
  tCoords <- many parseTextureCoord
  norms   <- many parseNormal
  reserved "usemtl" *> identifier
  reserved "s" *> identifier
  fs <- many parseFace

  return $ OBJ vs tCoords norms fs

loadOBJ :: String -> IO (OBJ Double)
loadOBJ filePath = do
        res <- parseFromFile parseOBJ filePath
        case res of
            Left err -> error (show err)
            Right obj -> return obj

