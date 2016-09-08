{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.Utils.OBJ where

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Data.Maybe
import Hickory.Math.Vector
import Hickory.Utils.Parsing

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '.' <|> char '_')

anySignedNumber = do
        n <- signed anyNumber
        optional (satisfy (== ' '))
        return n

number :: Parser Double
number = lexeme (signed anyNumber)

integer :: Parser Integer
integer = lexeme L.integer

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

lstToV3 [x,y,z] = V3 x y z
lstToV3 _ = error "Wrong size list for V3"
lstToV2 [x,y] = V2 x y
lstToV2 _ = error "Wrong size list for V2"

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") empty

parseVertex = lstToV3 <$> try (reserved "v" *> count 3 (lexeme anySignedNumber))
parseTextureCoord = lstToV2 <$> try (reserved "vt" *> count 2 (lexeme anyNumber))
parseNormal = lstToV3 <$> try (reserved "vn" *> count 3 (lexeme anySignedNumber))
parseFace = lstToV3 <$> try (reserved "f" *> count 3 (lexeme parseTriple))
    where parseTriple = do
                            v <- fromIntegral <$> L.integer
                            char '/'
                            t <- fromIntegral <$> L.integer
                            char '/'
                            n <- fromIntegral <$> L.integer
                            return (v,t,n)

parseOBJ :: Parser (OBJ Double)
parseOBJ = do
        sc
        reserved "mtllib" *> identifier *> reserved "o" *> identifier
        vs <- many parseVertex
        tCoords <- many parseTextureCoord
        norms <- many parseNormal
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

