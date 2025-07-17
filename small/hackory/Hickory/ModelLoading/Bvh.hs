{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Hickory.ModelLoading.Bvh where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe
import Hickory.ModelLoading.Parsing
import Data.Text (Text, pack)
import Hickory.Math.Vector (Scalar)

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser Text
identifier = lexeme $ pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '.'))

bracket :: Parser a -> Parser a
bracket = between (symbol "{") (symbol "}")

number :: Parser Scalar
number = lexeme (signed anyNumber)

integer :: Parser Integer
integer = lexeme L.decimal

parseBVH :: Parser BVH
parseBVH = do
        tree <- reserved "HIERARCHY" *> reserved "ROOT" *> parseJointBody
        motion <- parseMotion
        eof
        return $ BVH tree motion

signed :: Num b => Parser b -> Parser b
signed p = do
        n <- optional (symbol "-")
        num <- p
        return $ if isJust n then negate num else num

parseFrame :: Parser [Scalar]
parseFrame = do
        let f = (do
                    n <- signed anyNumber
                    optional (satisfy (== ' '))
                    return n)
        fs <- some f
        eol
        return fs

parseOffset :: Parser (Scalar,Scalar,Scalar)
parseOffset = reserved "OFFSET" *>
    ((,,) <$> number <*> number <*> number)

parseEndSite :: Parser Joint
parseEndSite = reserved "End Site" *>
        (JointEnd <$> bracket parseOffset)

parseChannels :: Parser [Text]
parseChannels = do
        reserved "CHANNELS"
        n <- fromIntegral <$> integer
        count n identifier

parseJointBody :: Parser Joint
parseJointBody = do
        jname <- identifier
        (off, chans, jnts) <- bracket $ do
            off <- parseOffset
            chans <- parseChannels
            joints <- many ((reserved "JOINT" *> parseJointBody) <|> parseEndSite)
            return (off, chans, joints)
        return $ Joint chans jname off jnts

parseMotion :: Parser Motion
parseMotion = do
        reserved "MOTION"
        reserved "Frames:"
        nFrames <- integer
        reserved "Frame Time:"
        fTime <- lexeme floating
        allFrames <- some (parseFrame)
        return $ Motion nFrames fTime allFrames

data BVH = BVH Joint Motion
               deriving (Show)

data Motion = Motion {
            numFrames :: Integer,
            frameTime :: Scalar,
            frames :: [[Scalar]]
            }
            deriving (Show)

type Offset = (Scalar, Scalar, Scalar)

data Joint = Joint {
           channels :: [Text],
           name :: Text,
           offset :: Offset,
           children :: [Joint]
           }
           | JointEnd Offset
           deriving (Show)

loadBVH :: String -> IO BVH
loadBVH filePath = do
        res <- parseFromFile parseBVH filePath
        case res of
            Left err -> error (show err)
            Right anim -> return anim
