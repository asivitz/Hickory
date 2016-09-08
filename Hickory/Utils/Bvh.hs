{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Hickory.Utils.BVH where

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Data.Maybe
import Hickory.Utils.Parsing

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '.')

bracket :: Parser a -> Parser a
bracket = between (symbol "{") (symbol "}")

number :: Parser Double
number = lexeme (signed anyNumber)

integer :: Parser Integer
integer = lexeme L.integer

parseBVH :: Parser BVH
parseBVH = do
        tree <- reserved "HIERARCHY" *> reserved "ROOT" *> parseJointBody
        motion <- parseMotion
        eof
        return $ BVH tree motion

signed p = do
        n <- optional (symbol "-")
        num <- p
        return $ if isJust n then negate num else num

parseFrame = do
        let f = (do
                    n <- signed anyNumber
                    optional (satisfy (== ' '))
                    return n)
        fs <- some f
        eol
        return fs

parseOffset = reserved "OFFSET" *>
    ((,,) <$> number <*> number <*> number)

parseEndSite = reserved "End Site" *>
        (JointEnd <$> bracket parseOffset)

parseChannels = do
        reserved "CHANNELS"
        n <- fromIntegral <$> integer
        count n identifier

parseJointBody = do
        jname <- identifier
        (off, chans, jnts) <- bracket $ do
            off <- parseOffset
            chans <- parseChannels
            joints <- many ((reserved "JOINT" *> parseJointBody) <|> parseEndSite)
            return (off, chans, joints)
        return $ Joint chans jname off jnts

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
            frameTime :: Double,
            frames :: [[Double]]
            }
            deriving (Show)

type Offset = (Double, Double, Double)

data Joint = Joint {
           channels :: [String],
           name :: String,
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
