{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.BVH where

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Prim
import Data.Maybe

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

anyNumber = do
        n <- L.number
        return $ case n of
                      Left i -> realToFrac i
                      Right d -> d

fraction :: MonadParsec s m Char => m String
fraction = do
  void (C.char '.')
  d <- some C.digitChar
  return ('.' : d)

floating :: MonadParsec s m Char => m Double
floating = label "floating" (read <$> f)
        where front = do
                chars <- many C.digitChar
                return $ if null chars then "0" else chars
              f = (++) <$> front <*> option " " fraction

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
        name <- identifier
        (off, chans, jnts) <- bracket $ do
            offset <- parseOffset
            chans <- parseChannels
            joints <- many ((reserved "JOINT" *> parseJointBody) <|> parseEndSite)
            return (offset, chans, joints)
        return $ Joint chans name off jnts

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
