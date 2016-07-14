{-# LANGUAGE OverloadedStrings #-}

module Utils.BVH where

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

bracket :: Parser a -> Parser a
bracket = between (symbol "{") (symbol "}")

anyNumber = do
        n <- lexeme L.number
        return $ case n of
                      Left i -> realToFrac i
                      Right d -> d

number :: Parser Double
number = L.signed sc anyNumber

integer :: Parser Integer
integer = lexeme L.integer

parseAnimation :: Parser Animation
parseAnimation = Animation <$> (reserved "HIERARCHY" *> parseRoot)

parseOffset = reserved "OFFSET" *>
    ((,,) <$> number <*> number <*> number)

parseEndSite = reserved "End Site" *>
        (JointEnd <$> bracket parseOffset)

parse6Channels = do
        reserved "CHANNELS"
        symbol "6"
        identifier *> identifier *> identifier *> identifier *> identifier *> identifier
        return ()

parse3Channels = do
        reserved "CHANNELS"
        symbol "3"
        identifier *> identifier *> identifier
        return ()

parseRoot = do
        reserved "ROOT"
        name <- identifier
        (off, jnts) <- bracket $ do
            offset <- parseOffset
            parse6Channels
            joints <- many (parseJoint <|> parseEndSite)
            return (offset, joints)
        return $ Joint name off jnts

parseJoint = do
        reserved "JOINT"
        name <- identifier
        (off, jnts) <- bracket $ do
            offset <- parseOffset
            parse3Channels
            joints <- many (parseJoint <|> parseEndSite)
            return (offset, joints)
        return $ Joint name off jnts

data Animation = Animation Joint
               deriving (Show)

type Offset = (Double, Double, Double)

data Joint = Joint String Offset [Joint]
           | JointEnd Offset
           deriving (Show)

loadBVH :: String -> IO Animation
loadBVH filePath = do
        res <- parseFromFile parseAnimation filePath
        case res of
            Left err -> error (show err)
            Right anim -> return anim
