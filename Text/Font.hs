{-# LANGUAGE OverloadedStrings #-}
module Text.Font where

import Data.Attoparsec.Text
{-import Data.Attoparsec.Char8-}
import Data.Hashable
import Control.Applicative
import qualified Data.Text as Text

data FontInfo a = FontInfo {
              lineHeight :: a,
              base :: a,
              scaleW :: a,
              scaleH :: a } deriving Show

emptyFontInfo :: Integral a => FontInfo a
emptyFontInfo = (FontInfo 0 0 0 0)

data KerningPair = KerningPair CharIdent CharIdent deriving (Show, Eq)

instance Data.Hashable.Hashable KerningPair where
      hashWithSalt s (KerningPair (CharIdent a) (CharIdent b)) = hashWithSalt s ((a * 1000) + b)

newtype CharIdent = CharIdent Int deriving (Eq, Show)

instance Data.Hashable.Hashable CharIdent where
      hashWithSalt s (CharIdent a) = hashWithSalt s a
        
data GlyphSpec a = GlyphSpec {
                ident :: CharIdent,
                x :: a,
                y :: a,
                width :: a,
                height :: a,
                xoffset :: a,
                yoffset :: a,
                xadvance :: a } deriving (Show)

data KerningSpec a = KerningSpec KerningPair a deriving Show

-- common lineHeight=117 base=95 scaleW=512 scaleH=512 pages=1 packed=0

isInlineSpace c =  
        c == ' '     ||
        c == '\t'

inlineSpace :: Parser Char
inlineSpace = satisfy isInlineSpace <?> "inlineSpace"

quotedString :: Parser Text.Text
quotedString = do
        char '"'
        prop <- takeTill (== '"')
        char '"'
        return prop

data Value a = StringVal String | CommaSepValue [a] deriving Show

stringVal :: Parser (Value a)
stringVal = do
        val <- quotedString
        return $ StringVal (Text.unpack val)

commaSepVal :: Integral a => Parser (Value a)
commaSepVal = do
        lst <- (signed decimal) `sepBy` (char ',')
        return $ CommaSepValue lst

parseProperty :: Integral a => Parser (String, Value a)
parseProperty = do
        key <- many' letter
        char '='
        val <- (stringVal <|> commaSepVal)
        return (key, val)

data CommandResult a = FontInfoResult (FontInfo a)
                   | GlyphResult (GlyphSpec a)
                   | KerningResult (KerningSpec a)
                   | Unused

toNum :: Integral a => Value a -> a
toNum (CommaSepValue (i:is)) = i
toNum _ = 0

pullNumVal :: (Integral a) => String -> [(String, Value a)] -> a
pullNumVal key alist =
        maybe 0 toNum $ lookup key alist

interpretCommand :: (Integral a) => (String, [(String, Value a)]) -> CommandResult a
interpretCommand ("common", alist) =
        FontInfoResult $ FontInfo {
                                  lineHeight = pullNumVal "lineHeight" alist,
                                  base = pullNumVal "base" alist,
                                  scaleW = pullNumVal "scaleW" alist,
                                  scaleH = pullNumVal "scaleH" alist} 

interpretCommand ("char", alist) =
        let pullval = (\k -> pullNumVal k alist) in
            GlyphResult $ GlyphSpec {
                        ident = (CharIdent $ fromIntegral $ pullval "id"),
                        x = pullval "x",
                        y = pullval "y",
                        width = pullval "width",
                        height = pullval "height",
                        xoffset = pullval "xoffset",
                        yoffset = pullval "yoffset",
                        xadvance = pullval "xadvance"
                        }

interpretCommand ("kerning", alist) =
        let pullval = (\k -> pullNumVal k alist) in
            KerningResult $ KerningSpec (KerningPair (CharIdent $ fromIntegral $ pullval "first") (CharIdent $ fromIntegral $ pullval "second")) (pullval "amount")

interpretCommand _ = Unused

parseCommand :: Integral a => Parser (String, [(String, Value a)])
parseCommand = do
        name <- many' letter
        inlineSpace
        alist <- parseProperty `sepBy` inlineSpace
        return (name, alist)

parseFont :: Integral a => Parser ((FontInfo a), [GlyphSpec a], [KerningSpec a])
parseFont = do
        commands <- parseCommand `sepBy` endOfLine
        let commands' = map interpretCommand commands
        return $ foldl (\(fi, chars, kernings) res -> case res of
            FontInfoResult fi' -> (fi', chars, kernings)
            GlyphResult g -> (fi, g:chars, kernings)
            KerningResult k -> (fi, chars, k:kernings)
            Unused -> (fi, chars, kernings)
            )
            (emptyFontInfo, [], [])
            commands'

runParseFont :: Integral a => Text.Text -> Either String ((FontInfo a), [GlyphSpec a], [KerningSpec a])
runParseFont = Data.Attoparsec.Text.parseOnly parseFont
