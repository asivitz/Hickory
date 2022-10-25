{-# LANGUAGE OverloadedStrings #-}

module Hickory.Text.ParseFnt where

import Data.Attoparsec.Text
import Control.Applicative
import qualified Data.Text as Text
import Hickory.Text.Types (FontInfo (..), GlyphSpec (..), KerningSpec (..), CharIdent (..), emptyFontInfo, KerningPair (..), Font (..), makeCharTable, makeKerningTable)
import Data.Foldable (foldl')


-- common lineHeight=117 base=95 scaleW=512 scaleH=512 pages=1 packed=0

isInlineSpace :: Char -> Bool
isInlineSpace c
   = c == ' '
  || c == '\t'

inlineSpace :: Parser Char
inlineSpace = satisfy isInlineSpace <?> "inlineSpace"

quotedString :: Parser Text.Text
quotedString = do
  _ <- char '"'
  prop <- takeTill (== '"')
  _ <- char '"'
  return prop

data Value a
  = StringVal String
  | CommaSepValue [a]
  deriving Show

stringVal :: Parser (Value a)
stringVal =
  StringVal . Text.unpack <$> quotedString

commaSepVal :: Integral a => Parser (Value a)
commaSepVal = do
  lst <- signed decimal `sepBy` char ','
  return $ CommaSepValue lst

parseProperty :: Integral a => Parser (String, Value a)
parseProperty = do
  key <- many' letter
  char '='
  val <- stringVal <|> commaSepVal
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
  FontInfoResult $
    FontInfo
      { lineHeight = pullNumVal "lineHeight" alist
      , base = pullNumVal "base" alist
      , scaleW = pullNumVal "scaleW" alist
      , scaleH = pullNumVal "scaleH" alist
      }

interpretCommand ("char", alist) =
        let pullval = (\k -> pullNumVal k alist) in
            GlyphResult $ GlyphSpec {
                        ident = CharIdent $ fromIntegral $ pullval "id",
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
  _ <- inlineSpace
  alist <- parseProperty `sepBy` inlineSpace
  return (name, alist)

parseFont :: Integral a => Parser (FontInfo a, [GlyphSpec a], [KerningSpec a])
parseFont = do
        commands <- parseCommand `sepBy` endOfLine
        let commands' = map interpretCommand commands
        return $ foldl' (\(fi, chars, kernings) res -> case res of
            FontInfoResult fi' -> (fi', chars, kernings)
            GlyphResult g -> (fi, g:chars, kernings)
            KerningResult k -> (fi, chars, k:kernings)
            Unused -> (fi, chars, kernings)
            )
            (emptyFontInfo, [], [])
            commands'

runParseFont :: Integral a => Text.Text -> Either String (FontInfo a, [GlyphSpec a], [KerningSpec a])
runParseFont = Data.Attoparsec.Text.parseOnly parseFont

makeFont :: Integral a => Text.Text -> String -> Either String (Font a)
makeFont text name = case runParseFont text of
                    Right (info, glyphspecs, kernings) ->
                        Right $ Font name info (makeCharTable info glyphspecs) (makeKerningTable kernings)
                    Left s -> Left s
