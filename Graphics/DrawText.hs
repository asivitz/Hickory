{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.DrawText (Printer(..), loadPrinter, pvcShaderPair, createPrinterVAOConfig, PositionedTextCommand(..), textcommand) where

import Types.Color
import Graphics.Drawing
import Graphics.Shader
import Text.Text
import Math.Matrix

import Graphics.Rendering.OpenGL.Raw.Core31
import Data.Text.IO as TextIO
import Textures.Textures
import Control.Monad

data Printer a = Printer (Font a) TexID
               deriving (Show, Eq)

createPrinterVAOConfig :: Shader -> IO VAOConfig
createPrinterVAOConfig shader = do
        vaoConfig <- createVAOConfig shader 
            [VertexGroup [Attachment sp_ATTR_POSITION 3,
                          Attachment sp_ATTR_TEX_COORDS 2,
                          Attachment sp_ATTR_COLOR 4]]
        config' <- indexVAOConfig vaoConfig
        return config'

loadPrinter :: String -> Shader -> String -> IO (Maybe (Printer Int))
loadPrinter resPath shader name = do
        texid <- loadTexture resPath $ name ++ ".png"
        case texid of
            Nothing -> return Nothing
            Just tid -> do
                text <- TextIO.readFile $ resPath ++ "/fonts/" ++ name ++ ".fnt"
                case makeFont text name of
                    Left s -> do
                        print $ "Error: Can't parse font file for " ++ name ++ ".fnt Msg: " ++ s
                        return Nothing
                    Right font -> return $ Just (Printer font tid)

textcommand :: TextCommand
textcommand = TextCommand { 
                          text = "",
                          fontSize = 4,
                          align = AlignCenter,
                          valign = Middle,
                          color = black,
                          leftBump = 0 }

pvcShaderPair = ("PerVertColor.vsh", "PerVertColor.fsh")

deleteIndex :: [a] -> Int -> [a]
deleteIndex [] _ = []
deleteIndex (x:xs) 0 = xs
deleteIndex (x:xs) i = (x:deleteIndex xs (i - 1))

modIndex :: [a] -> Int -> (a -> a) -> [a]
modIndex [] _ _ = []
modIndex (x:xs) 0 f = f x : xs
modIndex (x:xs) i f = x : modIndex xs (i - 1) f

appendToAL :: Eq key => [(key, [a])] -> key -> a -> [(key, [a])]
appendToAL [] key val = [(key, [val])]
appendToAL (x@(k, vals):xs) key val
        | k == key = ((k, val:vals):xs)
        | otherwise = (x:(appendToAL xs key val))

