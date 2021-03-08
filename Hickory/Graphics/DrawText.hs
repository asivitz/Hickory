{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hickory.Graphics.DrawText (Printer(..), loadPrinter, createPrinterVAOConfig, PositionedTextCommand(..), textcommand) where

import Hickory.Color
import Hickory.Graphics.GLSupport
import Hickory.Graphics.Drawing
import Hickory.Text.Text
import Hickory.Graphics.VAO (VAOConfig, createVAOConfig)

import Hickory.Utils.Utils
import Hickory.Graphics.Textures
import Graphics.GL.Compatibility41 as GL

data Printer a = Printer (Font a) TexID

instance Eq (Printer a) where
        Printer fa tid == Printer fb tidb = fontName fa == fontName fb

instance Show (Printer a) where
        show (Printer font tid) = "Printer:" ++ fontName font ++ "/" ++ show tid

createPrinterVAOConfig :: Shader -> IO VAOConfig
createPrinterVAOConfig shader = createVAOConfig shader
            [VertexGroup [Attachment sp_ATTR_POSITION 3,
                          Attachment sp_ATTR_TEX_COORDS 2,
                          Attachment sp_ATTR_COLOR 4]]

loadPrinter :: String -> Shader -> String -> IO (Maybe (Printer Int))
loadPrinter resPath shader name = do
        texid <- loadTexture resPath (name ++ ".png") GL_REPEAT
        case texid of
            Nothing -> return Nothing
            Just tid -> do
                text <- readFileAsText $ resPath ++ "/fonts/" ++ name ++ ".fnt"
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
