{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hickory.Graphics.DrawText
  ( Printer(..)
  , loadPrinter
  , createPrinterVAOConfig
  , PositionedTextCommand(..)
  , textcommand
  , printVAOObj
  ) where

import Hickory.Color
import Hickory.Graphics.GLSupport
import Hickory.Graphics.Drawing
import Hickory.Text.Text
import Hickory.Graphics.VAO (VAOConfig, createVAOConfig, VAOObj(..), loadVerticesIntoVAOConfig)

import Hickory.Utils.Utils
import Hickory.Graphics.Textures
import Graphics.GL.Compatibility41 as GL
import Linear (zero)

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
textcommand = TextCommand
  { text = ""
  , fontSize = 4
  , align = AlignCenter
  , valign = Middle
  , color = black
  , leftBump = 0
  }

printVAOObj :: Printer Int -> TextCommand -> VAOConfig -> IO VAOObj
printVAOObj (Printer font _) textCommand vaoconfig = do
  let command              = PositionedTextCommand zero textCommand
      (numsquares, floats) = transformTextCommandsToVerts [command] font

  if not $ null floats
    then do
      let (indices, numBlockIndices) = squareIndices (fromIntegral numsquares)
      loadVerticesIntoVAOConfig vaoconfig floats indices

      return (VAOObj vaoconfig (fromIntegral numBlockIndices) TriangleStrip)
    else error "Tried to print empty text command"
