{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systems.DrawText (SysData(..), make, empty, drawText, textcommand, reservePrinter, releasePrinter) where
import Control.Monad.State

import Engine.System
import Engine.Event

import Types.Color
import Utils.Resources
import Utils.Utils
import Data.IORef
import Graphics.GLUtils
import Graphics.Drawing
import Math.Matrix
import Math.Vector

import Graphics.DrawText
import qualified Systems.Textures as Textures
import qualified Systems.Draw as Draw

import Graphics.Rendering.OpenGL.Raw.Core31
import Data.Text.IO as TextIO

data SysData = SysData { 
             printerids :: RefStore String Int,
             commands :: [(Printer Int, [TextCommand])],
             perVertColorShader :: Maybe Shader
             }

data Printer a = Printer (Font a) TexID VAOConfig

empty = SysData { printerids = emptyRefStore, commands = [], perVertColorShader = Nothing }

reservePrinter :: IORef SysData -> IORef Textures.SysData -> String -> SysMonad IO (Maybe Int)
reservePrinter drawtext texes name = do
        SysData { printerids } <- getSysData drawtext
        (newprinterids, pid) <- reserve printerids name (loadPrinterID drawtext texes)
        sd <- getSysData drawtext {- re-get it bc printer has been added to command pairs -}
        putSysData drawtext sd { printerids = newprinterids }
        return pid

releasePrinter :: IORef SysData -> IORef Textures.SysData -> String -> SysMonad IO ()
releasePrinter drawtext texes name = do
        sd@SysData { printerids } <- getSysData drawtext
        newprinterids <- release printerids name (unloadPrinter drawtext texes (name ++ ".png"))
        putSysData drawtext sd { printerids = newprinterids }

{-empty = SysData { screenSize = (Size 0 0), window = fromC nullPtr }-}

make drawtext draw = System (run drawtext) nullHandleEvent (initS drawtext draw)

createPrinterVAOConfig :: Shader -> IO VAOConfig
createPrinterVAOConfig shader = do
        vaoConfig <- createVAOConfig shader 
            [(VertexGroup [(Attachment sp_ATTR_POSITION 2),
                           (Attachment sp_ATTR_TEX_COORDS 2),
                           (Attachment sp_ATTR_COLOR 4)])]
        config' <- indexVAOConfig vaoConfig
        return config'

loadPrinterID :: IORef SysData -> IORef Textures.SysData -> String -> SysMonad IO (Maybe Int)
loadPrinterID drawtext texes name = do
        sd@SysData { commands, perVertColorShader } <- getSysData drawtext
        case perVertColorShader of
            Nothing -> return Nothing
            Just pvcShader -> do
                mprinter <- loadPrinter texes pvcShader name
                case mprinter of
                    Nothing -> return Nothing
                    Just printer -> do
                        let commands' = (commands ++ [(printer, [])])
                        putSysData drawtext sd { commands = commands' }
                        return $ Just $ length commands

loadPrinter :: IORef Textures.SysData -> Shader -> String -> SysMonad IO (Maybe (Printer Int))
loadPrinter texes shader name = do
        texid <- Textures.reserveTex texes $ name ++ ".png"
        case texid of
            Nothing -> return Nothing
            Just tid -> do
                text <- liftIO $ TextIO.readFile $ "fonts/" ++ name ++ ".fnt"
                let f = makeFont text
                case f of
                    Left s -> do
                        liftIO $ print $ "Error: Can't parse font file for " ++ name ++ ".fnt Msg: " ++ s
                        return Nothing
                    Right font -> do
                        vaoconfig <- liftIO $ createPrinterVAOConfig shader
                        return $ Just (Printer font tid vaoconfig)

deleteIndex :: [a] -> Int -> [a]
deleteIndex [] _ = []
deleteIndex (x:xs) 0 = xs
deleteIndex (x:xs) i = (x:deleteIndex xs (i - 1))

modIndex :: [a] -> Int -> (a -> a) -> [a]
modIndex [] _ _ = []
modIndex (x:xs) 0 f = f x : xs
modIndex (x:xs) i f = x : modIndex xs (i - 1) f

unloadPrinter :: IORef SysData -> IORef Textures.SysData -> String -> Int -> SysMonad IO ()
unloadPrinter dt texes path pid = do
        Textures.releaseTex texes path
        sd@SysData { commands } <- getSysData dt
        putSysData dt sd { commands = deleteIndex commands pid }

drawText dt pid command = do
        sd@SysData { commands } <- getSysData dt
        let commands' = modIndex commands pid (\(printer, clst) -> (printer, (command:clst)))
        putSysData dt sd { commands = commands' }

textcommand :: TextCommand
textcommand = TextCommand { 
                          text = "",
                          pos = (V2 0 0),
                          fontSize = 12,
                          align = AlignCenter,
                          valign = Middle,
                          color = black,
                          leftBump = 0 }

run drawtext delta = do
        sd@SysData { commands, perVertColorShader } <- getSysData drawtext
        whenMaybe perVertColorShader $ \pvc -> do
            liftIO $ renderTextCommands pvc commands
        putSysData drawtext sd { commands = map (\(printer, tcoms) -> (printer, [])) commands }

initS drawtext draw = do
        sd <- getSysData drawtext
        shader <- Draw.reserveShader draw ("perVertColor.vsh", "perVertColor.fsh")
        putSysData drawtext sd { perVertColorShader = shader }

renderTextCommands shader printerPairs =
        mapM_ (\(printer, commands) -> printCommands shader printer commands) printerPairs

printCommands :: Real a => Shader -> Printer a -> [TextCommand] -> IO ()
printCommands _ _ [] = return ()
printCommands shader (Printer font texid VAOConfig { vao, indexVBO = Just ivbo, vertices = (vbo:_) } ) commands = do
        let squarelists = transformTextCommandsToVerts commands font
            numsquares = length squarelists
            floats = map realToFrac (foldl (++) [] squarelists)

        bindVAO vao
        bufferVertices vbo floats
        numBlockIndices <- bufferSquareIndices ivbo numsquares
        unbindVAO

        dc <- addDrawCommand mat44Identity white white texid shader worldLabel 0.0 True
        vao_payload <- setVAOCommand dc vao numBlockIndices gl_TRIANGLE_STRIP
        return ()
printCommands _ _ _ = return ()
