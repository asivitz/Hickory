{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Systems.DrawText (SysData(..), make, empty, drawText, PositionedTextCommand(..), textcommand, releasePrinter) where
import Control.Monad.State

import Engine.System
import Engine.World

import Types.Color
import Utils.Resources
import Utils.Utils
import Data.IORef
import Graphics.GLUtils
import Graphics.Drawing
import Math.Matrix

import Graphics.DrawText

import Graphics.Rendering.OpenGL.Raw.Core31
import Data.Text.IO as TextIO

data SysData = SysData { 
             printerids :: RefStore String PrinterID,
             printerpairs :: [(Printer Int, [(Label, [PositionedTextCommand])])],
             perVertColorShader :: Maybe Shader
             }

data Printer a = Printer (Font a) TexID VAOConfig

empty = SysData { printerids = emptyRefStore, printerpairs = [], perVertColorShader = Nothing }

reservePrinter' :: IORef SysData -> String -> SysMonad c IO (Maybe PrinterID)
reservePrinter' drawtext name = do
        SysData { printerids } <- getSysData drawtext
        (newprinterids, pid) <- reserve printerids name (loadPrinterID drawtext)
        whenNothing pid $ liftIO $ print ("Couldn't load printer: " ++ name)
        sd <- getSysData drawtext {- re-get it bc printer has been added to command pairs -}
        putSysData drawtext sd { printerids = newprinterids }
        return pid

releasePrinter :: IORef SysData -> String -> SysMonad c IO ()
releasePrinter drawtext name = do
        sd@SysData { printerids } <- getSysData drawtext
        newprinterids <- release printerids name (unloadPrinter drawtext (name ++ ".png"))
        putSysData drawtext sd { printerids = newprinterids }

{-empty = SysData { screenSize = (Size 0 0), window = fromC nullPtr }-}

make drawtext = System (run drawtext) (initS drawtext)

createPrinterVAOConfig :: Shader -> IO VAOConfig
createPrinterVAOConfig shader = do
        vaoConfig <- createVAOConfig shader 
            [(VertexGroup [(Attachment sp_ATTR_POSITION 3),
                           (Attachment sp_ATTR_TEX_COORDS 2),
                           (Attachment sp_ATTR_COLOR 4)])]
        config' <- indexVAOConfig vaoConfig
        return config'

loadPrinterID :: IORef SysData -> String -> SysMonad c IO (Maybe PrinterID)
loadPrinterID drawtext name = do
        sd@SysData { printerpairs, perVertColorShader } <- getSysData drawtext
        case perVertColorShader of
            Nothing -> liftIO $ print "Can't load printer: No shader" >> return Nothing
            Just pvcShader -> do
                mprinter <- loadPrinter pvcShader name
                case mprinter of
                    Nothing -> liftIO $ print "Load printer failed" >> return Nothing
                    Just printer -> do
                        let printerpairs' = (printerpairs ++ [(printer, [])])
                        putSysData drawtext sd { printerpairs = printerpairs' }
                        return $ Just $ length printerpairs

loadPrinter :: Shader -> String -> SysMonad c IO (Maybe (Printer Int))
loadPrinter shader name = do
        RPC { _reserveTex } <- getRPC
        texid <- _reserveTex $ name ++ ".png"
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

unloadPrinter :: IORef SysData -> String -> PrinterID -> SysMonad c IO ()
unloadPrinter dt path pid = do
        RPC { _releaseTex } <- getRPC
        _releaseTex path
        sd@SysData { printerpairs } <- getSysData dt
        putSysData dt sd { printerpairs = deleteIndex printerpairs pid }


appendToAL :: Eq key => [(key, [a])] -> key -> a -> [(key, [a])]
appendToAL [] key val = [(key, [val])]
appendToAL (x@(k, vals):xs) key val
        | k == key = ((k, val:vals):xs)
        | otherwise = (x:(appendToAL xs key val))

drawText' :: IORef SysData -> PrinterID -> Label -> PositionedTextCommand -> SysMonad c IO ()
drawText' dt pid label command = do
        sd@SysData { printerpairs } <- getSysData dt
        let printerpairs' = modIndex printerpairs pid (\(printer, labellst) -> (printer, appendToAL labellst label command))
        putSysData dt sd { printerpairs = printerpairs' }

textcommand :: TextCommand
textcommand = TextCommand { 
                          text = "",
                          fontSize = 4,
                          align = AlignCenter,
                          valign = Middle,
                          color = black,
                          leftBump = 0 }

run drawtext delta = do
        sd@SysData { printerpairs, perVertColorShader } <- getSysData drawtext
        whenMaybe perVertColorShader $ \pvc -> do
            liftIO $ renderTextCommands pvc printerpairs
        putSysData drawtext sd { printerpairs = map (\(printer, tcoms) -> (printer, [])) printerpairs }

initS drawtext = do
        RPC { _reserveShader } <- getRPC
        registerResource reservePrinter (reservePrinter' drawtext)
        registerResource drawText (drawText' drawtext)
        sd <- getSysData drawtext
        shader <- _reserveShader ("perVertColor.vsh", "perVertColor.fsh")
        putSysData drawtext sd { perVertColorShader = shader }

renderTextCommands shader printerPairs =
        mapM_ (\(printer, labellst) -> mapM_ (\(label, commands) ->
            printCommands shader label printer commands) labellst)
            printerPairs

printCommands :: Real a => Shader -> Label -> Printer a -> [PositionedTextCommand] -> IO ()
printCommands _ _ _ [] = return ()
printCommands shader label (Printer font texid VAOConfig { vao, indexVBO = Just ivbo, vertices = (vbo:_) } ) commands = do
        let squarelists = transformTextCommandsToVerts commands font
            numsquares = length squarelists
            floats = map realToFrac (foldl (++) [] squarelists)

        bindVAO vao
        bufferVertices vbo floats
        numBlockIndices <- bufferSquareIndices ivbo numsquares
        unbindVAO

        dc <- addDrawCommand mat44Identity white white texid shader label 0.0 True
        vao_payload <- setVAOCommand dc vao numBlockIndices gl_TRIANGLE_STRIP
        return ()
printCommands _ _ _ _ = return ()
