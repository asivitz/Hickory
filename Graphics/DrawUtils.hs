{-# LANGUAGE NamedFieldPuns #-}

module Graphics.DrawUtils where

import Types.Types

import Control.Monad
import Graphics.GLUtils
import Graphics.Shader
import Math.Matrix
import Math.Vector
import Math.VectorMatrix
import Data.List
import Data.Maybe
import Graphics.DrawText
import Text.Text
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Graphics.Rendering.OpenGL.Raw.Core31

import Graphics.Drawing
import Types.Color

data DrawSpec = Square (Maybe TexID) Shader |
                Text Shader (Printer Int) TextCommand |
                VAO (Maybe TexID) VAOObj
              deriving (Show)

drawSpec :: Mat44 -> Color -> RenderLayer -> DrawSpec -> IO ()
drawSpec mat color layer spec =
        case spec of
            Square tex shader ->
                addDrawCommand mat color color (fromMaybe nullTex tex) shader layer (realToFrac depth) True >> return ()
            Text shader printer _ -> error "Can't print text directly. Should transform into a VAO command."
            VAO tex (VAOObj (VAOConfig vao indexVBO vertices shader) numitems) -> do
                dc <- addDrawCommand mat white white (fromMaybe nullTex tex) shader layer 0.0 True
                vao_payload <- setVAOCommand dc vao numitems gl_TRIANGLE_STRIP
                return ()
    where depth = v4z $ mat44MulVec4 mat (v4 0 0 0 1)

data ParticleShader = ParticleShader Shader UniformLoc

{-
reserveParticleShader :: IORef SysData -> SysMonad c IO (Maybe ParticleShader)
reserveParticleShader  draw = do
        shader <- reserveShader' draw ("ParticleShader.vsh", "ParticleShader.fsh")
        case shader of
            Nothing -> return Nothing
            Just s -> do
                loc <- liftIO $ getUniformLoc s "size"
                return $ Just $ ParticleShader s (UniformLoc loc)
                -}

sizePosMat :: Size Float -> V3 -> Mat44
sizePosMat (Size w h) pos = mat44Scale w h 1 $ mat44TranslateV pos mat44Identity

sizePosRotMat :: Real a => Size Float -> V3 -> a -> Mat44
sizePosRotMat (Size w h) pos rot = mat44Scale w h 1 $ mat44Rotate 0 0 1 (realToFrac rot) $ mat44TranslateV pos mat44Identity

data RenderTree = Primative Mat44 Color DrawSpec
                | List [RenderTree]
                | NoRender
                deriving (Show)

data VAOObj = VAOObj VAOConfig Int
            deriving (Show)

type PrintDesc = (Printer Int, TextCommand, Color, Shader)

data RenderState = RenderState [(Maybe PrintDesc, VAOObj)]

collectPrintDescs :: RenderTree -> [PrintDesc]
collectPrintDescs (List subs) = concatMap collectPrintDescs subs
collectPrintDescs (Primative mat color (Text shader printer text) ) = [(printer, text, color, shader)]
collectPrintDescs _ = []

textToVAO :: [(Maybe PrintDesc, VAOObj)] -> RenderTree -> RenderTree
textToVAO m (List subs) = List $ map (textToVAO m) subs
textToVAO m (Primative mat col (Text sh pr@(Printer font tex) text)) = Primative mat col (VAO (Just tex) (fromJust $ lookup (Just (pr, text, col, sh)) m))
textToVAO m a = a

updateVAOObj :: PrintDesc -> VAOObj -> IO VAOObj
updateVAOObj (Printer font texid, textCommand, color, shader)
             (VAOObj vaoconfig@VAOConfig { vao, indexVBO = Just ivbo, vertices = (vbo:_) } _ ) = do
                 let command = PositionedTextCommand vZero (textCommand { color = color })
                     (numsquares, floats) = transformTextCommandsToVerts [command] font

                 if not $ null floats then do
                        bindVAO vao
                        bufferVertices vbo floats
                        numBlockIndices <- bufferSquareIndices ivbo numsquares
                        unbindVAO

                        return (VAOObj vaoconfig numBlockIndices)
                     else
                         error "Tried to print empty text command"



renderTree :: RenderLayer -> RenderTree -> RenderState -> IO RenderState
renderTree layer tree state = do
        state'@(RenderState vaolst) <- updateRenderState tree state
        let tree' = textToVAO vaolst tree

        drawTree layer tree'

        return state'

updateRenderState :: RenderTree -> RenderState -> IO RenderState
updateRenderState tree (RenderState vaolst) = do
        let texts = collectPrintDescs tree
            existing = mapMaybe fst vaolst
            unused = existing \\ texts
            new = texts \\ existing

            xx :: [(Maybe PrintDesc, VAOObj)] -> [PrintDesc] -> [PrintDesc] -> IO [(Maybe PrintDesc, VAOObj)]
            xx [] newones notneeded = return []
            xx ((desc, vaoobj):ys) newones notneeded =
                case desc of
                    Nothing -> case newones of
                                   (x:xs) -> do
                                       rest <- xx ys xs notneeded
                                       vo <- updateVAOObj x vaoobj
                                       return ((Just x, vo) : rest)
                                   [] -> do
                                       rest <- xx ys [] notneeded
                                       return ((desc, vaoobj) : rest)
                    Just d -> if d `elem` notneeded
                                  then do
                                      rest <- xx ys newones notneeded
                                      return ((Nothing, vaoobj) : rest)
                                  else do
                                      rest <- xx ys newones notneeded
                                      return ((desc, vaoobj) : rest)
        vaolst' <- xx vaolst new unused
        return $ RenderState vaolst'


{-rtDepth :: RenderTree -> Scalar-}
{-rtDepth (RSquare _ (Vector3 _ _ z) _ _ _) = z-}
{-rtDepth _ = 0-}

drawTree :: RenderLayer -> RenderTree -> IO ()
drawTree _ NoRender = return ()
drawTree layer (Primative mat col spec) = drawSpec mat col layer spec
drawTree layer (List children) = mapM_ (drawTree layer) children -- (sortOn rtDepth children)

