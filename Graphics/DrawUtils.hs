{-# LANGUAGE NamedFieldPuns #-}

module Graphics.DrawUtils where

import Types.Types

import Control.Monad
import Graphics.Shader
import Math.Matrix
import Math.Vector
import Math.VectorMatrix
import Data.List
import Data.Maybe
import Graphics.DrawText
import Text.Text
import qualified Data.Text as Text
import GLInterface.GLSupport
import Foreign.C.Types
import Data.Foldable (toList)
import Control.Lens hiding (List)
import Linear.Matrix
import qualified Utils.OBJ as OBJ
import qualified Data.List.Utils as LUtils

import Graphics.Drawing
import Types.Color
import Graphics.GL.Compatibility41 as GL

data Command = Command Shader Mat44 Color DrawSpec

data DrawSpec = Text (Printer Int) TextCommand |
                VAO (Maybe TexID) VAOObj |
                DynVAO (Maybe TexID) VAOConfig ([GLfloat],[GLushort],DrawType)
              deriving (Show)

drawSpec :: Shader -> Mat44 -> Color -> DrawSpec -> IO ()
drawSpec shader mat color spec =
        case spec of
            Text printer _ -> error "Can't print text directly. Should transform into a VAO command."
            VAO tex (VAOObj (VAOConfig vao indexVBO vertices) numitems drawType) -> do
                drawCommand shader mat color color tex vao (fromIntegral numitems) drawType
            DynVAO tex vaoconfig@(VAOConfig vao indexVBO vertices) (verts,indices,drawType) -> do
                loadVerticesIntoVAOConfig vaoconfig verts indices
                drawCommand shader mat color color tex vao (fromIntegral $ length indices) drawType
    where depth = (mat !* v4 0 0 0 1) ^. _z


{-
data ParticleShader = ParticleShader Shader UniformLoc
reserveParticleShader :: IORef SysData -> SysMonad c IO (Maybe ParticleShader)
reserveParticleShader  draw = do
        shader <- reserveShader' draw ("ParticleShader.vsh", "ParticleShader.fsh")
        case shader of
            Nothing -> return Nothing
            Just s -> do
                loc <- liftIO $ getUniformLoc s "size"
                return $ Just $ ParticleShader s (UniformLoc loc)
                -}

sizePosMat :: Size Scalar -> V3 Scalar -> Mat44
sizePosMat (Size w h) pos = mkTranslation pos !*! scaled (V4 w h 1 1)

size3PosMat :: V3 Scalar -> V3 Scalar -> Mat44
size3PosMat (V3 w h d) pos = mkTranslation pos !*! scaled (V4 w h d 1)

sizePosRotMat :: Size Scalar -> V3 Scalar -> Scalar -> Mat44
sizePosRotMat (Size w h) pos rot = mkTranslation pos !*! mkRotation (V3 0 0 1) rot !*! scaled (V4 w h 1 1)

data RenderTree = Primative Shader Mat44 Color DrawSpec
                | List [RenderTree]
                | XForm (Maybe Mat44) RenderTree
                | NoRender
                deriving (Show)

data VAOObj = VAOObj VAOConfig Int DrawType
            deriving (Show)

data PrintDesc = PrintDesc (Printer Int) TextCommand Color
               deriving (Show)

-- Vector equality using (==) doesn't always work on ARM
instance Eq PrintDesc where
        PrintDesc pra tca cola == PrintDesc prb tcb colb = pra == prb && tca == tcb && vnull (cola - colb)

data RenderState = RenderState [(Maybe PrintDesc, VAOObj)]

collectPrintDescs :: RenderTree -> [PrintDesc]
collectPrintDescs (List subs) = concatMap collectPrintDescs subs
collectPrintDescs (Primative shader mat color (Text printer text) ) = [PrintDesc printer text color]
collectPrintDescs (XForm _ child) = collectPrintDescs child
collectPrintDescs _ = []

textToVAO :: [(Maybe PrintDesc, VAOObj)] -> RenderTree -> RenderTree
textToVAO m (List subs) = List $ map (textToVAO m) subs
textToVAO m (XForm mat child) = XForm mat $ textToVAO m child
textToVAO m (Primative sh mat col (Text pr@(Printer font tex) txt)) =
        Primative sh mat col
                    (VAO (Just tex)
                         (fromMaybe (error $ "Can't find vao for text command: " ++ show (pr, txt, col) ++ " in: " ++ show m)
                                    (lookup (Just (PrintDesc pr txt col)) m)))
textToVAO m a = a

updateVAOObj :: PrintDesc -> VAOObj -> IO VAOObj
updateVAOObj (PrintDesc (Printer font texid) textCommand color)
             (VAOObj vaoconfig@VAOConfig { vao, indexVBO = Just ivbo, vertices = (vbo:_) } _ _) = do
                 let command = PositionedTextCommand zero (textCommand { color = color })
                     (numsquares, floats) = transformTextCommandsToVerts [command] font

                 if not $ null floats then do
                        bindVAO vao
                        bufferVertices vbo floats
                        let (indices, numBlockIndices) = squareIndices (fromIntegral numsquares)
                        bufferIndices ivbo indices

                        return (VAOObj vaoconfig (fromIntegral numBlockIndices) TriangleStrip)
                     else
                         error "Tried to print empty text command"

cubeFloats :: [GLfloat]
cubeFloats = concatMap toList verts
    where h = 0.5
          l = -h
          p1 = v3 l l l
          p2 = v3 h l l
          p3 = v3 h h l
          p4 = v3 l h l
          p5 = v3 l l h
          p6 = v3 h l h
          p7 = v3 h h h
          p8 = v3 l h h
          verts = [p1, p2, p3, p4, p5, p6, p7, p8]

loadVerticesIntoVAOConfig :: VAOConfig -> [GLfloat] -> [GLushort] -> IO ()
loadVerticesIntoVAOConfig VAOConfig { vao, indexVBO = Just ivbo, vertices = (vbo:_) } vs indices = do
        bindVAO vao
        bufferVertices vbo vs
        bufferIndices ivbo indices

packVAOObj :: VAOConfig -> [GLfloat] -> [GLushort] -> DrawType -> IO VAOObj
packVAOObj vaoconfig verts indices drawType = do
        loadVerticesIntoVAOConfig vaoconfig verts indices
        return $ VAOObj vaoconfig (length indices) drawType

buildData :: OBJ.OBJ Double -> ([GLfloat], [GLushort])
buildData (OBJ.OBJ vertices texCoords normals faces) = (concat $ LUtils.valuesAL pool, indices)
    where construct (vidx, tcidx, nidx) = map realToFrac $ toList (vertices !! (vidx - 1)) ++ toList (texCoords !! (tcidx - 1)) ++ toList (normals !! (nidx - 1))
          pool = foldl' (\alst e -> LUtils.addToAL alst e (construct e)) [] $ concatMap toList faces
          indices = map (fromIntegral . fromJust . (\e -> findIndex (\(e', _) -> e == e') pool)) $ concatMap toList faces

loadObjIntoVAOConfig :: VAOConfig -> OBJ.OBJ Double -> IO VAOObj
loadObjIntoVAOConfig
    vaoconfig@VAOConfig { vao, indexVBO = Just ivbo, vertices = (vbo:_) }
    obj
    = do
        let (vertexData, indices) = buildData obj
        packVAOObj vaoconfig vertexData indices Triangles

loadCubeIntoVAOConfig :: VAOConfig -> IO VAOObj
loadCubeIntoVAOConfig vaoconfig = do
        let floats = cubeFloats
            indices = [6, 7, 5, 4, 0, 7, 3, 6, 2, 5, 1, 0, 2, 3]
        packVAOObj vaoconfig floats indices TriangleStrip

mkCubeVAOObj :: Shader -> IO VAOObj
mkCubeVAOObj shader = createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 3]] >>= indexVAOConfig >>= loadCubeIntoVAOConfig

mkSquareVerts texW texH = (floats, indices, TriangleFan)
    where h = 0.5
          l = -h
          floats = [l, h, 0, 0,
                    l, l, 0, texH,
                    h, l, texW, texH,
                    h, h, texW, 0]
          indices = [0,1,2,3]

loadSquareIntoVAOConfig :: VAOConfig -> IO VAOObj
loadSquareIntoVAOConfig vaoconfig = packVAOObj vaoconfig floats indices TriangleFan
    where h = 0.5
          l = -h
          floats = [l, h, 0, 0,
                    l, l, 0, 1,
                    h, l, 1, 1,
                    h, h, 1, 0]
          indices = [0,1,2,3]

mkSquareVAOObj :: Shader -> IO VAOObj
mkSquareVAOObj shader = createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 2, Attachment sp_ATTR_TEX_COORDS 2]] >>= indexVAOConfig >>= loadSquareIntoVAOConfig

loadUntexturedSquareIntoVAOConfig :: VAOConfig -> IO VAOObj
loadUntexturedSquareIntoVAOConfig vaoconfig = do
        let h = 0.5
            l = -h
            floats = [l, h,
                      l, l,
                      h, l,
                      h, h]
            indices = [0,1,2,3]

        loadVerticesIntoVAOConfig vaoconfig floats indices

        return (VAOObj vaoconfig (length indices) TriangleFan)

mkUntexturedSquareVAOObj :: Shader -> IO VAOObj
mkUntexturedSquareVAOObj shader = createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 2]] >>= indexVAOConfig >>= loadUntexturedSquareIntoVAOConfig

renderTree :: Mat44 -> RenderTree -> RenderState -> IO RenderState
renderTree viewmat tree state = do
        state'@(RenderState vaolst) <- updateRenderState tree state
        let tree' = textToVAO vaolst tree

        drawTree viewmat tree'

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

runDrawCommand (Command sh mat col spec) = drawSpec sh mat col spec

{-rtDepth :: RenderTree -> Scalar-}
{-rtDepth (RSquare _ (Vector3 _ _ z) _ _ _) = z-}
{-rtDepth _ = 0-}
drawTree :: M44 Scalar -> RenderTree -> IO ()
drawTree mat tree = mapM_ runDrawCommand (reverse commands)
        where commands = collectTreeSpecs mat tree
              {-sorted = sortOn (\(Command _ m _ _) -> - (m !* V4 0 0 0 1) ^. _z) commands-}

collectTreeSpecs :: M44 Scalar -> RenderTree -> [Command]
collectTreeSpecs _ NoRender = []
collectTreeSpecs parentMat (XForm mat child) = collectTreeSpecs mat' child
    where mat' = case mat of
                     Just n -> parentMat !*! n
                     Nothing -> parentMat

collectTreeSpecs parentMat (Primative sh mat col spec) = [Command sh mat' col spec]
    where mat' = parentMat !*! mat
collectTreeSpecs parentMat (List children) = concatMap (collectTreeSpecs parentMat) children -- (sortOn rtDepth children)

