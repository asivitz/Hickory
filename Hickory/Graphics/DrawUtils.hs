{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Hickory.Graphics.DrawUtils where

import Hickory.Types

import Hickory.Math.Matrix
import Hickory.Math.Vector
import Data.List
import Data.Maybe
import Hickory.Graphics.DrawText
import Hickory.Text.Text
import Hickory.Graphics.GLSupport
import Data.Foldable (toList)
import qualified Hickory.Utils.OBJ as OBJ
import qualified Hickory.Utils.DirectX as DX
import qualified Data.List.Utils as LUtils
import Linear.Matrix

import Hickory.Graphics.Drawing
import Hickory.Color
import Graphics.GL.Compatibility41 as GL
import qualified Data.Vector.Unboxed as Vector

data Command = Command Shader Mat44 Color DrawSpec

data DrawSpec = Text (Printer Int) TextCommand |
                VAO (Maybe TexID) VAOObj |
                Animated AnimatedModel String Scalar |
                DynVAO (Maybe TexID) VAOConfig ([GLfloat],[GLushort],DrawType)
              deriving (Show)


retrieveActionMat :: (String, Double) -> [(String, [Mat44])] -> Maybe Mat44
retrieveActionMat (actionName, time) actionMats = (\kf -> kf !! (floor (time * 60) `mod` length kf)) <$> keyFrames
        where keyFrames = lookup actionName actionMats

buildAnimatedMats :: Mat44 -> Mat44 -> (String, Double) -> DX.Frame -> [(String, Mat44)]
buildAnimatedMats bindParent animParent animSel DX.Frame { DX.frameName, DX.children, DX.actionMats, DX.mat }
    = (frameName, m) : concatMap (buildAnimatedMats bindPose animMat animSel) children
        where m = animMat !*! inv44 bindPose
              bindPose = bindParent !*! mat
              animMat = animParent !*! actionMat
              actionMat = fromMaybe mat $ retrieveActionMat animSel actionMats

animatedMats :: DX.Frame -> (String, Double) -> [Mat44]
animatedMats f animSel = (sortem . buildAnimatedMats identity identity animSel) f
        where sortem :: [(String, Mat44)] -> [Mat44]
              sortem pairs = let DX.Mesh { DX.skinWeights } = pullMesh f in
                  reverse $ foldl' (\lst sw -> maybe lst (\x -> snd x : lst) $ find (\(name, mat) -> name == DX.transformNodeName sw) pairs) [] skinWeights

drawSpec :: Shader -> Mat44 -> Color -> DrawSpec -> IO ()
drawSpec shader mat color spec =
        case spec of
            Text printer _ -> error "Can't print text directly. Should transform into a VAO command."
            Animated (AnimatedModel (VAOObj vaoConfig numitems drawType) frame) actionName time -> do
                drawCommand shader
                            [UniformBinding "modelMat" (MatrixUniform [mat]),
                             UniformBinding "colors" (QuadFUniform [rgb 0 0 0, rgb 0.9 0.9 0.9, rgb 0 0 0.4, rgb 0.3 0 0, rgb 0.9 0.9 0.9, rgb 0.9 0.9 0.9]),
                             -- TODO: Variable FPS
                             UniformBinding "boneMat" (MatrixUniform (animatedMats frame (actionName, time)))
                             ]
                            Nothing vaoConfig (fromIntegral numitems) drawType
            VAO tex (VAOObj vaoConfig numitems drawType) -> do
                drawCommand shader
                            [UniformBinding "modelMat" (MatrixUniform [mat]),
                             UniformBinding "color" (QuadFUniform [color])
                             ]
                            tex vaoConfig (fromIntegral numitems) drawType
            DynVAO tex vaoConfig (verts,indices,drawType) -> do
                loadVerticesIntoVAOConfig vaoConfig verts indices
                drawCommand shader
                            [UniformBinding "modelMat" (MatrixUniform [mat]), UniformBinding "color" (QuadFUniform [color])]
                            tex vaoConfig (fromIntegral $ length indices) drawType
    {-where depth = (mat !* v4 0 0 0 1) ^. _z-}


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

size3PosRotMat :: V3 Scalar -> V3 Scalar -> Scalar -> Mat44
size3PosRotMat (V3 w h d) pos rot = mkTranslation pos !*! mkRotation (V3 0 0 1) rot !*! scaled (V4 w h d 1)

data RenderTree = Primative Shader Mat44 Color DrawSpec
                | List [RenderTree]
                | XForm (Maybe Mat44) RenderTree
                | NoRender
                deriving (Show)

data VAOObj = VAOObj {
            vaoConfig :: VAOConfig,
            count :: Int,
            drawType :: DrawType
            }
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
             (VAOObj vaoconfig _ _) = do
                 let command = PositionedTextCommand zero (textCommand { color = color })
                     (numsquares, floats) = transformTextCommandsToVerts [command] font

                 if not $ null floats then do
                        let (indices, numBlockIndices) = squareIndices (fromIntegral numsquares)
                        loadVerticesIntoVAOConfig vaoconfig floats indices

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

loadVAOObj :: VAOConfig -> DrawType -> ([GLfloat], [GLushort]) -> IO VAOObj
loadVAOObj vaoconfig drawType (verts, indices) = do
        loadVerticesIntoVAOConfig vaoconfig verts indices
        return $ VAOObj vaoconfig (length indices) drawType

loadCubeIntoVAOConfig :: VAOConfig -> IO VAOObj
loadCubeIntoVAOConfig vaoconfig = do
        let floats = cubeFloats
            indices = [6, 7, 5, 4, 0, 7, 3, 6, 2, 5, 1, 0, 2, 3]
        loadVAOObj vaoconfig TriangleStrip (floats, indices)

mkCubeVAOObj :: Shader -> IO VAOObj
mkCubeVAOObj shader = createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 3]] >>= loadCubeIntoVAOConfig

mkSquareVerts texW texH = (floats, indices, TriangleFan)
    where h = 0.5
          l = -h
          floats = [l, h, 0, 0,
                    l, l, 0, texH,
                    h, l, texW, texH,
                    h, h, texW, 0]
          indices = [0,1,2,3]

loadSquareIntoVAOConfig :: VAOConfig -> IO VAOObj
loadSquareIntoVAOConfig vaoconfig = loadVAOObj vaoconfig TriangleFan (floats, indices)
    where h = 0.5
          l = -h
          floats = [l, h, 0, 0,
                    l, l, 0, 1,
                    h, l, 1, 1,
                    h, h, 1, 0]
          indices = [0,1,2,3]

mkSquareVAOObj :: Shader -> IO VAOObj
mkSquareVAOObj shader = createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 2, Attachment sp_ATTR_TEX_COORDS 2]] >>= loadSquareIntoVAOConfig

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
mkUntexturedSquareVAOObj shader = createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 2]] >>= loadUntexturedSquareIntoVAOConfig

-- Model Loading

-- .OBJ

-- Packs an OBJ's data into an array of vertices and indices
-- The vertices are position, tex coords, and normals packed together
packOBJ :: OBJ.OBJ Double -> ([GLfloat], [GLushort])
packOBJ (OBJ.OBJ vertices texCoords normals faces) = (concat $ LUtils.valuesAL pool, indices)
    where construct (vidx, tcidx, nidx) = map realToFrac $ toList (vertices !! (vidx - 1)) ++ toList (texCoords !! (tcidx - 1)) ++ toList (normals !! (nidx - 1))
          pool = foldl' (\alst e -> LUtils.addToAL alst e (construct e)) [] $ concatMap toList faces
          indices = map (fromIntegral . fromJust . (\e -> findIndex (\(e', _) -> e == e') pool)) $ concatMap toList faces

createVAOConfigFromOBJ :: Shader -> String -> IO VAOObj
createVAOConfigFromOBJ shader path = do
        obj <- OBJ.loadOBJ path
        createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 3, Attachment sp_ATTR_TEX_COORDS 2, Attachment sp_ATTR_NORMALS 3]]
            >>= \config -> loadVAOObj config Triangles (packOBJ obj)

-- interleaves arrays based on an array of counts
-- interleave [[1,2,3,4,5,6],[40,50,60]] [2,1] ~> [1,2,40,3,4,50,5,6,60]
interleave :: [[a]] -> [Int] -> [a]
interleave vals counts | any null vals = []
interleave vals counts = pull counts vals ++ interleave (sub counts vals) counts
    where pull ns vs = concatMap (\(n,v) -> take n v) (zip ns vs)
          sub ns vs = map (\(n,v) -> drop n v) (zip ns vs)

data AnimatedModel = AnimatedModel VAOObj DX.Frame
            deriving (Show)

-- .X
pullMesh :: DX.Frame -> DX.Mesh
pullMesh x = case go x of
                 Nothing -> error "Could not grab mesh from X structure"
                 Just (m@DX.Mesh { DX.skinWeights }) -> m { DX.skinWeights = sortWeights skinWeights x }
    where go (DX.Frame name mat children Nothing _) = listToMaybe $ mapMaybe go children
          go (DX.Frame name mat _ (Just mesh) _) = Just mesh
          -- Put weights, and therefore bones, in order from root to leaf
          sortWeights :: [DX.SkinWeights] -> DX.Frame -> [DX.SkinWeights]
          sortWeights weights (DX.Frame name _ children _ _) = case find (\DX.SkinWeights { DX.transformNodeName } -> transformNodeName == name) weights of
                                                                        Just w -> [w] ++ concatMap (sortWeights weights) children
                                                                        Nothing -> concatMap (sortWeights weights) children

-- Provide a material index for each vertex
packMaterialIndices :: Vector.Vector Int -> Vector.Vector Int -> [Float]
packMaterialIndices vertFaces materialIndices = for [0..numIndices-1] $ \x -> realToFrac $ materialIndices Vector.! (faceNum x)
    where faceNum x = floor $ (realToFrac $ fromJust $ Vector.elemIndex x vertFaces) / 3
          numIndices = Vector.length vertFaces
          for = flip map

packXMesh :: DX.Mesh -> ([GLfloat], [GLushort])
packXMesh DX.Mesh { DX.nVertices, DX.vertices, DX.nFaces, DX.faces, DX.meshNormals, DX.meshTextureCoords, DX.skinWeights, DX.meshMaterialList }
    = (dat, indices)
    where verts = Vector.toList (Vector.map realToFrac vertices)
          material_indices = packMaterialIndices faces (DX.faceIndexes meshMaterialList)
          assignments = reverse $ foldl' (\lst i -> fromMaybe (error $ "Vertex #" ++ show i ++ " not assigned to a bone.")
                                                              (fromIntegral <$> findIndex (\DX.SkinWeights { DX.vertexIndices } -> Vector.elem i vertexIndices) skinWeights) : lst)
                                         [] [0..(Vector.length vertices - 1)]
          dat = interleave [verts, assignments, material_indices] [3,1,1]
          indices = Vector.toList (Vector.map fromIntegral faces)

loadModelFromX :: Shader -> String -> IO AnimatedModel
loadModelFromX shader path = do
        frame <- DX.loadX path
        let mesh = pullMesh frame
        vo <- createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 3,
                                                   Attachment sp_ATTR_BONE_INDEX 1,
                                                   Attachment sp_ATTR_MATERIAL_INDEX 1]]
            >>= \config -> loadVAOObj config Triangles (packXMesh mesh)

        return $ AnimatedModel vo frame

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

