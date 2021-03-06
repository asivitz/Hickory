{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Hickory.Graphics.DrawUtils where

import Hickory.Types

import Control.Lens (Lens', view, set)
import Hickory.Utils.Utils
import Hickory.Math.Matrix
import Hickory.Math.Vector
import Data.List
import Data.Maybe
import Hickory.Graphics.DrawText
import Hickory.Text.Text
import Hickory.Graphics.GLSupport
import Data.Foldable (toList, forM_)
import qualified Hickory.Utils.Obj as OBJ
import qualified Hickory.Utils.DirectX as DX
import Data.Text (Text)
import Data.IORef (IORef, writeIORef, readIORef)
import Linear (V3(..), V4(..), scaled, M44, identity, (!*!), (!*), inv44)

import Hickory.Graphics.Drawing
import Graphics.GL.Compatibility41 as GL
import qualified Data.Vector.Unboxed as Vector
import Hickory.Graphics.VAO (VAOConfig, createVAOConfig, VAOObj(..), drawCommand, loadVerticesIntoVAOConfig)

data Command = Command Mat44 [UniformBinding] (Maybe TexID) DrawSpec

data DrawSpec
  = VAO VAOObj
  | DynVAO VAOConfig ([GLfloat],[GLushort],DrawType)
  deriving (Show)

--TODO: Read animation FPS from directx file
retrieveActionMat :: (Text, Double) -> [(Text, [Mat44])] -> Maybe Mat44
retrieveActionMat (actionName, time) actionMats = (\kf -> kf !! (floor (time * 25) `mod` length kf)) <$> keyFrames
        where keyFrames = lookup actionName actionMats

buildAnimatedMats :: Mat44 -> Mat44 -> (Text, Double) -> DX.Frame -> [(Text, Mat44)]
buildAnimatedMats bindParent animParent animSel DX.Frame { DX.frameName, DX.children, DX.actionMats, DX.mat } =
  (frameName, m) : concatMap (buildAnimatedMats bindPose animMat animSel) children
 where
  m         = animMat !*! inv44 bindPose
  bindPose  = bindParent !*! mat
  animMat   = animParent !*! actionMat
  actionMat = fromMaybe mat $ retrieveActionMat animSel actionMats

animatedMats :: DX.Frame -> DX.Mesh -> (Text, Double) -> [Mat44]
animatedMats f DX.Mesh { DX.skinWeights } animSel = (sortem . buildAnimatedMats identity identity animSel) f
        where sortem :: [(Text, Mat44)] -> [Mat44]
              sortem pairs = reverse $ foldl' (\lst sw -> maybe lst (\x -> snd x : lst) $ find (\(name, _) -> name == DX.transformNodeName sw) pairs) [] skinWeights

colorUniform :: V4 Scalar -> UniformBinding
colorUniform color = UniformBinding "color" (QuadFUniform [color])

boneMatUniform :: ThreeDModel -> Text -> Double -> [UniformBinding]
boneMatUniform (ThreeDModel _ Nothing _ _) _ _ = []
boneMatUniform (ThreeDModel _ (Just frame) mesh _) actionName time = [UniformBinding "boneMat" (Matrix4Uniform mats)]
    where mats = animatedMats frame mesh (actionName, time)

drawSpec :: [UniformBinding] -> Maybe TexID -> DrawSpec -> IO ()
drawSpec uniforms tex spec = case spec of
  VAO (VAOObj _ 0 _) -> pure ()
  VAO (VAOObj vaoConfig numitems drawType) -> do
    drawCommand uniforms tex vaoConfig (fromIntegral numitems) drawType
  DynVAO vaoConfig (verts, indices, drawType) -> do
    loadVerticesIntoVAOConfig vaoConfig verts indices
    drawCommand uniforms tex vaoConfig (fromIntegral $ length indices) drawType

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

data RenderTree
  = Primitive [UniformBinding] (Maybe TexID) DrawSpec
  | List [RenderTree]
  | XForm Mat44 RenderTree
  | NoRender
  deriving (Show)

cubeFloats :: [GLfloat]
cubeFloats = concatMap toList verts
 where
  h     = 0.5
  l     = -h
  p1    = V3 l l l
  p2    = V3 h l l
  p3    = V3 h h l
  p4    = V3 l h l
  p5    = V3 l l h
  p6    = V3 h l h
  p7    = V3 h h h
  p8    = V3 l h h
  verts = [p1, p2, p3, p4, p5, p6, p7, p8]

cubeIndices :: [GLushort]
cubeIndices = [6, 7, 5, 4, 0, 7, 3, 6, 2, 5, 1, 0, 2, 3]

loadVAOObj :: VAOConfig -> DrawType -> ([GLfloat], [GLushort]) -> IO VAOObj
loadVAOObj vaoconfig drawType (verts, indices) = do
  loadVerticesIntoVAOConfig vaoconfig verts indices
  return $ VAOObj vaoconfig (length indices) drawType

loadCubeIntoVAOConfig :: VAOConfig -> IO VAOObj
loadCubeIntoVAOConfig vaoconfig = do
  let floats  = cubeFloats
      indices = cubeIndices
  loadVAOObj vaoconfig TriangleStrip (floats, indices)

loadInvertedCubeIntoVAOConfig :: VAOConfig -> IO VAOObj
loadInvertedCubeIntoVAOConfig vaoconfig = do
  loadVAOObj vaoconfig TriangleStrip (cubeFloats, reverse cubeIndices)

mkCubeVAOObj :: Shader -> IO VAOObj
mkCubeVAOObj shader = createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 3]] >>= loadCubeIntoVAOConfig

mkInvertedCubeVAOObj :: Shader -> IO VAOObj
mkInvertedCubeVAOObj shader = createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 3]] >>= loadCubeIntoVAOConfig

mkSquareVerts :: (Num t, Fractional t1) => t1 -> t1 -> ([t1], [t], DrawType)
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
packOBJ (OBJ.OBJ vertices texCoords normals faces) = (concatMap snd pool, indices)
 where
  construct (vidx, tcidx, nidx) =
    map realToFrac $ toList (vertices  !! (vidx - 1))
                  ++ toList (texCoords !! (tcidx - 1))
                  ++ toList (normals   !! (nidx - 1))
  pool    = foldl' (\alst e -> (e, construct e) : alst) [] $ concatMap toList faces
  indices = map (fromIntegral . fromJust . (\e -> findIndex (\(e', _) -> e == e') pool)) $ concatMap toList faces

createVAOConfigFromOBJ :: Shader -> String -> IO VAOObj
createVAOConfigFromOBJ shader path = do
  obj <- OBJ.loadOBJ path
  createVAOConfig
      shader
      [VertexGroup [Attachment sp_ATTR_POSITION 3, Attachment sp_ATTR_TEX_COORDS 2, Attachment sp_ATTR_NORMALS 3]]
    >>= \config -> loadVAOObj config Triangles (packOBJ obj)

-- interleaves arrays based on an array of counts
-- interleave [[1,2,3,4,5,6],[40,50,60]] [2,1] ~> [1,2,40,3,4,50,5,6,60]
interleave :: [[a]] -> [Int] -> [a]
interleave vals _ | any null vals = []
interleave vals counts            = pull counts vals ++ interleave (sub counts vals) counts
 where
  pull ns vs = concatMap (\(n, v) -> take n v) (zip ns vs)
  sub ns vs = map (\(n, v) -> drop n v) (zip ns vs)

data ThreeDModel = ThreeDModel VAOObj (Maybe DX.Frame) DX.Mesh (V3 Double, V3 Double)
  deriving (Show)

animModelVAO :: ThreeDModel -> VAOObj
animModelVAO (ThreeDModel v _ _ _) = v

-- .X
pullMesh :: DX.Frame -> DX.Mesh
pullMesh fr = case go fr of
  Nothing                             -> error "Could not grab mesh from X structure"
  Just (m@DX.Mesh { DX.skinWeights }) -> m { DX.skinWeights = sortWeights skinWeights fr }
 where
  go (DX.Frame _ _   children Nothing     _) = listToMaybe $ mapMaybe go children
  go (DX.Frame _ mat _        (Just mesh) _) = Just (transformMesh mat mesh)
  -- Put weights, and therefore bones, in order from root to leaf
  sortWeights :: [DX.SkinWeights] -> DX.Frame -> [DX.SkinWeights]
  sortWeights weights (DX.Frame name _ children _ _) =
    case find (\DX.SkinWeights { DX.transformNodeName } -> transformNodeName == name) weights of
      Just w  -> [w] ++ concatMap (sortWeights weights) children
      Nothing -> concatMap (sortWeights weights) children
  transformMesh :: Mat44 -> DX.Mesh -> DX.Mesh
  transformMesh m msh@DX.Mesh { DX.vertices } =
    msh { DX.vertices = Vector.map (\(x, y, z) -> let V4 x' y' z' _ = m !* (V4 x y z 1) in (x', y', z')) vertices }

faceNumForFaceIdx :: Int -> Int
faceNumForFaceIdx x = floor $ (realToFrac x :: Double) / 3

-- Provide a material index for each vertex
packMaterialIndices :: Vector.Vector Int -> Vector.Vector Int -> [Float]
packMaterialIndices vertFaces materialIndices = for [0 .. numIndices - 1]
  $ \x -> realToFrac $ materialIndices Vector.! faceNum x
 where
  faceNum x = faceNumForFaceIdx (fromJust $ Vector.elemIndex x vertFaces)
  numIndices = Vector.length vertFaces

-- TODO: Technically we can't assume each normal corresponds with a face.
-- We should read the extra data their to find the actual correspondance.
-- However, Blender exports them in face order so it doesn't matter.
packNormals :: Vector.Unbox a => Vector.Vector Int -> Vector.Vector (a, a, a) -> [a]
packNormals faces normals = concat $ (map (\(x, y, z) -> [x, y, z]) . map snd . sortOn fst) pairs
 where
  pairs = for (Vector.toList faces) (\vIdx -> let fnum = faceNumForFaceIdx vIdx in (vIdx, normals Vector.! fnum))

packVertices :: (Vector.Unbox a, Vector.Unbox a1, Vector.Unbox a2, Real a, Real a1, Real a2, Fractional b) => Vector.Vector (a2, a1, a) -> [b]
packVertices verts = concatMap (\(x,y,z) -> [realToFrac x,realToFrac y,realToFrac z]) (Vector.toList verts)

packAnimatedXMesh :: DX.Mesh -> ([GLfloat], [GLushort])
packAnimatedXMesh DX.Mesh { DX.vertices, DX.faces, DX.meshNormals, DX.skinWeights, DX.meshMaterialList } =
  (dat, indices)
 where
  verts            = packVertices vertices
  material_indices = packMaterialIndices faces (DX.faceIndexes meshMaterialList)
  normals          = map realToFrac $ packNormals faces (DX.normals meshNormals)
  assignments      = reverse $ foldl'
    ( \lst i ->
      fromMaybe
          (error $ "Vertex #" ++ show i ++ " not assigned to a bone.")
          (fromIntegral <$> findIndex (\DX.SkinWeights { DX.vertexIndices } -> Vector.elem i vertexIndices) skinWeights)
        : lst
    )
    []
    [0 .. (Vector.length vertices - 1)]
  dat     = interleave [verts, normals, assignments, material_indices] [3, 3, 1, 1]
  indices = Vector.toList (Vector.map fromIntegral faces)

packXMesh :: DX.Mesh -> ([GLfloat], [GLushort])
packXMesh DX.Mesh { DX.vertices, DX.faces, DX.meshMaterialList } = (dat, indices)
 where
  verts            = packVertices vertices
  material_indices = packMaterialIndices faces (DX.faceIndexes meshMaterialList)
  dat              = interleave [verts, material_indices] [3, 1]
  indices          = Vector.toList (Vector.map fromIntegral faces)

isAnimated :: DX.Mesh -> Bool
isAnimated DX.Mesh { DX.skinWeights } = (not . null) skinWeights

meshExtents :: DX.Mesh -> (V3 Double, V3 Double)
meshExtents DX.Mesh { DX.vertices } = (V3 mnx mny mnz, V3 mxx mxy mxz)
 where
  Just (mnx, mny, mnz) = f min
  Just (mxx, mxy, mxz) = f max
  f func = Vector.foldl'
    ( \trip (x, y, z) -> case trip of
      Just (mx, my, mz) -> Just (func mx x, func my y, func mz z)
      Nothing           -> Just (x, y, z)
    )
    Nothing
    vertices

loadModelFromX :: Shader -> String -> IO ThreeDModel
loadModelFromX shader path = do
  frame <- DX.loadX path
  let mesh    = pullMesh frame
      extents = meshExtents mesh

  if isAnimated mesh
    then do
      vo <-
        createVAOConfig
            shader
            [ VertexGroup
                [ Attachment sp_ATTR_POSITION       3
                , Attachment sp_ATTR_NORMALS        3
                , Attachment sp_ATTR_BONE_INDEX     1
                , Attachment sp_ATTR_MATERIAL_INDEX 1
                ]
            ]
          >>= \config -> loadVAOObj config Triangles (packAnimatedXMesh mesh)

      return $ ThreeDModel vo (Just frame) mesh extents
    else do
      vo <- createVAOConfig shader [VertexGroup [Attachment sp_ATTR_POSITION 3, Attachment sp_ATTR_MATERIAL_INDEX 1]]
        >>= \config -> loadVAOObj config Triangles (packXMesh mesh)
      return $ ThreeDModel vo Nothing mesh extents

renderTrees :: [RenderTree] -> IO ()
renderTrees trees = do
  clearScreen
  forM_ trees $ \t -> do
    renderTree t
    clearDepth

renderTree :: RenderTree -> IO ()
renderTree = drawTree

render :: [RenderTree] -> IO ()
render = renderTrees

runDrawCommand :: Command -> IO ()
runDrawCommand (Command mat uniforms tex spec) = drawSpec (UniformBinding "modelMat" (Matrix4Uniform [mat]) : uniforms) tex spec

{-rtDepth :: RenderTree -> Scalar-}
{-rtDepth (RSquare _ (Vector3 _ _ z) _ _ _) = z-}
{-rtDepth _ = 0-}
drawTree :: RenderTree -> IO ()
drawTree tree = mapM_ runDrawCommand (reverse commands)
  where commands = collectTreeSpecs Nothing tree
        {-sorted = sortOn (\(Command _ m _ _) -> - (m !* V4 0 0 0 1) ^. _z) commands-}

combineMats :: Maybe (M44 Scalar) -> Maybe (M44 Scalar) -> Maybe (M44 Scalar)
combineMats Nothing Nothing = Nothing
combineMats Nothing (Just y) = Just y
combineMats (Just x) Nothing = Just x
combineMats (Just x) (Just y) = Just (x !*! y)

collectTreeSpecs :: Maybe (M44 Scalar) -> RenderTree -> [Command]
collectTreeSpecs _         NoRender          = []
collectTreeSpecs parentMat (XForm mat child) = collectTreeSpecs (combineMats parentMat (Just mat)) child
collectTreeSpecs parentMat (Primitive uniforms tex spec) = [Command mat' uniforms tex spec]
  where mat' = fromMaybe identity parentMat
collectTreeSpecs parentMat (List children) = concatMap (collectTreeSpecs parentMat) children -- (sortOn rtDepth children)
