module Hickory.ModelLoading.DirectXModel
  ( loadModelFromX, ThreeDModel(..), animatedMats )
  where

import Hickory.Math.Matrix
import Data.List
import Data.Maybe
import qualified Hickory.ModelLoading.DirectX as DX
import Data.Text (Text)
import Linear (V3(..), V4(..), identity, (!*!), (!*), inv44, liftI2)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

import Hickory.ModelLoading.DirectX (MeshTextureCoords(..))
import qualified Hickory.Vulkan.Types as V
import Hickory.Math.Vector (Scalar)

{-
directxToModelData :: WavefrontOBJ -> ModelData
directxToModelData obj@WavefrontOBJ {..} = ModelData {..}
  where
  vertices         = SV.convert $ V.concatMap (packLocations obj) allFaceIndices
  normals          = SV.convert $ V.concatMap (packNormals obj) allFaceIndices
  uvs              = SV.convert $ V.concatMap (packTexCoords obj) allFaceIndices
  bone_indices     = SV.replicate (V.length allFaceIndices) 0
  material_indices = SV.replicate (V.length allFaceIndices) 0
  face_indices     = SV.convert $ V.concatMap (\(Face one two three _) -> V.fromList $ mapMaybe (fmap fromIntegral . (`V.elemIndex` allFaceIndices)) [one,two,three]) faces

  faces = fmap elValue objFaces
  allFaceIndices :: V.Vector FaceIndex
  allFaceIndices = V.fromList . nub . concat $ faces <&> \(Face one two three xtras) -> one : two : three : xtras

-}

data ThreeDModel = ThreeDModel
  { packedMesh :: V.Mesh
  , frame      :: Maybe DX.Frame
  , mesh       :: DX.Mesh
  , extents    :: (V3 Scalar, V3 Scalar)
  }

-- animModelVAO :: ThreeDModel -> VAO
-- animModelVAO (ThreeDModel v _ _ _) = v

{-
boneMatUniform :: ThreeDModel -> Text -> Double -> [ShaderFunction]
boneMatUniform (ThreeDModel _ Nothing _ _) _ _ = []
boneMatUniform (ThreeDModel _ (Just frame) mesh _) actionName time = [bindUniform "boneMat" mats]
    where mats = animatedMats frame mesh (actionName, time)
          -}

-- .X
pullMesh :: DX.Frame -> DX.Mesh
pullMesh fr = case go fr of
  Nothing                             -> error "Could not grab mesh from X structure"
  Just m@DX.Mesh { DX.skinWeights } -> m { DX.skinWeights = sortWeights skinWeights fr }
 where
  go (DX.Frame _ _   children Nothing     _) = listToMaybe $ mapMaybe go children
  go (DX.Frame _ mat _        (Just mesh) _) = Just (transformMesh mat mesh)
  -- Put weights, and therefore bones, in order from root to leaf
  sortWeights :: [DX.SkinWeights] -> DX.Frame -> [DX.SkinWeights]
  sortWeights weights (DX.Frame name _ children _ _) =
    case find (\DX.SkinWeights { DX.transformNodeName } -> Just transformNodeName == name) weights of
      Just w  -> w : concatMap (sortWeights weights) children
      Nothing -> concatMap (sortWeights weights) children
  transformMesh :: Mat44 -> DX.Mesh -> DX.Mesh
  transformMesh m msh@DX.Mesh { DX.vertices } =
    msh { DX.vertices = UV.map (\(x, y, z) -> let V4 x' y' z' _ = m !* V4 x y z 1 in (x', y', z')) vertices }

loadModelFromX :: String -> IO ThreeDModel
loadModelFromX path = do
  frame <- DX.loadX path
  let mesh    = pullMesh frame
      extents = meshExtents mesh
  pure $ ThreeDModel (xToMesh mesh) (Just frame) mesh extents

{-
packAnimatedXMesh :: DX.Mesh -> (SV.Vector GLfloat, SV.Vector GLuint)
packAnimatedXMesh DX.Mesh { DX.vertices, DX.faces, DX.meshNormals, DX.skinWeights, DX.meshMaterialList } =
  (SV.fromList dat, indices)
 where
  verts            = packVertices vertices
  material_indices = packMaterialIndices faces (DX.faceIndexes meshMaterialList)
  normals          = map realToFrac $ packNormals faces (DX.normals meshNormals)
  assignments      = reverse $ foldl'
    ( \lst i ->
      fromMaybe
          (error $ "Vertex #" ++ show i ++ " not assigned to a bone.")
          (fromIntegral <$> findIndex (\DX.SkinWeights { DX.vertexIndices } -> UV.elem i vertexIndices) skinWeights)
        : lst
    )
    []
    [0 .. (UV.length vertices - 1)]
  dat     = interleave [verts, normals, assignments, material_indices] [3, 3, 1, 1]
  indices = V.convert $ UV.map fromIntegral faces

packXMesh :: DX.Mesh -> (SV.Vector GLfloat, SV.Vector GLuint)
packXMesh DX.Mesh { DX.vertices, DX.faces, DX.meshMaterialList, DX.meshNormals, DX.meshTextureCoords } = (SV.fromList dat, indices)
 where
  verts            = packVertices vertices
  material_indices = packMaterialIndices faces (DX.faceIndexes meshMaterialList)
  normals          = map realToFrac $ packNormals faces (DX.normals meshNormals)
  texture_coords   = fmap realToFrac . UV.toList $ textureCoords meshTextureCoords
  dat              = interleave [verts, normals, material_indices, texture_coords] [3, 3, 1, 2]
  indices          = V.convert (UV.map fromIntegral faces)
  -}

xToMesh :: DX.Mesh -> V.Mesh
xToMesh DX.Mesh {..} = V.Mesh meshVerts (Just meshIndices) minPosition maxPosition
  where
  positions     = (V.Position, SV.fromList $ packVertices vertices)
  normals       = (V.Normal,   SV.fromList $ packNormals faces (DX.normals meshNormals))
  uvs           = (V.TextureCoord, SV.convert $ textureCoords meshTextureCoords)
  material_idxs = (V.MaterialIndex, SV.fromList $ packMaterialIndices faces (DX.faceIndexes meshMaterialList))
  boneIndices   = (V.BoneIndex,) . SV.fromList <$> traverse
    (\i -> fromIntegral <$> findIndex (\DX.SkinWeights { DX.vertexIndices } -> UV.elem i vertexIndices) skinWeights)
    [0 .. (UV.length vertices - 1)]
  meshVerts     = catMaybes [Just positions, Just normals, Just uvs, boneIndices, Just material_idxs]
  meshIndices   = SV.map fromIntegral $ SV.convert faces
  minPosition = UV.foldl1' (liftI2 min) . UV.map (\(x,y,z) -> V3 x y z) $ vertices
  maxPosition = UV.foldl1' (liftI2 max) . UV.map (\(x,y,z) -> V3 x y z) $ vertices

-- isAnimated :: DX.Mesh -> Bool
-- isAnimated DX.Mesh { DX.skinWeights } = (not . null) skinWeights

meshExtents :: DX.Mesh -> (V3 Scalar, V3 Scalar)
meshExtents DX.Mesh { DX.vertices } = (V3 mnx mny mnz, V3 mxx mxy mxz)
 where
  Just (mnx, mny, mnz) = f min
  Just (mxx, mxy, mxz) = f max
  f func = UV.foldl'
    ( \trip (x, y, z) -> case trip of
      Just (mx, my, mz) -> Just (func mx x, func my y, func mz z)
      Nothing           -> Just (x, y, z)
    )
    Nothing
    vertices

-- Provide a material index for each vertex
packMaterialIndices :: UV.Vector Int -> UV.Vector Int -> [Float]
packMaterialIndices vertFaces materialIndices = flip map [0 .. numIndices - 1]
  $ \x -> realToFrac $ materialIndices UV.! faceNum x
 where
  faceNum x = faceNumForFaceIdx (fromJust $ UV.elemIndex x vertFaces)
  numIndices = UV.length vertFaces

-- TODO: Technically we can't assume each normal corresponds with a face.
-- We should read the extra data their to find the actual correspondance.
-- However, Blender exports them in face order so it doesn't matter.
packNormals :: UV.Unbox a => UV.Vector Int -> UV.Vector (a, a, a) -> [a]
packNormals faces normals = concatMap ((\ (x, y, z) -> [x, y, z]) . snd) (sortOn fst pairs)
 where
  pairs = map (\vIdx -> let fnum = faceNumForFaceIdx vIdx in (vIdx, normals UV.! fnum)) (UV.toList faces)

packVertices :: (UV.Unbox a, UV.Unbox a1, UV.Unbox a2, Real a, Real a1, Real a2, Fractional b) => UV.Vector (a2, a1, a) -> [b]
packVertices verts = concatMap (\(x,y,z) -> [realToFrac x,realToFrac y,realToFrac z]) (UV.toList verts)

faceNumForFaceIdx :: Int -> Int
faceNumForFaceIdx x = floor $ (realToFrac x :: Scalar) / 3

{-
-- interleaves arrays based on an array of counts
-- interleave [[1,2,3,4,5,6],[40,50,60]] [2,1] ~> [1,2,40,3,4,50,5,6,60]
interleave :: [[a]] -> [Int] -> [a]
interleave vals _ | any null vals = []
interleave vals counts            = pull counts vals ++ interleave (sub counts vals) counts
 where
  pull ns vs = concatMap (\(n, v) -> take n v) (zip ns vs)
  sub ns vs = map (\(n, v) -> drop n v) (zip ns vs)
  -}

--TODO: Read animation FPS from directx file
retrieveActionMat :: (Text, Scalar) -> [(Text, [Mat44])] -> Maybe Mat44
retrieveActionMat (actionName, time) actionMats = (\kf -> kf !! (floor (time * 25) `mod` length kf)) <$> keyFrames
        where keyFrames = lookup actionName actionMats

buildAnimatedMats :: Mat44 -> Mat44 -> (Text, Scalar) -> DX.Frame -> [(Text, Mat44)]
buildAnimatedMats bindParent animParent animSel DX.Frame { DX.frameName, DX.children, DX.actionMats, DX.mat } =
  case frameName of
    Just fn -> (fn, m) : rest
    Nothing -> rest
 where
  rest      = concatMap (buildAnimatedMats bindPose animMat animSel) children
  m         = animMat !*! inv44 bindPose
  bindPose  = bindParent !*! mat
  animMat   = animParent !*! actionMat
  actionMat = fromMaybe mat $ retrieveActionMat animSel actionMats

animatedMats :: DX.Frame -> DX.Mesh -> (Text, Scalar) -> [Mat44]
animatedMats f DX.Mesh { DX.skinWeights } animSel = (sortem . buildAnimatedMats identity identity animSel) f
        where sortem :: [(Text, Mat44)] -> [Mat44]
              sortem pairs = reverse $ foldl' (\lst sw -> maybe lst (\x -> snd x : lst) $ find (\(name, _) -> name == DX.transformNodeName sw) pairs) [] skinWeights
