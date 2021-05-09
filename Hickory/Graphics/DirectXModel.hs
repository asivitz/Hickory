module Hickory.Graphics.DirectXModel where

import Hickory.Utils.Utils
import Hickory.Math.Matrix
import Data.List
import Data.Maybe
import Hickory.Graphics.GLSupport
import qualified Hickory.Utils.DirectX as DX
import Data.Text (Text)
import Linear (V3(..), V4(..), identity, (!*!), (!*), inv44)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

import Hickory.Graphics.Drawing
import Graphics.GL.Compatibility41 as GL
import Hickory.Graphics.VAO (createVAOConfig, VAOObj(..))
import Hickory.Graphics.DrawUtils (loadVAOObj)


data ThreeDModel = ThreeDModel VAOObj (Maybe DX.Frame) DX.Mesh (V3 Double, V3 Double)
  deriving (Show)

animModelVAO :: ThreeDModel -> VAOObj
animModelVAO (ThreeDModel v _ _ _) = v

boneMatUniform :: ThreeDModel -> Text -> Double -> [UniformBinding]
boneMatUniform (ThreeDModel _ Nothing _ _) _ _ = []
boneMatUniform (ThreeDModel _ (Just frame) mesh _) actionName time = [UniformBinding "boneMat" (Matrix4Uniform mats)]
    where mats = animatedMats frame mesh (actionName, time)

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
    msh { DX.vertices = UV.map (\(x, y, z) -> let V4 x' y' z' _ = m !* (V4 x y z 1) in (x', y', z')) vertices }

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
                [ Attachment "position" 3
                , Attachment "normal" 3
                , Attachment "boneIndex" 1
                , Attachment "materialIndex" 1
                ]
            ]
          >>= \config -> loadVAOObj config Triangles (packAnimatedXMesh mesh)

      return $ ThreeDModel vo (Just frame) mesh extents
    else do
      vo <- createVAOConfig shader [VertexGroup [Attachment "position" 3, Attachment "materialIndex" 1]]
        >>= \config -> loadVAOObj config Triangles (packXMesh mesh)
      return $ ThreeDModel vo Nothing mesh extents

packAnimatedXMesh :: DX.Mesh -> (SV.Vector GLfloat, SV.Vector GLushort)
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

packXMesh :: DX.Mesh -> (SV.Vector GLfloat, SV.Vector GLushort)
packXMesh DX.Mesh { DX.vertices, DX.faces, DX.meshMaterialList } = (SV.fromList dat, indices)
 where
  verts            = packVertices vertices
  material_indices = packMaterialIndices faces (DX.faceIndexes meshMaterialList)
  dat              = interleave [verts, material_indices] [3, 1]
  indices          = V.convert (UV.map fromIntegral faces)

isAnimated :: DX.Mesh -> Bool
isAnimated DX.Mesh { DX.skinWeights } = (not . null) skinWeights

meshExtents :: DX.Mesh -> (V3 Double, V3 Double)
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
packMaterialIndices vertFaces materialIndices = for [0 .. numIndices - 1]
  $ \x -> realToFrac $ materialIndices UV.! faceNum x
 where
  faceNum x = faceNumForFaceIdx (fromJust $ UV.elemIndex x vertFaces)
  numIndices = UV.length vertFaces

-- TODO: Technically we can't assume each normal corresponds with a face.
-- We should read the extra data their to find the actual correspondance.
-- However, Blender exports them in face order so it doesn't matter.
packNormals :: UV.Unbox a => UV.Vector Int -> UV.Vector (a, a, a) -> [a]
packNormals faces normals = concat $ (map (\(x, y, z) -> [x, y, z]) . map snd . sortOn fst) pairs
 where
  pairs = for (UV.toList faces) (\vIdx -> let fnum = faceNumForFaceIdx vIdx in (vIdx, normals UV.! fnum))

packVertices :: (UV.Unbox a, UV.Unbox a1, UV.Unbox a2, Real a, Real a1, Real a2, Fractional b) => UV.Vector (a2, a1, a) -> [b]
packVertices verts = concatMap (\(x,y,z) -> [realToFrac x,realToFrac y,realToFrac z]) (UV.toList verts)

faceNumForFaceIdx :: Int -> Int
faceNumForFaceIdx x = floor $ (realToFrac x :: Double) / 3

-- interleaves arrays based on an array of counts
-- interleave [[1,2,3,4,5,6],[40,50,60]] [2,1] ~> [1,2,40,3,4,50,5,6,60]
interleave :: [[a]] -> [Int] -> [a]
interleave vals _ | any null vals = []
interleave vals counts            = pull counts vals ++ interleave (sub counts vals) counts
 where
  pull ns vs = concatMap (\(n, v) -> take n v) (zip ns vs)
  sub ns vs = map (\(n, v) -> drop n v) (zip ns vs)

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
