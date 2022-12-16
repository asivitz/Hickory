module Hickory.ModelLoading.Wavefront where

import Data.List
import Data.Maybe
import Data.Functor ((<&>))
import Hickory.Graphics.GLSupport
import Linear (V2(..), V3(..), zero)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Codec.Wavefront (WavefrontOBJ(..), Location(..), TexCoord(..), Normal(..), Face(..), FaceIndex(..), elValue)
import qualified Codec.Wavefront as Wavefront

import Graphics.GL.Compatibility41 as GL
import Hickory.Graphics.VAO (createIndexedVAO, createDirectVAO, VAO(..), VertexGroup (..), Attachment (..))
import qualified Data.HashMap.Strict as Map
import Control.Monad.State.Strict (State, execState, modify, gets)
import Data.Hashable (Hashable(..))
import Hickory.Graphics.Shader (Shader)
import qualified Hickory.Vulkan.Mesh as HM

loadWavefront :: FilePath -> IO WavefrontOBJ
loadWavefront path = Wavefront.fromFile path >>= \case
  Left err -> error err
  Right obj -> pure obj

wavefrontToMesh :: WavefrontOBJ -> HM.Mesh
wavefrontToMesh obj@WavefrontOBJ {..} = HM.Mesh {..}
  where
  positions = (HM.Position, SV.convert $ V.concatMap (packLocations obj) allFaceIndices)
  normals   = (HM.Normal, SV.convert $ V.concatMap (packNormals obj) allFaceIndices)
  uvs       = (HM.TextureCoord, SV.convert $ V.concatMap (packTexCoords obj) allFaceIndices)
  indices   = Just . SV.convert $ V.concatMap (\(Face one two three _) -> V.fromList $ mapMaybe (fmap fromIntegral . (`V.elemIndex` allFaceIndices)) [one,two,three]) faces
  vertices  = [positions, normals, uvs]

  faces = fmap elValue objFaces
  allFaceIndices :: V.Vector FaceIndex
  allFaceIndices = V.fromList . nub . concat $ faces <&> \(Face one two three xtras) -> one : two : three : xtras

loadWavefrontMesh :: FilePath -> IO HM.Mesh
loadWavefrontMesh = fmap wavefrontToMesh . loadWavefront

--

-- Pack a WavefrontOBJ into vertices and indices
-- Configurable for different data (positions, normals, etc.)
{-# DEPRECATED #-}
packOBJIndexed :: WavefrontOBJ -> [WavefrontOBJ -> FaceIndex -> V.Vector GLfloat] -> (SV.Vector GLfloat, SV.Vector GLuint)
packOBJIndexed obj@WavefrontOBJ { objFaces } fs = (SV.convert verts, SV.convert indices)
  where
  indices = V.concatMap (\(Face one two three _) -> V.fromList $ mapMaybe (fmap fromIntegral . (`V.elemIndex` allFaceIndices)) [one,two,three]) faces
  verts :: V.Vector GLfloat
  verts = V.concatMap (\fi -> V.concat $ map ($ fi) packFuns) allFaceIndices
  packFuns = map ($ obj) fs
  faces = fmap elValue objFaces
  allFaceIndices :: V.Vector FaceIndex
  allFaceIndices = V.fromList . nub . concat $ faces <&> \(Face one two three xtras) -> one : two : three : xtras

grabLocation :: WavefrontOBJ -> FaceIndex -> V3 Float
grabLocation WavefrontOBJ { objLocations } (FaceIndex loci _ _) =
  case objLocations V.!? pred loci of
    Nothing -> error "Can't find location in wavefront obj"
    Just (Location x y z _) -> V3 x y z

grabNormal :: WavefrontOBJ -> FaceIndex -> Maybe (V3 Float)
grabNormal WavefrontOBJ { objNormals } (FaceIndex _ _ mni) =
  case mni >>= (objNormals V.!?) . pred of
    Just (Normal x y z) -> Just $ V3 x y z
    Nothing -> Nothing

grabTexCoords :: WavefrontOBJ -> FaceIndex -> Maybe (V2 Float)
grabTexCoords WavefrontOBJ { objTexCoords } (FaceIndex _ mtci _) =
  case mtci >>= (objTexCoords V.!?) . pred of
    Just (TexCoord u v _) -> Just $ V2 u v
    Nothing -> Nothing

convertV3 :: V3 a -> V.Vector a
convertV3 (V3 x y z) = V.fromList [x, y, z]

convertV2 :: V2 a -> V.Vector a
convertV2 (V2 x y) = V.fromList [x, y]

packNormals :: WavefrontOBJ -> FaceIndex -> V.Vector GLfloat
packNormals obj fi = convertV3 . fromMaybe zero $ grabNormal obj fi

packLocations :: WavefrontOBJ -> FaceIndex -> V.Vector GLfloat
packLocations obj fi = convertV3 $ grabLocation obj fi

packTexCoords :: WavefrontOBJ -> FaceIndex -> V.Vector GLfloat
packTexCoords obj fi = convertV2 . fromMaybe zero $ grabTexCoords obj fi


{- HERE BE DRAGONS -}

-- The basic idea is
--     (1,0,0)
--       /\
--      /  \
--     /    \
--     ------
-- (0,1,0)  (0,0,1)
-- Useful for effects like drawing edges
packBarycentricCoords :: WavefrontOBJ -> FaceIndex -> V.Vector GLfloat
packBarycentricCoords obj = \fi -> case Map.lookup fi cache of
  Nothing -> V.fromList [0, 0, 0]
  Just (V3 x y z) -> V.fromList [x,y,z]
  where
  cache = barycentricCoords obj

instance Hashable FaceIndex where
  hashWithSalt salt (FaceIndex loci mtci mni) = salt + loci + fromMaybe 0 mtci + fromMaybe 0 mni

barycentricCoords :: WavefrontOBJ -> Map.HashMap FaceIndex (V3 Float)
barycentricCoords obj@WavefrontOBJ { objNormals = _, objFaces } = execState (V.mapM (foo . elValue) objFaces) Map.empty
  where
  foo :: Face -> State (Map.HashMap FaceIndex (V3 Float)) ()
  foo (Face one two three _) = do
    let basis1 = basisForColor (Map.lookup one colors)
        basis2 = basisForColor (Map.lookup two colors)
        basis3 = basisForColor (Map.lookup three colors)

    -- Internal verts get higher numbers, so that they disappear when e.g.
    -- drawing edges
    let c1 :: V3 Float = basis1 + hide13 + hide12
        c2 :: V3 Float = basis2 + hide23 + hide12
        c3 :: V3 Float = basis3 + hide23 + hide13
        hide12 = if numFacesContaining obj edgeCache one two > 1 then basis3 else 0
        hide23 = if numFacesContaining obj edgeCache two three > 1 then basis1 else 0
        hide13 = if numFacesContaining obj edgeCache one three > 1 then basis2 else 0

    setCoordOfIndex one c1
    setCoordOfIndex two c2
    setCoordOfIndex three c3
    pure ()
  basisForColor = \case
    Just Red   -> V3 1 0 0
    Just Green -> V3 0 1 0
    Just Blue  -> V3 0 0 1
    Nothing -> error "Can't find color"
  setCoordOfIndex :: FaceIndex -> V3 Float -> State (Map.HashMap FaceIndex (V3 Float)) ()
  setCoordOfIndex k = modify . Map.insert k
  colors = colorObj obj
  edgeCache = mkEdgeCache obj

data ObjColor = Red | Green | Blue deriving (Eq, Show)

-- Assign a 'color' to each vert, so that no triangle has two of the same color
-- Not so useful in itself, but useful for making barycentric coords
colorObj :: WavefrontOBJ -> Map.HashMap FaceIndex ObjColor
colorObj WavefrontOBJ { objNormals = _, objFaces } = execState (V.mapM (foo . elValue) objFaces) Map.empty
  where
  foo :: Face -> State (Map.HashMap FaceIndex ObjColor) ()
  foo (Face one two three _) = do
    setColor one two three
    setColor two one three
    setColor three one two

  setColor :: FaceIndex -> FaceIndex -> FaceIndex -> State (Map.HashMap FaceIndex ObjColor) ()
  setColor fi other1 other2 = do
    colorOfIndex fi >>= \case
      Just _ -> pure ()
      Nothing -> do
        existing <- traverse colorOfIndex [other1, other2]
        let new = head $ [Red, Green, Blue] \\ catMaybes existing
        setColorOfIndex fi new

  colorOfIndex :: FaceIndex -> State (Map.HashMap FaceIndex ObjColor) (Maybe ObjColor)
  colorOfIndex = gets . Map.lookup
  setColorOfIndex :: FaceIndex -> ObjColor -> State (Map.HashMap FaceIndex ObjColor) ()
  setColorOfIndex k = modify . Map.insert k

-- Standard config of positions, tex coords, and normals
{-# DEPRECATED #-}
vaoFromWavefront :: Shader -> WavefrontOBJ -> IO VAO
vaoFromWavefront shader obj =
  createIndexedVAO
    shader
    [VertexGroup [Attachment "position" 3, Attachment "texCoords" 2, Attachment "normal" 3]]
    (packOBJIndexed obj [packLocations, packTexCoords, packNormals])
    Triangles

vaoFromWavefrontWithBarycentric :: Shader -> WavefrontOBJ -> IO VAO
vaoFromWavefrontWithBarycentric shader obj =
  createIndexedVAO shader
    [VertexGroup [Attachment "position" 3, Attachment "texCoords" 2, Attachment "normal" 3, Attachment "barycentric" 3]]
    (packOBJIndexed obj [packLocations, packTexCoords, packNormals, packBarycentricCoords])
    Triangles

-- Pack a WavefrontOBJ into vertices
-- Configurable for different data (positions, normals, etc.)
packOBJ :: WavefrontOBJ -> [WavefrontOBJ -> Face -> [V.Vector GLfloat]] -> (SV.Vector GLfloat, Int)
packOBJ obj@WavefrontOBJ { objFaces } fs = (SV.convert verts, V.length objFaces * 3)
  where
  verts :: V.Vector GLfloat
  verts = V.concatMap (\face -> V.concat . fmap V.concat . transpose $ map ($ face) packFuns) faces
  packFuns = map ($ obj) fs
  faces = fmap elValue objFaces

eachFaceIndex :: (WavefrontOBJ -> FaceIndex -> V.Vector GLfloat) -> WavefrontOBJ -> Face -> [V.Vector GLfloat]
eachFaceIndex f obj (Face one two three xtras) = f obj <$> one : two : three : xtras

packInternalEdge :: WavefrontOBJ -> Face -> [V.Vector GLfloat]
packInternalEdge obj = \(Face one two three _) ->
  let internal12 = numFacesContaining obj edgeCache one two   > 1
      internal23 = numFacesContaining obj edgeCache two three > 1
      internal13 = numFacesContaining obj edgeCache one three > 1
  in [ toV internal23, toV internal13, toV internal12 ]
  where
  toV False = V.fromList [ 0 ]
  toV True  = V.fromList [ 1 ]
  edgeCache = mkEdgeCache obj

packBarycentricFace :: WavefrontOBJ -> Face -> [V.Vector GLfloat]
packBarycentricFace obj = \(Face one two three _) ->
  let basis1 = V3 1 0 0
      basis2 = V3 0 1 0
      basis3 = V3 0 0 1
      c1 :: V3 Float = basis1 + hide13 + hide12
      c2 :: V3 Float = basis2 + hide23 + hide12
      c3 :: V3 Float = basis3 + hide23 + hide13
      hide12 = if numFacesContaining obj edgeCache one two > 1 then basis3 else 0
      hide23 = if numFacesContaining obj edgeCache two three > 1 then basis1 else 0
      hide13 = if numFacesContaining obj edgeCache one three > 1 then basis2 else 0
  in [ convertV3 c1, convertV3 c2, convertV3 c3]
  where
  edgeCache = mkEdgeCache obj

vaoFromWavefrontWithInternalEdges :: Shader -> WavefrontOBJ -> IO VAO
vaoFromWavefrontWithInternalEdges shader obj =
  createDirectVAO
    shader
    [VertexGroup [Attachment "position" 3, Attachment "texCoords" 2, Attachment "normal" 3, Attachment "excludeEdge" 1]]
    (packOBJ obj [eachFaceIndex packLocations, eachFaceIndex packTexCoords, eachFaceIndex packNormals, packInternalEdge])
    Triangles

vaoFromWavefrontWithBarycentricDirect :: Shader -> WavefrontOBJ -> IO VAO
vaoFromWavefrontWithBarycentricDirect shader obj =
  createDirectVAO shader
    [VertexGroup [Attachment "position" 3, Attachment "texCoords" 2, Attachment "normal" 3, Attachment "barycentric" 3]]
    (packOBJ obj [eachFaceIndex packLocations, eachFaceIndex packTexCoords, eachFaceIndex packNormals, packBarycentricFace])
    Triangles

 -- Utils

numFacesContaining :: WavefrontOBJ -> Map.HashMap Int Int -> FaceIndex -> FaceIndex -> Int
numFacesContaining obj cache a b = Map.lookupDefault 0 (hashEdge obj a b) cache

hashEdge :: WavefrontOBJ -> FaceIndex -> FaceIndex -> Int
hashEdge obj fi1 fi2 = round $ 100 * (x + 3*y + 5*z + 7*nx + 11*ny + 13*nz + 17*u + 19*v)
  where
  (V3 x y z)    = grabLocation obj fi1 + grabLocation obj fi2
  (V3 nx ny nz) = fromMaybe zero (grabNormal obj fi1) + fromMaybe zero (grabNormal obj fi2)
  (V2 u v)      = fromMaybe zero (grabTexCoords obj fi1) + fromMaybe zero (grabTexCoords obj fi2)

mkEdgeCache :: WavefrontOBJ -> Map.HashMap Int Int
mkEdgeCache obj@WavefrontOBJ { objFaces } = V.foldl' (\s (elValue -> (Face one two three _))
    -> Map.insertWith (+) (hashEdge obj one two) 1
    $  Map.insertWith (+) (hashEdge obj one three) 1
    $  Map.insertWith (+) (hashEdge obj two three) 1
    s
    ) Map.empty objFaces
