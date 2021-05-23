module Hickory.Graphics.Wavefront where

import Data.List
import Data.Maybe
import Data.Functor ((<&>))
import Hickory.Graphics.GLSupport
import Hickory.Graphics.DrawUtils (loadVAOObj)
import Linear (V3(..))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Codec.Wavefront (WavefrontOBJ(..), Location(..), TexCoord(..), Normal(..), Face(..), FaceIndex(..), elValue)
import qualified Codec.Wavefront as Wavefront

import Graphics.GL.Compatibility41 as GL
import Hickory.Graphics.VAO (createVAOConfig, VAOObj(..))
import qualified Data.HashMap.Strict as Map
import Control.Monad.State.Strict (State, execState, modify, gets)
import Data.Hashable (Hashable(..))

-- Pack a WavefrontOBJ into vertices and indices
-- Configurable for different data (positions, normals, etc.)
packOBJ :: WavefrontOBJ -> [WavefrontOBJ -> FaceIndex -> V.Vector GLfloat] -> (SV.Vector GLfloat, SV.Vector GLushort)
packOBJ obj@WavefrontOBJ { objFaces } fs = (SV.convert verts, SV.convert indices)
  where
  indices = V.concatMap (\(Face one two three _) -> V.fromList $ mapMaybe (fmap fromIntegral . (`V.elemIndex` allFaceIndices)) [one,two,three]) faces
  verts :: V.Vector GLfloat
  verts = V.concatMap (\fi -> V.concat $ map ($fi) packFuns) allFaceIndices
  packFuns = map ($obj) fs
  faces = fmap elValue objFaces
  allFaceIndices :: V.Vector FaceIndex
  allFaceIndices = V.fromList . nub . concat $ faces <&> \(Face one two three xtras) -> one : two : three : xtras

packNormals :: WavefrontOBJ -> FaceIndex -> V.Vector GLfloat
packNormals WavefrontOBJ { objNormals } (FaceIndex _ _ mni) = case mni >>= (objNormals V.!?) . pred of
  Nothing -> V.fromList [0, 0, 0]
  Just (Normal x y z) -> V.fromList [x, y, z]

packLocations :: WavefrontOBJ -> FaceIndex -> V.Vector GLfloat
packLocations WavefrontOBJ { objLocations } (FaceIndex loci _ _) = case objLocations V.!? pred loci of
  Nothing -> V.fromList [0, 0, 0]
  Just (Location x y z _) -> V.fromList [x, y, z]

packTexCoords :: WavefrontOBJ -> FaceIndex -> V.Vector GLfloat
packTexCoords WavefrontOBJ { objTexCoords } (FaceIndex _ mtci _) = case mtci >>= (objTexCoords V.!?) . pred of
  Nothing -> V.fromList [0, 0]
  Just (TexCoord u v _) -> V.fromList [u, v]

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
        hide12 = if numFacesContaining one two > 1 then basis3 else 0
        hide23 = if numFacesContaining two three > 1 then basis1 else 0
        hide13 = if numFacesContaining one three > 1 then basis2 else 0

    setCoordOfIndex one c1
    setCoordOfIndex two c2
    setCoordOfIndex three c3
    pure ()
  basisForColor = \case
    Just Red   -> V3 1 0 0
    Just Green -> V3 0 1 0
    Just Blue  -> V3 0 0 1
    Nothing -> error "Can't find color"
  numFacesContaining :: FaceIndex -> FaceIndex -> Int
  numFacesContaining a b = V.sum $ V.map (\f -> let fis = faceIndices (elValue f) in if a `elem` fis && b `elem` fis then 1 else 0) objFaces
  faceIndices (Face one two three is) = one : two : three : is
  setCoordOfIndex :: FaceIndex -> V3 Float -> State (Map.HashMap FaceIndex (V3 Float)) ()
  setCoordOfIndex k = modify . Map.insert k
  colors = colorObj obj

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

loadWavefront :: String -> IO WavefrontOBJ
loadWavefront path = Wavefront.fromFile path >>= \case
  Left err -> error err
  Right obj -> pure obj

-- Standard config of positions, tex coords, and normals
vaoFromWavefront :: Shader -> WavefrontOBJ -> IO VAOObj
vaoFromWavefront shader obj = do
  config <- createVAOConfig shader
    [VertexGroup [Attachment "position" 3, Attachment "texCoords" 2, Attachment "normal" 3]]
  loadVAOObj config Triangles (packOBJ obj [packLocations, packTexCoords, packNormals])

vaoFromWavefrontWithBarycentric :: Shader -> WavefrontOBJ -> IO VAOObj
vaoFromWavefrontWithBarycentric shader obj = do
  config <- createVAOConfig shader
    [VertexGroup [Attachment "position" 3, Attachment "texCoords" 2, Attachment "normal" 3, Attachment "barycentric" 3]]
  loadVAOObj config Triangles (packOBJ obj [packLocations, packTexCoords, packNormals, packBarycentricCoords])
