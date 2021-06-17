{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Hickory.Graphics.DrawUtils where

import Hickory.Types

import Hickory.Math.Matrix
import Hickory.Math.Vector
import Hickory.Graphics.GLSupport
import Data.Foldable (toList)
import Linear (V3(..), V4(..), scaled, M44, (!*!))
import qualified Data.Vector.Storable as SV

import Graphics.GL.Compatibility41 as GL
import Hickory.Graphics.VAO (VAOConfig, createIndexedVAOConfig, VAOObj(..), loadVerticesIntoIndexedVAOConfig, loadVerticesIntoDirectVAOConfig)

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

cubeFloats :: SV.Vector GLfloat
cubeFloats = SV.fromList . concatMap toList $ verts
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

cubeIndices :: SV.Vector GLushort
cubeIndices = SV.fromList [6, 7, 5, 4, 0, 7, 3, 6, 2, 5, 1, 0, 2, 3]

loadVAOObjIndexed :: VAOConfig -> DrawType -> (SV.Vector GLfloat, SV.Vector GLushort) -> IO VAOObj
loadVAOObjIndexed vaoconfig drawType (verts, indices) = do
  loadVerticesIntoIndexedVAOConfig vaoconfig verts indices
  return $ VAOObj vaoconfig (SV.length indices) drawType

loadVAOObjDirect :: VAOConfig -> DrawType -> (SV.Vector GLfloat, Int) -> IO VAOObj
loadVAOObjDirect vaoconfig drawType (verts, numTris) = do
  loadVerticesIntoDirectVAOConfig vaoconfig verts
  return $ VAOObj vaoconfig numTris drawType

loadCubeIntoVAOConfig :: VAOConfig -> IO VAOObj
loadCubeIntoVAOConfig vaoconfig = do
  let floats  = cubeFloats
      indices = cubeIndices
  loadVAOObjIndexed vaoconfig TriangleStrip (floats, indices)

loadInvertedCubeIntoVAOConfig :: VAOConfig -> IO VAOObj
loadInvertedCubeIntoVAOConfig vaoconfig = do
  loadVAOObjIndexed vaoconfig TriangleStrip (cubeFloats, SV.reverse cubeIndices)

mkCubeVAOObj :: Shader -> IO VAOObj
mkCubeVAOObj shader = createIndexedVAOConfig shader [VertexGroup [Attachment "position" 3]] >>= loadCubeIntoVAOConfig

mkInvertedCubeVAOObj :: Shader -> IO VAOObj
mkInvertedCubeVAOObj shader = createIndexedVAOConfig shader [VertexGroup [Attachment "position" 3]] >>= loadInvertedCubeIntoVAOConfig

mkSquareVerts :: (SV.Storable t, SV.Storable t1, Num t, Fractional t1) => t1 -> t1 -> (SV.Vector t1, SV.Vector t, DrawType)
mkSquareVerts texW texH = (SV.fromList floats, SV.fromList indices, TriangleFan)
  where h = 0.5
        l = -h
        floats = [l, h, 0, texH,
                  l, l, 0, 0,
                  h, l, texW, 0,
                  h, h, texW, texH]
        indices = [0,1,2,3]

loadSquareIntoVAOConfig :: Scalar -> VAOConfig -> IO VAOObj
loadSquareIntoVAOConfig sideLength vaoconfig = loadVAOObjIndexed vaoconfig TriangleFan (floats, indices)
  where h = realToFrac sideLength
        l = -h
        floats = SV.fromList [l, h, 0, 0, 1, 0, 1,
                            l, l, 0, 0, 1, 0, 0,
                            h, l, 0, 0, 1, 1, 0,
                            h, h, 0, 0, 1, 1, 1]
        indices = SV.fromList [0,1,2,3]

mkSquareVAOObj :: Scalar -> Shader -> IO VAOObj
mkSquareVAOObj sideLength shader = createIndexedVAOConfig shader [VertexGroup [Attachment "position" 2, Attachment "normal" 3, Attachment "texCoords" 2]]
                               >>= loadSquareIntoVAOConfig sideLength

loadUntexturedSquareIntoVAOConfig :: VAOConfig -> IO VAOObj
loadUntexturedSquareIntoVAOConfig vaoconfig = do
  let h = 0.5
      l = -h
      floats = [l, h,
                l, l,
                h, l,
                h, h]
      indices = [0,1,2,3]

  loadVerticesIntoIndexedVAOConfig vaoconfig (SV.fromList floats) (SV.fromList indices)

  return (VAOObj vaoconfig (length indices) TriangleFan)

mkUntexturedSquareVAOObj :: Shader -> IO VAOObj
mkUntexturedSquareVAOObj shader = createIndexedVAOConfig shader [VertexGroup [Attachment "position" 2]] >>= loadUntexturedSquareIntoVAOConfig

combineMats :: Maybe (M44 Scalar) -> Maybe (M44 Scalar) -> Maybe (M44 Scalar)
combineMats Nothing Nothing = Nothing
combineMats Nothing (Just y) = Just y
combineMats (Just x) Nothing = Just x
combineMats (Just x) (Just y) = Just (x !*! y)
