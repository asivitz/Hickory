{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Hickory.Graphics.DrawUtils where

import Hickory.Types

import Control.Lens ((^.))
import Hickory.Math.Matrix
import Hickory.Math.Vector
import Data.Maybe
import Hickory.Graphics.GLSupport
import Data.Foldable (toList, forM_)
import Linear (V3(..), V4(..), scaled, M44, identity, (!*!), _m33, inv33, transpose)
import qualified Data.Vector.Storable as SV
import Hickory.Graphics.Types (DrawSpec(..), RenderTree(..))

import Hickory.Graphics.Drawing
import Graphics.GL.Compatibility41 as GL
import Hickory.Graphics.VAO (VAOConfig, createVAOConfig, VAOObj(..), loadVerticesIntoVAOConfig)
import Hickory.Graphics.Textures (TexID)
import Hickory.Graphics.Uniforms (ShaderFunction, bindUniform)

data Command = Command Mat44 [ShaderFunction] [TexID] DrawSpec

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

loadVAOObj :: VAOConfig -> DrawType -> (SV.Vector GLfloat, SV.Vector GLushort) -> IO VAOObj
loadVAOObj vaoconfig drawType (verts, indices) = do
  loadVerticesIntoVAOConfig vaoconfig verts indices
  return $ VAOObj vaoconfig (SV.length indices) drawType

loadCubeIntoVAOConfig :: VAOConfig -> IO VAOObj
loadCubeIntoVAOConfig vaoconfig = do
  let floats  = cubeFloats
      indices = cubeIndices
  loadVAOObj vaoconfig TriangleStrip (floats, indices)

loadInvertedCubeIntoVAOConfig :: VAOConfig -> IO VAOObj
loadInvertedCubeIntoVAOConfig vaoconfig = do
  loadVAOObj vaoconfig TriangleStrip (cubeFloats, SV.reverse cubeIndices)

mkCubeVAOObj :: Shader -> IO VAOObj
mkCubeVAOObj shader = createVAOConfig shader [VertexGroup [Attachment "position" 3]] >>= loadCubeIntoVAOConfig

mkInvertedCubeVAOObj :: Shader -> IO VAOObj
mkInvertedCubeVAOObj shader = createVAOConfig shader [VertexGroup [Attachment "position" 3]] >>= loadInvertedCubeIntoVAOConfig

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
loadSquareIntoVAOConfig sideLength vaoconfig = loadVAOObj vaoconfig TriangleFan (floats, indices)
  where h = realToFrac sideLength
        l = -h
        floats = SV.fromList [l, h, 0, 0, 1, 0, 1,
                            l, l, 0, 0, 1, 0, 0,
                            h, l, 0, 0, 1, 1, 0,
                            h, h, 0, 0, 1, 1, 1]
        indices = SV.fromList [0,1,2,3]

mkSquareVAOObj :: Scalar -> Shader -> IO VAOObj
mkSquareVAOObj sideLength shader = createVAOConfig shader [VertexGroup [Attachment "position" 2, Attachment "normal" 3, Attachment "texCoords" 2]]
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

  loadVerticesIntoVAOConfig vaoconfig (SV.fromList floats) (SV.fromList indices)

  return (VAOObj vaoconfig (length indices) TriangleFan)

mkUntexturedSquareVAOObj :: Shader -> IO VAOObj
mkUntexturedSquareVAOObj shader = createVAOConfig shader [VertexGroup [Attachment "position" 2]] >>= loadUntexturedSquareIntoVAOConfig

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
runDrawCommand (Command mat uniforms texs spec) = drawSpec (stdUniforms ++ uniforms) texs spec
  where
  stdUniforms =
    [ bindUniform "modelMat" [mat]
    , bindUniform "normalMat" [Linear.transpose (inv33 $ mat ^. _m33)]
    ]

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
collectTreeSpecs parentMat (Primitive uniforms texs spec) = [Command mat' uniforms texs spec]
  where mat' = fromMaybe identity parentMat
collectTreeSpecs parentMat (List children) = concatMap (collectTreeSpecs parentMat) children -- (sortOn rtDepth children)
