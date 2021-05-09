{-# LANGUAGE FlexibleInstances #-}

module Hickory.Graphics.Uniforms where

import Hickory.Graphics.Shader (Shader, retrieveLoc, UniformLoc)
import Hickory.Math.Matrix
import Linear (V3, V4, M44, M33,transpose)
import Graphics.GL.Compatibility41 as GL
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Array (withArray)
import Data.List (genericLength)

type ShaderFunction = Shader -> IO ()

bindUniform :: Uniform a => String -> a -> Shader -> IO ()
bindUniform name val shader = case retrieveLoc name shader of
  Just loc -> bindUniformLoc loc val
  Nothing -> pure ()

class Uniform a where
  bindUniformLoc :: UniformLoc -> a -> IO ()

instance Uniform Int where
  bindUniformLoc loc i = glUniform1i loc (fromIntegral i)

instance Uniform Double where
  bindUniformLoc loc f = glUniform1f loc (realToFrac f)

instance Uniform [V3 Double] where
  bindUniformLoc loc v = uniform3fv loc v

instance Uniform (V3 Double) where
  bindUniformLoc loc v = uniform3fv loc [v]

instance Uniform [V4 Double] where
  bindUniformLoc loc v = uniform4fv loc v

instance Uniform (V4 Double) where
  bindUniformLoc loc v = uniform4fv loc [v]

instance Uniform [Mat33] where
  bindUniformLoc loc m = uniformMatrix3fv loc m

instance Uniform Mat33 where
  bindUniformLoc loc m = uniformMatrix3fv loc [m]

instance Uniform [Mat44] where
  bindUniformLoc loc m = uniformMatrix4fv loc m

instance Uniform Mat44 where
  bindUniformLoc loc m = uniformMatrix4fv loc [m]

uniform4fv :: GLint -> [V4 Double] -> IO ()
uniform4fv loc vs =
  withVec4s vs $ \ptr ->
    glUniform4fv loc (genericLength vs) (castPtr ptr)

uniform3fv :: GLint -> [V3 Double] -> IO ()
uniform3fv loc vs =
  withVec3s vs $ \ptr ->
    glUniform3fv loc (genericLength vs) (castPtr ptr)

withVec4s :: [V4 Double] -> (Ptr GLfloat -> IO b) -> IO b
withVec4s vecs f = withArray (map (fmap realToFrac) vecs :: [V4 GLfloat]) (f . castPtr)

withVec3s :: [V3 Double] -> (Ptr GLfloat -> IO b) -> IO b
withVec3s vecs f = withArray (map (fmap realToFrac) vecs :: [V3 GLfloat]) (f . castPtr)

{-withVec4s vecs f = withVec4 (vecs !! 0) (f . castPtr) --withArray vecs (f . castPtr)-}

withMat44s :: [Mat44] -> (Ptr GLfloat -> IO b) -> IO b
withMat44s mats f = withArray (map (\mat -> (fmap realToFrac <$> transpose mat :: M44 GLfloat)) mats) (f . castPtr)

withMat33s :: [Mat33] -> (Ptr GLfloat -> IO b) -> IO b
withMat33s mats f = withArray (map (\mat -> (fmap realToFrac <$> transpose mat :: M33 GLfloat)) mats) (f . castPtr)

uniformMatrix4fv :: GLint -> [Mat44] -> IO ()
uniformMatrix4fv loc mats =
  withMat44s mats $ \ptr ->
    glUniformMatrix4fv loc (genericLength mats) GL_FALSE (castPtr ptr)

uniformMatrix3fv :: GLint -> [Mat33] -> IO ()
uniformMatrix3fv loc mats =
  withMat33s mats $ \ptr ->
    glUniformMatrix3fv loc (genericLength mats) GL_FALSE (castPtr ptr)
