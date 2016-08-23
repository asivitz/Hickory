{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Shader (
              loadShader
              )
              where

import Foreign.Storable
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import GLInterface.GLSupport
import Graphics.Drawing
import Control.Monad

loadShader resPath vert frag = do
   let prefix = resPath ++ "/Shaders/"
       vpath = prefix ++ vert
       fpath = prefix ++ frag
   vsource <- shaderSourceForPath vpath
   fsource <- shaderSourceForPath fpath

   vs <- compileVertShader vsource
   fs <- compileFragShader fsource

   case vs of
       Just vsh -> case fs of
                       Just fsh -> do
                           prog <- buildShaderProgram vsh fsh
                           case prog of
                               Just pr -> return pr
                               Nothing -> do
                                   glDeleteShader vsh
                                   glDeleteShader fsh
                                   error $ "Failed to link shader program: " ++ vpath ++ "-" ++ fpath
                       Nothing -> error $ "Couldn't load frag shader: " ++ fpath
       Nothing -> error $ "Couldn't load vertex shader: " ++ vpath

shaderSourceForPath path = do
        source <- readFile path
        -- TODO: iOS shouldn't have this header
        return $ "#version 150\n" ++ source
