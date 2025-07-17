module Main where

import System.Environment
import System.FilePath
import Hickory.ModelLoading.Wavefront
import Hickory.ModelLoading.DirectXModel (loadModelFromX, packedMesh)
import Hickory.Vulkan.Mesh (writeMeshToFile)

main :: IO ()
main = do
  getArgs >>= \case
    [inf, outf] -> case takeExtension inf of
      ".obj" -> do
        wf <- loadWavefront inf
        writeMeshToFile outf (wavefrontToMesh wf)
      ".x" -> do
        x <- loadModelFromX inf
        writeMeshToFile outf (packedMesh x)
      _ -> putStrLn "Unknown extension. I accept wavefront models (.obj)"
    _ -> do
      putStrLn "pack-model turns a wavefront model into a Hickory-native format"
      putStrLn ""
      putStrLn "Usage: pack-model [wavefront model path] [out path]"
