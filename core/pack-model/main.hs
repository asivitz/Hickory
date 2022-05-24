module Main where

import System.Environment
import System.FilePath
import Hickory.ModelLoading.Packed
import Hickory.ModelLoading.Wavefront

main :: IO ()
main = do
  getArgs >>= \case
    [inf, outf] -> case takeExtension inf of
      ".obj" -> do
        wf <- loadWavefront inf
        writeToFile outf (wavefrontToModelData wf)
      _ -> putStrLn "Unknown extension. I accept wavefront models (.obj)"
    _ -> do
      putStrLn "pack-model turns a wavefront model into a Hickory-native format"
      putStrLn ""
      putStrLn "Usage: pack-model [wavefront model path] [out path]"
