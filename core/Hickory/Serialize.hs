module Hickory.Serialize where

import Text.Read
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Groom

writeState :: Show m => m -> IO ()
writeState = writeData "modelState.txt"

loadState :: Read a => IO a
loadState = loadData "modelState.txt"

writeData :: Show m => String -> m -> IO ()
writeData fileName m =
  Text.writeFile fileName (Text.pack $ groom m) >> print ("Wrote state" :: String)

loadData :: Read a => String -> IO a
loadData fileName = do
  s <- readEither . Text.unpack <$> Text.readFile fileName
  case s of
    Left str -> error str
    Right a -> return a
