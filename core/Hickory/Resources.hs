module Hickory.Resources where

import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import qualified Data.HashMap.Strict as Map
import Data.Text as Text (Text, unpack)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, readIORef)
import Data.Maybe (fromMaybe)

pull :: MonadReader r m => (r -> Map.HashMap Text a) -> Text -> m a
pull bucket k = asks (fromMaybe (error $ "Can't find resource: " ++ Text.unpack k) . Map.lookup k . bucket)

readerFromIORef :: MonadIO m => IORef r -> ReaderT r m a -> m a
readerFromIORef ref f = liftIO (readIORef ref) >>= runReaderT f
