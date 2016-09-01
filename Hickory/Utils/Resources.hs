module Hickory.Utils.Resources where

import Data.Hashable
import qualified Data.HashMap.Strict as HashMap

type RefStore a b = HashMap.HashMap a (b, Int)

emptyRefStore :: RefStore a b
emptyRefStore = HashMap.empty

reserve :: (Hashable a, Eq a, Monad m) => RefStore a b -> a -> (a -> m (Maybe b)) -> m (RefStore a b, Maybe b)
reserve store name loadfunc = do
        let val = HashMap.lookup name store
        case val of
            Nothing -> do
                val' <- loadfunc name
                case val' of
                    Nothing -> return (store, Nothing)
                    Just v -> return $ (HashMap.insert name (v, 0) store, Just v)
            Just (val', i) -> do
                return $ (HashMap.insert name (val', i + 1) store, Just val')

release :: (Hashable a, Eq a, Monad m) => RefStore a b -> a -> (b -> m ()) -> m (RefStore a b)
release store name deletefunc = do
        let val = HashMap.lookup name store
        case val of
            Nothing -> return store
            Just (val', 0) -> do
                deletefunc val'
                return $ (HashMap.delete name store)
            Just (val', i) -> do
                return $ (HashMap.insert name (val', i - 1) store)
