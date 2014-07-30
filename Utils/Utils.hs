module Utils.Utils where

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe a f = maybe (return ()) f a

justOrM :: Monad m => a -> Maybe a -> m a
justOrM def val = return $ maybe def id val
