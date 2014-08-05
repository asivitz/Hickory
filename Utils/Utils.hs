module Utils.Utils where

lerp :: Num a => a -> a -> a -> a
lerp fract a b = (a * (1 - fract)) + (b * fract)

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe a f = maybe (return ()) f a

whenMaybe2 :: Monad m => Maybe a -> Maybe b -> (a -> b -> m ()) -> m ()
whenMaybe2 a b f = case a of
                       Nothing -> return ()
                       Just a' -> maybe (return ()) (f a') b

justOrM :: Monad m => a -> Maybe a -> m a
justOrM def val = return $ maybe def id val
