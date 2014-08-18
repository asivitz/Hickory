module Utils.Utils where

lerp :: Num a => a -> a -> a -> a
lerp fract a b = (a * (1 - fract)) + (b * fract)

clamp :: Ord a => a -> a -> a -> a
clamp a low high = min (max a low) high

rlerp :: Fractional a => a -> a -> a -> a
rlerp a low high = (a - low) / (high - low)

rlerpClamp :: (Fractional a, Ord a) => a -> a -> a -> a
rlerpClamp a low high = rlerp (clamp a low high) low high

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe a f = maybe (return ()) f a

whenMaybe2 :: Monad m => Maybe a -> Maybe b -> (a -> b -> m ()) -> m ()
whenMaybe2 a b f = case a of
                       Nothing -> return ()
                       Just a' -> maybe (return ()) (f a') b

whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing a f = case a of
                      Nothing -> f
                      Just b -> return ()

justOrM :: Monad m => a -> Maybe a -> m a
justOrM def val = return $ maybe def id val

applyList :: [a -> a] -> a -> a
applyList [] arg = arg
applyList (x:xs) arg = x (applyList xs arg)
