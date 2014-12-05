module Utils.Utils where

import Data.List
import Data.Ord
import Data.Maybe

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

whenMaybeM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenMaybeM a f = a >>= \b -> maybe (return ()) f b

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

{-| Like sortBy, but memoizes the results of the comparison function -}
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))

{-| Like sortOn, but throws out items that get turned into Nothing -}
sortOnMaybe :: Ord b => (a -> Maybe b) -> [a] -> [a]
sortOnMaybe f = map snd . sortBy (comparing fst) . mapMaybe (\x ->
                                 case f x of
                                     Nothing -> Nothing
                                     Just a -> Just (a, x))

modifyAt :: (a -> a) -> Int -> [a] -> [a]
modifyAt f 0 (x:xs) = f x : xs
modifyAt f num (x:xs) = x : (modifyAt f (num - 1) xs)
modifyAt f num [] = []

modify :: Eq a => a -> (a -> a) -> [a] -> [a]
modify _ _ [] = []
modify a f (x:xs)
    | a == x = f x : xs
    | otherwise = x : modify a f xs

for = flip map

deleteAt :: [a] -> Int -> [a]
deleteAt [] _ = []
deleteAt (x:xs) 0 = xs
deleteAt (x:xs) n = x : (deleteAt xs (n - 1))
