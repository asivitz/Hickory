{-# LANGUAGE CPP #-}

module Hickory.Utils.Utils where

import Data.List
import Data.Ord
import Data.Maybe
import Data.IORef
import Data.Time
import qualified Data.Text.IO as TextIO
import qualified Debug.Trace

#if defined(ghcjs_HOST_OS)
import qualified Data.Text as Text
import JavaScript.Web.XMLHttpRequest
import Data.JSString (unpack, pack, JSString)
#endif


tracer :: (Show a, Show b) => b -> a -> a
tracer label a = Debug.Trace.traceShow (label, a) a

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

replace :: (a -> Bool) -> a -> [a] -> [a]
replace p a (x:xs) | p x = a:xs
replace p a (x:xs) = x : replace p a xs
replace p a [] = []

chopBy :: Int -> [a] -> [[a]]
chopBy num [] = []
chopBy num lst = take num lst : chopBy num (drop num lst)

makeFPSTicker :: IO (IO Double)
makeFPSTicker = do
        initial_time <- getCurrentTime
        ref <- newIORef (0, initial_time, 0)

        return $ do
            new_time <- getCurrentTime
            (count, last_time, last_report) <- readIORef ref
            let diff = diffUTCTime new_time last_time
            if diff > 1
                then do
                    let report = realToFrac $ count / diff
                    writeIORef ref (0, new_time, report)
                    return report
                else do
                    writeIORef ref (count+1, last_time, last_report)
                    return last_report

#if defined(ghcjs_HOST_OS)
readFileAsText path = do
        resp <- xhr Request { reqMethod = GET, reqURI = pack path, reqLogin = Nothing, reqHeaders = [], reqWithCredentials = False, reqData = NoData }
        return . Text.pack . unpack . fromJust $ contents resp
#else
readFileAsText = TextIO.readFile
#endif
