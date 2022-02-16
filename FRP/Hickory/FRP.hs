module Hickory.FRP where

import Control.Applicative (liftA2)
import Reactive.Banana (Behavior, Event, (<@>))

-- Helpers


foldE :: (a -> b -> a) -> a -> [b] -> a
foldE x = foldr (flip x)

noEvs :: (a -> b -> (a, [c])) -> a -> b -> a
noEvs f a b = fst $ f a b


-- Unused

splitPair :: Event (a, b) -> (Event a, Event b)
splitPair e = (fst <$> e, snd <$> e)

foldEvents :: Behavior b -> (b -> a -> b) -> Event [a] -> Event b
foldEvents b h e = fmap (foldE h) b <@> e

foldEvents' :: Behavior (a -> b -> a) -> Behavior a -> Event [b] -> Event a
foldEvents' bf b e = foldE <$> bf <*> b <@> e

foldEventEmitter :: (a -> b -> (a, [c])) -> a -> [b] -> (a, [c])
foldEventEmitter f a = foldr (flip $ evFolder f) (a, [])

evFolder :: (a -> b -> (a, [c])) -> (a, [c]) -> b -> (a, [c])
evFolder f (a, cs) b = (<> cs) <$> f a b

-- useful for [Behavior (Resources -> RenderTree)] -> Behavior (Resources -> [RenderTree])
combineRenderFuncs :: Applicative f => [f (a -> b)] -> f (a -> [b])
combineRenderFuncs = foldr (liftA2 go) (pure $ const [])
  where go :: (a -> b) -> (a -> [b]) -> a -> [b]
        go f fs r = f r : fs r
