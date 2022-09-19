module Hickory.FRP.UI where

import Hickory.Input
import Hickory.Math.Vector
import Hickory.Types
import Linear (V2(..))
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Data.List (mapAccumL, uncons, findIndex)
import Data.Tuple (swap)
import Reactive.Banana ((<@>), MonadMoment)
import Data.Maybe (mapMaybe, catMaybes, isNothing)
import Linear.Metric (distance)
import Control.Lens (over, each, _2)
import Hickory.Utils.Utils (modifyAt)

data InputTarget a = InputTarget
  { loc            :: V2 Scalar
  , target         :: Scalar
  , value          :: a
  , children       :: [InputTarget a]
  , transformPoint :: Bool
  }

bottomRight :: (Real a1, Fractional a2) => a2 -> a2 -> Size a1 -> V2 a2
bottomRight x y (Size w h) = V2 (realToFrac w - x) (realToFrac h - y)

bottomLeft :: (Real a1, Fractional a2) => a2 -> a2 -> Size a1 -> V2 a2
bottomLeft x y (Size _w h) = V2 x (realToFrac h - y)

topRight :: (Real a1, Fractional a2) => a2 -> a2 -> Size a1 -> V2 a2
topRight x y (Size w _h) = V2 (realToFrac w - x) y

topLeft :: (Real a1, Fractional a2) => a2 -> a2 -> Size a1 -> V2 a2
topLeft x y (Size _w _h) = V2 x y

topMiddle :: (Fractional a1, Real a2) => a1 -> Size a2 -> V2 a1
topMiddle y (Size w _h) = V2 (realToFrac w/2) y

bottomMiddle :: (Fractional a1, Real a2) => a1 -> Size a2 -> V2 a1
bottomMiddle y (Size w h) = V2 (realToFrac w/2) (realToFrac h - y)

middle :: (Fractional b, Real a) => Size a -> V2 b
middle (Size w h) = V2 (realToFrac w/2) (realToFrac h/2)


-- Given input targets and touch events, create new events for the hit
-- targets (along with locally transformed touch events)
inputLayer :: forall a m. MonadMoment m => B.Behavior [InputTarget a] -> B.Event [TouchEvent] -> m (B.Event [(Maybe a, TouchEvent)], B.Behavior [a])
inputLayer targets touchEvs = do
  (e,b) <- B.mapAccum ([] :: [(Int, (a, V2 Scalar -> V2 Scalar))]) $ behavAccum <$> targets <@> touchEvs
  pure (e, fmap (fst . snd) <$> b)
  where

  behavAccum :: [InputTarget a] -> [TouchEvent] -> [(Int, (a, V2 Scalar -> V2 Scalar))] -> ([(Maybe a, TouchEvent)], [(Int, (a, V2 Scalar -> V2 Scalar))])
  behavAccum targs tevs focused = swap $ mapAccumL perTouch focused tevs
    where

    perTouch :: [(Int, (a, V2 Scalar -> V2 Scalar))] -> TouchEvent -> ([(Int, (a, V2 Scalar -> V2 Scalar))], (Maybe a, TouchEvent))
    perTouch cache te = case lookup (touchIdent te) focused of
      Just (a, xform) -> case te of
        Down i v -> (cache, (Just a, Down i $ xform v))
        Up   i v -> (dropWhile ((==i) . fst) cache, (Just a, Up i $ xform v))
        Loc  i v -> (cache, (Just a, Loc i $ xform v))
      Nothing -> case te of
        Down i v -> case headMay $ mapMaybe (hitTarget v) targs of
          Just (a,xform)  -> ((i,(a,xform)):cache, (Just a, Down i $ xform v))
          Nothing         -> (cache, (Nothing, te))
        _ -> (cache, (Nothing, te))

    hitTarget :: V2 Scalar -> InputTarget a -> Maybe (a, V2 Scalar -> V2 Scalar)
    hitTarget v InputTarget {..} =
      if distance v loc < target
      then headMay . reverse . over (each . _2) xform $ (value, id) : mapMaybe (hitTarget (v - loc)) children
      else Nothing
      where
      xform = if transformPoint then ((\x -> x - loc).) else id

headMay :: [a] -> Maybe a
headMay = fmap fst . uncons

data TouchChange
 = AddTouch  TouchIdent Int (V2 Scalar)
 | LoseTouch TouchIdent Int (V2 Scalar)
 deriving Show

-- |Track the state of each touch and generate events as they enter/leave
-- Events include the order in which the touch entered (0th or 1st touch, etc.)
trackTouches :: B.Event [TouchEvent] -> B.MomentIO (B.Event [TouchChange], B.Behavior [(TouchIdent, V2 Scalar)])
trackTouches = B.mapAccum [] . fmap f
  where
  f :: [TouchEvent] -> [(TouchIdent, V2 Scalar)] -> ([TouchChange], [(TouchIdent, V2 Scalar)])
  f tes state = foldr h ([], state) tes

  h :: TouchEvent -> ([TouchChange], [(TouchIdent, V2 Scalar)]) -> ([TouchChange], [(TouchIdent, V2 Scalar)])
  h te (tcs, st) = let (tcs', st') = g te st in (tcs ++ tcs', st')

  g :: TouchEvent -> [(TouchIdent, V2 Scalar)] -> ([TouchChange], [(TouchIdent, V2 Scalar)])
  g te state = case te of
    Down i v -> addOrUpdate i v state
    Loc  i v -> ([], update i v state)
    Up   i v -> (catMaybes [(\idx -> LoseTouch i idx v) <$> findIndex ((==i) . fst) state], filter ((/=i) . fst) state)

  update k v = \case
    (x:xs) -> if k == fst x
              then (k, v) : xs
              else x : update k v xs
    [] -> []

  addOrUpdate :: TouchIdent -> V2 Scalar -> [(TouchIdent, V2 Scalar)] -> ([TouchChange], [(TouchIdent, V2 Scalar)])
  addOrUpdate k v state = case findIndex ((==k) . fst) state of
    Just i  -> ([], modifyAt (const (k,v)) i state)
    Nothing -> ([AddTouch k (length state) v], state ++ [(k, v)])

unclaimedTouches :: B.Event [(Maybe c, TouchEvent)] -> B.Event [TouchEvent]
unclaimedTouches = B.filterE (not . null) . fmap (fmap snd . filter (isNothing . fst))
