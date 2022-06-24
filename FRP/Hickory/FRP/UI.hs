module Hickory.FRP.UI where

import Hickory.Input
import Hickory.Math.Vector
import Hickory.Types
import Linear (V2(..))
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Data.List (mapAccumL, uncons)
import Data.Tuple (swap)
import Reactive.Banana ((<@>))
import Data.Maybe (mapMaybe)
import Linear.Metric (distance)
import Control.Lens (over, each, _2)

data InputTarget a = InputTarget
  { loc      :: V2 Scalar
  , target   :: Scalar
  , value    :: a
  , children :: [InputTarget a]
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
inputLayer :: forall a. B.Behavior [InputTarget a] -> B.Event [TouchEvent] -> B.MomentIO (B.Event [(Maybe a, TouchEvent)])
inputLayer targets touchEvs =
  fmap fst <$> B.mapAccum ([] :: [(Int, (a, V2 Scalar -> V2 Scalar))]) $ behavAccum <$> targets <@> touchEvs
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
      then headMay . reverse . over (each . _2) ((\x -> x - loc).) $ (value, id) : mapMaybe (hitTarget (v - loc)) children
      else Nothing

headMay :: [a] -> Maybe a
headMay = fmap fst . uncons
