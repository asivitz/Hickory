{-# LANGUAGE DuplicateRecordFields #-}

module Hickory.FRP.UI where

import Hickory.Input
import Hickory.Math.Vector
import Hickory.Types
import Linear (V2(..), zero, (^/), (^*))
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Data.List (mapAccumL, uncons, findIndex, sortOn)
import Data.Tuple (swap)
import Reactive.Banana ((<@>), MonadMoment)
import Data.Maybe (mapMaybe, catMaybes, isNothing)
import Linear.Metric ( distance, norm )
import Control.Lens (over, each, _1, _2, _3, view)
import Hickory.Utils.Utils (modifyAt)
import Data.Time (NominalDiffTime)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))

data InputTarget a = InputTarget
  { loc            :: V2 Scalar
  , target         :: Scalar
  , value          :: a
  , children       :: [InputTarget a]
  , transformPoint :: Bool
  , offset         :: Bool -- Offset touch events based on original touch down offset
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
  (e,b) <- B.mapAccum ([] :: [(Int, (a, V2 Scalar -> V2 Scalar, V2 Scalar))]) $ behavAccum <$> targets <@> touchEvs
  pure (e, fmap (view _1 . snd) <$> b)
  where

  behavAccum :: [InputTarget a] -> [TouchEvent] -> [(Int, (a, V2 Scalar -> V2 Scalar, V2 Scalar))] -> ([(Maybe a, TouchEvent)], [(Int, (a, V2 Scalar -> V2 Scalar, V2 Scalar))])
  behavAccum targs tevs focused = swap $ mapAccumL perTouch focused tevs
    where

    perTouch :: [(Int, (a, V2 Scalar -> V2 Scalar, V2 Scalar))] -> TouchEvent -> ([(Int, (a, V2 Scalar -> V2 Scalar, V2 Scalar))], (Maybe a, TouchEvent))
    perTouch cache te@TouchEvent {..} = case lookup touchIdent focused of
      Just (a, xform, offset) -> case eventType of
        Down -> (cache, (Just a, TouchEvent touchIdent (xform (loc - offset)) Down))
        Up dur -> (dropWhile ((==touchIdent) . fst) cache, (Just a, TouchEvent touchIdent (xform (loc - offset)) (Up dur)))
        Loc  -> (cache, (Just a, TouchEvent touchIdent (xform (loc - offset)) Loc))
      Nothing -> case eventType of
        Down -> case headMay . sortOn (norm . view _3) $ mapMaybe (hitTarget loc) targs of
          Just (a,xform, offset)  -> ((touchIdent,(a,xform,offset)):cache, (Just a, TouchEvent touchIdent (xform (loc - offset)) Down ))
          Nothing         -> (cache, (Nothing, te))
        _ -> (cache, (Nothing, te))

    hitTarget :: V2 Scalar -> InputTarget a -> Maybe (a, V2 Scalar -> V2 Scalar, V2 Scalar)
    hitTarget v InputTarget {..} =
      if distance v loc < target
      then headMay . reverse . over (each . _2) xform $ (value, id, if offset then v - loc else zero) : mapMaybe (hitTarget (v - loc)) children
      else Nothing
      where
      xform = if transformPoint then ((\x -> x - loc).) else id


data VelocityObject a = VelocityObject
  { item        :: a
  , offset      :: V2 Scalar
  , posHistory  :: Seq.Seq (NominalDiffTime, V2 Scalar)
  , velocity    :: Maybe (V2 Scalar)
  }
-- Given input targets and touch events, create new events for the hit targets
-- Targets will slide with momentum and emit up events until they stop
velocityTargetLayer
  :: forall a m. (Eq a, MonadMoment m)
  => B.Behavior [InputTarget a]
  -> B.Event [TouchEvent]
  -> B.Event NominalDiffTime
  -> m (B.Event [(Maybe a, TouchEvent)], B.Behavior [a])
velocityTargetLayer targets touchEvs tickEv = do
  time <- B.accumB 0 ((+) <$> tickEv)
  (e,b) <- B.mapAccum (mempty :: Map.Map Int (VelocityObject a)) $
    B.unionWith combineEvents (touchAccum <$> targets <*> time <@> touchEvs) (timeAccum <$> tickEv)

  pure (e, map item . filter (isNothing . velocity) . Map.elems <$> b)
  where
  initVelMult = 0.5
  dampening   = 4

  combineEvents f1 f2 acc =
    let (x', acc')   = f1 acc
        (x'', acc'') = f2 acc'
    in (x' <> x'', acc'')

  timeAccum :: NominalDiffTime -> Map.Map Int (VelocityObject a) -> ([(Maybe a, TouchEvent)], Map.Map Int (VelocityObject a))
  timeAccum time cache = Map.mapAccum (\lst vo -> (maybe id (:) (genEv vo) lst, move vo)) [] (Map.filter filt cache)
    where
    genEv VelocityObject { velocity = Nothing } = Nothing
    genEv VelocityObject { item, velocity = Just _, posHistory = (_, v) :<| _ } = Just (Just item, TouchEvent (-1) v (Up 0))
    genEv VelocityObject { velocity = Just _, posHistory = Seq.Empty } = Nothing
    move vo@VelocityObject { velocity = Nothing }  = vo
    move vo@VelocityObject { velocity = Just vel, posHistory = ph@((time', v') :<| _) }
      = vo { velocity = Just (vel ^* max 0 (1 - realToFrac time * dampening))
           , posHistory = (time' + time, v' + (vel ^* realToFrac time)) :<| ph }
    move VelocityObject { posHistory = Seq.Empty } = error "Impossible"
    filt VelocityObject { velocity = Nothing }  = True
    filt VelocityObject { velocity = Just vel } = norm vel > 0.1

  touchAccum :: [InputTarget a] -> NominalDiffTime -> [TouchEvent] -> Map.Map Int (VelocityObject a) -> ([(Maybe a, TouchEvent)], Map.Map Int (VelocityObject a))
  touchAccum targs time tevs cache_ = swap $ mapAccumL perTouch cache_ tevs
    where

    perTouch :: Map.Map Int (VelocityObject a) -> TouchEvent -> (Map.Map Int (VelocityObject a), (Maybe a, TouchEvent))
    perTouch cache te@TouchEvent {..} = case eventType of
      Down -> let hit = headMay . sortOn (norm . snd) $ mapMaybe (hitTarget loc) targs in
        case hit of
          Just (hitItem, hitOffset) -> ( Map.insert touchIdent (VelocityObject hitItem hitOffset (Seq.singleton (time, loc - hitOffset)) Nothing)
                                         (Map.filter ((/=hitItem) . item) cache)
                                       , (Just hitItem, TouchEvent touchIdent (loc - hitOffset) Down)
                                       )
          Nothing -> (cache, (Nothing, te))
      Loc  -> case cached of
        Just VelocityObject {velocity = Nothing, offset, item, posHistory} ->
          (Map.adjust (\vo -> vo { posHistory = (time, loc - offset) :<| posHistory }) touchIdent cache, (Just item, TouchEvent touchIdent (loc - offset) Loc))
        _ -> (cache, (Nothing, te))
      Up dur -> case cached of
        Just VelocityObject {velocity = Nothing, offset, item } ->
          ( Map.adjust (\vo@VelocityObject { posHistory = ph }
                              -> let _ :|> (time', v') = Seq.take 10 ph
                                 in vo { posHistory = (time, loc - offset) :<| ph
                                    , velocity = Just ((loc - offset - v') ^/ (realToFrac (time - time') / initVelMult)) }) touchIdent cache
          , (Just item, TouchEvent touchIdent (loc - offset) (Up dur)))
        _ -> (cache, (Nothing, te))
      where
      cached = Map.lookup touchIdent cache

    hitTarget :: V2 Scalar -> InputTarget a -> Maybe (a, V2 Scalar)
    hitTarget v InputTarget {..} =
      if distance v loc < target
      then headMay . reverse $ (value, if offset then v - loc else zero) : mapMaybe (hitTarget (v - loc)) children
      else Nothing

headMay :: [a] -> Maybe a
headMay = fmap fst . uncons

data TouchChange
 = AddTouch  TouchIdent Int (V2 Scalar)
 | LocTouch  TouchIdent Int (V2 Scalar)
 | LoseTouch TouchIdent Int (V2 Scalar)
 deriving Show

-- |Track the state of each touch and generate events as they enter/leave
-- Events include the order in which the touch entered (0th or 1st touch, etc.)
trackTouches :: B.MonadMoment m => B.Event [TouchEvent] -> m (B.Event [TouchChange], B.Behavior [(TouchIdent, V2 Scalar)])
trackTouches = B.mapAccum [] . fmap f
  where
  f :: [TouchEvent] -> [(TouchIdent, V2 Scalar)] -> ([TouchChange], [(TouchIdent, V2 Scalar)])
  f tes state = foldr h ([], state) tes

  h :: TouchEvent -> ([TouchChange], [(TouchIdent, V2 Scalar)]) -> ([TouchChange], [(TouchIdent, V2 Scalar)])
  h te (tcs, st) = let (tcs', st') = g te st in (tcs ++ tcs', st')

  g :: TouchEvent -> [(TouchIdent, V2 Scalar)] -> ([TouchChange], [(TouchIdent, V2 Scalar)])
  g TouchEvent {..} state = case eventType of
    Down -> addOrUpdate touchIdent loc state
    Loc  -> (catMaybes [(\idx -> LocTouch touchIdent idx loc) <$> findIndex ((==touchIdent) . fst) state], update touchIdent loc state)
    Up _dur -> (catMaybes [(\idx -> LoseTouch touchIdent idx loc) <$> findIndex ((==touchIdent) . fst) state], filter ((/=touchIdent) . fst) state)

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
