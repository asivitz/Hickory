module Hickory.Math.Bezier where

import Linear ((^*))

-- |Define a cubic bezier curve with a start, an end, and two interior control points
-- t runs from 0 (at the start) to 1 (at the end)
cubic :: (Functor f, Num a, Num (f a))
      => f a
      -> f a
      -> f a
      -> f a
      -> a
      -> f a
cubic start control1 control2 end t
  = start ^* omt_cu
  + control1 ^* (3 * omt_sq * t)
  + control2 ^* (3 * oneminust * tsq)
  + end ^* tcu
  where
  tsq = t * t
  tcu = tsq * t
  oneminust = 1 - t
  omt_sq = oneminust * oneminust
  omt_cu = omt_sq * oneminust

-- |This is the derivative of the `cubic` function
cubicDerivative :: (Functor f, Num a, Num (f a))
      => f a
      -> f a
      -> f a
      -> f a
      -> a
      -> f a
cubicDerivative start control1 control2 end t
  = start ^* omt_cu'
  + control1 ^* (3 * (omt_sq + omt_sq' * t))
  + control2 ^* (3 * (oneminust * tsq' + omt' * tsq))
  + end ^* tcu'
  where
  tsq = t * t
  tsq' = 2 * t
  tcu' = 3 * tsq
  oneminust = 1 - t
  omt_sq = oneminust * oneminust
  omt_sq' = 2 * oneminust * omt'
  omt_cu' = 3 * omt_sq * omt'
  omt' = -1
