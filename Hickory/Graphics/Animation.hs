{-# LANGUAGE OverloadedStrings #-}

module Hickory.Graphics.Animation where

import Hickory.Math.Vector
import Hickory.Math.Matrix
import Linear.Quaternion
import Data.List
import Control.Lens ((^.))
import qualified Hickory.Utils.Bvh as BVH
import Data.Text (Text)
import Linear (V3(..), zero, liftI2, point, unit, (^*), (!*!), (!*), identity, mkTransformation, _x, _y, _z, _xyz, M44)

data Frame a
  = Joint Text (M44 a) [Frame a]
  | End (M44 a)
  deriving (Show)

data Animation a = Animation
  { numFrames :: Integer
  , frameTime :: Double
  , boundingBox :: (V3 Double, V3 Double)
  , frames :: [(Frame a, (V3 Double, V3 Double))]
  } deriving (Show)

degToRad :: Double -> Double
degToRad = (*(pi/180))

vmax :: V3 Scalar -> V3 Scalar -> V3 Scalar
vmax = liftI2 max

vmin :: V3 Scalar -> V3 Scalar -> V3 Scalar
vmin = liftI2 min

vminimum :: [V3 Scalar] -> V3 Scalar
vminimum = foldl' vmin 10000

vmaximum :: [V3 Scalar] -> V3 Scalar
vmaximum = foldl' vmax (-10000)

findJointLimits :: Frame Double -> (V3 Double, V3 Double)
findJointLimits j = case j of
  (Joint _ mat children) -> f mat children
  (End mat) -> f mat []
  where f mat chdn = (vmin local mn, vmax local mx)
          where local = (mat !* point zero) ^. _xyz
                limits = map findJointLimits chdn
                (mn, mx) = (vminimum (map fst limits), vmaximum (map snd limits))

bvhToAnimation :: BVH.BVH -> Animation Double
bvhToAnimation (BVH.BVH joint (BVH.Motion nFrames fTime fs)) =
        Animation nFrames fTime bbox framelist
    where {-modelUp = unit _y-}
          {-worldUp = unit _z-}
          {-axis = cross modelUp worldUp-}
          {-angle = vabsangle worldUp modelUp-}
          {-upVectorRot = mkRotation axis angle :: M44 Double-}
          rot = identity
          framelist = (map (\f -> let ([], frame) = animateJoint rot f joint in (frame, findJointLimits frame)) fs)
          bbox = (vminimum (map (fst . snd) framelist), vmaximum (map (snd . snd) framelist))

consumeChanData :: [Text] -> [Double] -> (Double, Double, Double) -> (M44 Double, [Double])
consumeChanData chanNames chanData (x, y, z) = (mkTransformation quat (v + V3 x y z), drop (length chanNames) chanData)
    where (v, quat) = foldl' f (zero :: V3 Double, axisAngle (unit _x) 0 :: Quaternion Double) (zip chanNames chanData :: [(Text, Double)])
          f :: (V3 Double, Quaternion Double) -> (Text, Double) -> (V3 Double, Quaternion Double)
          f (v', quat') (name, val) = case name of
                                          "Xposition" -> (v' + unit _x ^* val, quat')
                                          "Yposition" -> (v' + unit _y ^* val, quat')
                                          "Zposition" -> (v' + unit _z ^* val, quat')
                                          "Xrotation" -> (v', quat' * axisAngle (unit _x) (degToRad val))
                                          "Yrotation" -> (v', quat' * axisAngle (unit _y) (degToRad val))
                                          "Zrotation" -> (v', quat' * axisAngle (unit _z) (degToRad val))
                                          _ -> error "Unrecognized rotation axis"

animateJoint :: M44 Double -> [Double] -> BVH.Joint -> ([Double], Frame Double)
animateJoint parentMat chanData (BVH.Joint chanNames name offset children) = (leftOverChannels, Joint name mat childJoints)
    where (leftOverChannels, childJoints) = mapAccumL (animateJoint mat) rest children
          mat = parentMat !*! jointMat
          (jointMat, rest) = consumeChanData chanNames chanData offset
animateJoint parentMat rest (BVH.JointEnd (x,y,z)) = (rest, End $ parentMat !*! mkTranslation (V3 x y z))
