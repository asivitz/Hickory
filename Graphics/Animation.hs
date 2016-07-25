module Graphics.Animation where

import Math.Vector
import Math.Matrix
import Linear.Metric
import Linear.Quaternion
import Data.List
import Control.Lens
import Data.Ord
import qualified Utils.BVH as BVH

data Frame a = Joint String (M44 a) [Frame a]
             | End (M44 a)
             deriving (Show)

data Animation a = Animation {
        numFrames :: Integer,
        frameTime :: Double,
        boundingBox :: (V3 Double, V3 Double),
        frames :: [(Frame a, (V3 Double, V3 Double))]
        }
        deriving (Show)

degToRad :: Double -> Double
degToRad = (*(pi/180))

vmax = liftI2 max
vmin = liftI2 min

vminimum = foldl' vmin 10000
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
        Animation nFrames fTime bbox frames
    where modelUp = unit _y
          worldUp = unit _z
          axis = cross modelUp worldUp
          angle = vabsangle worldUp modelUp
          upVectorRot = mkRotation axis angle :: M44 Double
          rot = identity
          frames = (map (\f -> let ([], frame) = animateJoint rot f joint in (frame, findJointLimits frame)) fs)
          bbox = (vminimum (map (fst . snd) frames), vmaximum (map (snd . snd) frames))

consumeChanData :: [String] -> [Double] -> (Double, Double, Double) -> (M44 Double, [Double])
consumeChanData chanNames chanData (x, y, z) = (mkTransformation quat (v + V3 x y z), drop (length chanNames) chanData)
    where (v, quat) = foldl' f (zero :: V3 Double, axisAngle (unit _x) 0 :: Quaternion Double) (zip chanNames chanData :: [(String, Double)])
          f :: (V3 Double, Quaternion Double) -> (String, Double) -> (V3 Double, Quaternion Double)
          f (v', quat') (name, val) = case name of
                                          "Xposition" -> (v' + unit _x ^* val, quat')
                                          "Yposition" -> (v' + unit _y ^* val, quat')
                                          "Zposition" -> (v' + unit _z ^* val, quat')
                                          "Xrotation" -> (v', quat' * axisAngle (unit _x) (degToRad val))
                                          "Yrotation" -> (v', quat' * axisAngle (unit _y) (degToRad val))
                                          "Zrotation" -> (v', quat' * axisAngle (unit _z) (degToRad val))

animateJoint :: M44 Double -> [Double] -> BVH.Joint -> ([Double], Frame Double)
animateJoint parentMat chanData (BVH.Joint chanNames name offset children) = (leftOverChannels, Joint name mat childJoints)
    where (leftOverChannels, childJoints) = mapAccumL (animateJoint mat) rest children
          mat = parentMat !*! jointMat
          (jointMat, rest) = consumeChanData chanNames chanData offset
animateJoint parentMat rest (BVH.JointEnd (x,y,z)) = (rest, End $ parentMat !*! mkTranslation (V3 x y z))
animateJoint _ _ _ = error "Couldn't convert BVH to animation"
