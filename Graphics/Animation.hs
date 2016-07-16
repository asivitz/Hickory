module Graphics.Animation where

import Math.Vector
import Math.Matrix
import Linear.Quaternion
import Data.List
import qualified Utils.BVH as BVH

data Frame a = Joint String (M44 a) [Frame a]
             | End (M44 a)
             deriving (Show)

data Animation a = Animation {
        numFrames :: Integer,
        frameTime :: Double,
        frames :: [Frame a]
        }
        deriving (Show)

degToRad :: Double -> Double
degToRad = (*(pi/180))

bvhToAnimation :: BVH.BVH -> Animation Double
bvhToAnimation (BVH.BVH joint (BVH.Motion nFrames fTime fs)) =
        Animation nFrames fTime (map (\f -> let ([], frame) = animateJoint rot f joint in frame) fs)
    where modelUp = unit _y
          worldUp = unit _z
          axis = cross modelUp worldUp
          angle = vabsangle worldUp modelUp
          upVectorRot = mkRotation axis angle :: M44 Double
          rot = identity

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
