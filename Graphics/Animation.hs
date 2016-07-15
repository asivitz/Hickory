module Graphics.Animation where

import Math.Vector
import Linear.Quaternion
import Data.List
import qualified Utils.BVH as BVH

data Frame a = Joint String (V3 a) (Quaternion a) [Frame a]
             | End (V3 a)
             deriving (Show)

data Animation a = Animation {
        numFrames :: Integer,
        frameTime :: Double,
        frames :: [Frame a]
        }
        deriving (Show)

bvhToAnimation :: BVH.BVH -> Animation Double
bvhToAnimation (BVH.BVH joint (BVH.Motion nFrames fTime fs)) =
        Animation nFrames fTime (map (\f -> let ([], frame) = animateJoint f joint in frame) fs)

animateJoint :: [Double] -> BVH.Joint -> ([Double], Frame Double)
animateJoint (zr:yr:xr:rest) (BVH.Joint 3 name (x,y,z) children) = (leftOverChannels, Joint name (V3 x y z) (axisAngle (unit _z) zr * axisAngle (unit _y) yr * axisAngle (unit _x) xr) childJoints)
    where (leftOverChannels, childJoints) = mapAccumL animateJoint rest children
animateJoint (xp:yp:zp:zr:yr:xr:rest) (BVH.Joint 6 name (x,y,z) children) = (leftOverChannels, Joint name (V3 (x + xp) (y + yp) (z + zp)) (axisAngle (unit _z) zr * axisAngle (unit _y) yr * axisAngle (unit _x) xr) childJoints)
    where (leftOverChannels, childJoints) = mapAccumL animateJoint rest children
animateJoint rest (BVH.JointEnd (x,y,z)) = (rest, End (V3 x y z))
animateJoint _ _ = error "Couldn't convert BVH to animation"
