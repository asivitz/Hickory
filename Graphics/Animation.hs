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

degToRad = (*(pi/180))

bvhToAnimation :: BVH.BVH -> Animation Double
bvhToAnimation (BVH.BVH joint (BVH.Motion nFrames fTime fs)) =
        Animation nFrames fTime (map (\f -> let ([], frame) = animateJoint (mkRotation axis angle) f joint in frame) fs)
    where modelUp = unit _y
          worldUp = unit _z
          axis = cross modelUp worldUp
          angle = vabsangle worldUp modelUp

animateJoint :: M44 Double -> [Double] -> BVH.Joint -> ([Double], Frame Double)
animateJoint parentMat (zr:yr:xr:rest) (BVH.Joint 3 name (x,y,z) children) = (leftOverChannels, Joint name mat childJoints)
    where (leftOverChannels, childJoints) = mapAccumL (animateJoint mat) rest children
          mat = parentMat !*! mkTransformation (axisAngle (unit _z) (degToRad zr) * axisAngle (unit _y) (degToRad yr) * axisAngle (unit _x) (degToRad xr)) (V3 x y z)
animateJoint parentMat (xp:yp:zp:zr:yr:xr:rest) (BVH.Joint 6 name (x,y,z) children) = (leftOverChannels, Joint name mat childJoints)
    where (leftOverChannels, childJoints) = mapAccumL (animateJoint mat) rest children
          mat = parentMat !*! mkTransformation (axisAngle (unit _z) (degToRad zr) * axisAngle (unit _y) (degToRad yr) * axisAngle (unit _x) (degToRad xr)) zero
animateJoint parentMat rest (BVH.JointEnd (x,y,z)) = (rest, End $ parentMat !*! mkTranslation (V3 x y z))
animateJoint _ _ _ = error "Couldn't convert BVH to animation"
