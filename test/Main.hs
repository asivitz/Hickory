{-# LANGUAGE ImplicitParams #-}
import Test.HUnit
import Test.HUnit.Approx
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Hickory.Math.Vector

testv2angle = TestCase $ do
    let ?epsilon = 0.00001

    v2angle (v2 1 0) (v2 1 1) @?~ pi / 4
    v2angle (v2 1 1) (v2 1 0) @?~ pi / (-4)
    v2angle (v2 1 0) (v2 (-1) 1) @?~ pi * (3 / 4)
    v2angle (v2 1 0) (v2 (-1) (-1)) @?~ pi * ((-3) / 4)

    v2angle (v2 (-1) 0) (v2 0 1) @?~ pi / (-2)
    v2angle (v2 (-1) (-1)) (v2 0 (-1)) @?~ pi / 4
    v2angle (v2 (-1) (-1)) (v2 1 1) @?~ -pi
    v2angle (v2 (-1) (-1)) (v2 0 1) @?~ pi * ((-3)/4)

testv2clockwise = TestCase $ do
    v2clockwise (v2 1 0) (v2 1 1) @?= False
    v2clockwise (v2 1 1) (v2 1 0) @?= True
    v2clockwise (v2 (-1) 0) (v2 0 1) @?= True

testv2SegmentsIntersect = TestCase $ do
    v2SegmentsIntersect (v2 0 0, v2 5 5) (v2 0 5, v2 5 0) @?= True
    v2SegmentsIntersect (v2 0 0, v2 5 5) (v2 5 0, v2 10 5) @?= False
    v2SegmentsIntersect (v2 2 0, v2 2 5) (v2 0 2, v2 5 2) @?= True
    v2SegmentsIntersect (v2 0 0, v2 0 5) (v2 1 0, v2 1 5) @?= False
    v2SegmentsIntersect (v2 0 0, v2 5 0) (v2 0 2, v2 5 2) @?= False

tests = test ["v2angle" ~: testv2angle,
              "v2clockwise" ~: testv2clockwise,
              "v2SegmentsIntersect" ~: testv2SegmentsIntersect]

main :: IO ()
main = do
        runTestTT tests
        return ()
