module Aritmetiikka where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line

(#+) :: Point -> Vector -> Point
(a, b) #+ (x, y) = (a + x, b + y)

(#-) :: Point -> Point -> Vector
(a, b) #- (x, y) = (a - x, b - y)

osuuko :: Point -> Point -> Point -> Point -> Bool
osuuko p1 p2 p3 p4 =
  case intersectLineLine p1 p2 p3 p4 of
    Nothing -> False
    Just _a -> True