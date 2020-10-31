module ApuViivoja where

import Ampuminen
import Graphics.Gloss
import Hemmot

hemmoNurkat :: [Hemmo] -> [Picture]
hemmoNurkat hemmot =
  map
    ( \d ->
        let (((x, y), (_x1, _y1)), ((_x2, _y2), (_x3, _y3))) = nurkkaPisteetHemmo d
            t1 = translate x y (color black (circleSolid 10))
         in t1
    )
    hemmot

hemmoNurkat1 :: [Hemmo] -> [Picture]
hemmoNurkat1 hemmot =
  map
    ( \d ->
        let (((_x, _y), (_x1, _y1)), ((_x2, _y2), (_x3, _y3))) = nurkkaPisteetHemmo d
            t1 = translate _x1 _y1 (color black (circleSolid 10))
         in t1
    )
    hemmot

hemmoNurkat2 :: [Hemmo] -> [Picture]
hemmoNurkat2 hemmot =
  map
    ( \d ->
        let (((_x, _y), (_x1, _y1)), ((_x2, _y2), (_x3, _y3))) = nurkkaPisteetHemmo d
            t1 = translate _x2 _y2 (color black (circleSolid 10))
         in t1
    )
    hemmot

hemmoNurkat3 :: [Hemmo] -> [Picture]
hemmoNurkat3 hemmot =
  map
    ( \d ->
        let (((_x, _y), (_x1, _y1)), ((_x2, _y2), (_x3, _y3))) = nurkkaPisteetHemmo d
            t1 = translate _x3 _y3 (color black (circleSolid 10))
         in t1
    )
    hemmot

v1 :: [Hemmo] -> [Picture]
v1 hemmot =
  map
    ( \d ->
        let ((p1, p2), (p3, p4)) = nurkkaPisteetHemmo d
            t1 = color yellow (line [p1, p4])
         in t1
    )
    hemmot

v2 :: [Ammus] -> [Picture]
v2 ammukset =
  map
    ( \d ->
        let ((p1, p2), (p3, p4)) = ammusTörmäysViivat d
            t1 = color red (line [p1, p4])
         in t1
    )
    ammukset