module Aritmetiikka where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line

(#+) :: Point -> Vector -> Point
(a, b) #+ (x, y) = (a + x, b + y)

(#-) :: Point -> Point -> Vector
(a, b) #- (x, y) = (a - x, b - y)

type PointPari = ((Point, Point), (Point, Point))

osuuko_ :: PointPari -> PointPari -> Bool
osuuko_ ((p1, p2), (p3, p4)) ((pl1, pl2), (pl3, pl4)) =
  case intersectSegSeg p1 p4 pl1 pl4 of
    Nothing -> False
    Just _a -> True

osuuko :: PointPari -> PointPari -> Bool
osuuko (pointPari1, pointPari2) (pointPari3, pointPari4) = (fst (foldr ok (False, []) [1 .. 4]))
  where
    ok n (b, _loput) =
      case b of
        False -> case n of
          1 -> (osuukoApu (fst pointPari1) (snd pointPari2) (fst pointPari3) (snd pointPari4), _loput)
          2 -> (osuukoApu (fst pointPari1) (snd pointPari2) (snd pointPari3) (fst pointPari4), _loput)
          3 -> (osuukoApu (snd pointPari2) (fst pointPari2) (fst pointPari3) (snd pointPari4), _loput)
          4 -> (osuukoApu (snd pointPari1) (fst pointPari2) (snd pointPari3) (fst pointPari4), _loput)
          otherwise -> (b, _loput)
        True -> (True, [])

kpe :: PointPari -> Point
kpe pointPari = fst (fst pointPari)

kpt :: PointPari -> Point
kpt pointPari = snd (snd pointPari)

kpet :: PointPari -> Point
kpet pointPari = fst (snd pointPari)

kpte :: PointPari -> Point
kpte pointPari = snd (fst pointPari)

osuukoApu :: Point -> Point -> Point -> Point -> Bool
osuukoApu p1 p2 p3 p4 = case intersectSegSeg p1 p2 p3 p4 of
  Nothing -> False
  Just _a -> True