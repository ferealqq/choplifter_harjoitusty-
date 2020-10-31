module Ampuminen where

import Aritmetiikka
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Kopteri

data Ammus = Ammus
  { ammus_paikka :: Point, -- missä ammus

    -- | Kuinka nopeasti menee?
    ammus_nopeus :: Vector
  }
  deriving (Show)

luoAmmukset :: Float -> Kopteri -> [Ammus]
luoAmmukset monta kopteri = map (luoAmmus kopteri) [1 .. monta]

luoAmmus :: Kopteri -> Float -> Ammus
luoAmmus kopteri@(Kopteri {kop_paikka = (x, y)}) mones = Ammus (x - mones * 15, y - mones * 35) (2, 2)

piirräAmmus :: Ammus -> Picture
piirräAmmus ammus = translate x y (scale 0.4 0.4 ammusKuva)
  where
    (x, y) = ammus_paikka ammus
    ammusKuva = translate 0 (150) (color black pallo)
    pallo = circleSolid 50

-- TODO

päivitäAmmus :: Float -> Ammus -> Ammus
päivitäAmmus aikaEdellisestä ammus =
  ammus
    { ammus_paikka =
        ( ammusX,
          ammusY - vY
        ),
      ammus_nopeus = ((vX + 2) * 0.97, (vY + 2) * 0.97)
    }
  where
    (ammusX, ammusY) = ammus_paikka ammus
    (vX, vY) = ammus_nopeus ammus

ammusTörmäysViivat :: Ammus -> ((Point, Point), (Point, Point))
ammusTörmäysViivat ammus =
  let säde = 80
      y1 = (säde, säde) #+ (ammus_paikka ammus)
      y2 = (- säde, säde) #+ (ammus_paikka ammus)
      y3 = (säde, - säde) #+ (ammus_paikka ammus)
      y4 = (- säde, - säde) #+ (ammus_paikka ammus)
   in ((y1, y2), (y3, y4))

ammusTesti :: Ammus
ammusTesti = Ammus (0, 0) (2, 2)

montaAmmusta :: [Ammus]
montaAmmusta = [ammusTesti, Ammus (-100, -100) (2, 2), Ammus (-100, -100) (2, 2)]

montaAmmusta2 :: [Ammus]
montaAmmusta2 = [ammusTesti, Ammus (800, 800) (2, 2), Ammus (700, 700) (2, 2)]

viivatAmmus :: [((Point, Point), (Point, Point))]
viivatAmmus = map ammusTörmäysViivat montaAmmusta

viivatAmmus2 :: [((Point, Point), (Point, Point))]
viivatAmmus2 = map ammusTörmäysViivat montaAmmusta2

hävinneetAmmukset :: [Ammus]
hävinneetAmmukset = [Ammus (-10000000, -100000000000) (-100, -100), Ammus (-99999999, -999999999) (-999, -999)]

kadonneetViivat :: [((Point, Point), (Point, Point))]
kadonneetViivat = map ammusTörmäysViivat hävinneetAmmukset