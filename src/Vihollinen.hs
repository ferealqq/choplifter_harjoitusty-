module Vihollinen where

import Aritmetiikka
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

data Vihollinen = Vihollinen
  { vihollinen_sijainti :: Point,
    vihollinen_pituus :: Float,
    vihollinen_leveys :: Float
  }
  deriving (Show, Eq)

haluaakoLiikkua :: Point -> Vihollinen -> Bool
haluaakoLiikkua kopterinPaikka vihollinen = haluaaLiikkua
  where
    haluaaLiikkua = magV (kopterinPaikka #- vihollinen_sijainti vihollinen) < 600

minneVihollinenMenisi :: Point -> Vihollinen -> Float
minneVihollinenMenisi kopterinPaikka vihollinen
  | fst kopterinPaikka < fst (vihollinen_sijainti vihollinen) =
    -2
  | otherwise =
    2

päivitäVihollista :: Point -> Vihollinen -> Vihollinen
päivitäVihollista kopterinSijainti vihollinen
  | haluaakoLiikkua kopterinSijainti vihollinen =
    vihollinen {vihollinen_sijainti = vihollinen_sijainti vihollinen #+ (suunta, 0)}
  | otherwise =
    vihollinen
  where
    suunta = minneVihollinenMenisi kopterinSijainti vihollinen

piirräVihollinen :: Float -> Vihollinen -> Picture
piirräVihollinen aika vihollinen =
  let (x, y) = vihollinen_sijainti vihollinen
      lantio = (15, 40)
      vasenJalka = 15 + sin (12 * aika) * 7
      oikeaJalka = 15 + cos (12 * aika) * 7
      vihollinenKuva =
        color
          red
          ( translate 0 110 (circleSolid 20)
              <> line [(0, 100), lantio] -- selkä
              <> line
                [ (-40, 90 + cos (8 * aika + 0.3) * 40),
                  (-30, 90),
                  (30, 90),
                  (40, 90 + cos (8 * aika) * 40) -- kädet
                ]
              <> line
                [ (-25, vasenJalka),
                  (-20, vasenJalka),
                  lantio,
                  (30, oikeaJalka),
                  (35, oikeaJalka) --jalat
                ]
          )
   in translate x y vihollinenKuva

nurkkaPisteetVihollinen :: Vihollinen -> ((Point, Point), (Point, Point))
nurkkaPisteetVihollinen _vihu@(Vihollinen {vihollinen_sijainti = (hx, hy), vihollinen_pituus = pituus, vihollinen_leveys = leveys}) =
  let vihollinenMinX = hx - (leveys / 2)
      vihollinenMaxX = hx + (leveys / 2)
      vihollinenMaxY = hy + pituus
      vasenYlä = (vihollinenMinX, vihollinenMaxY)
      vasenAla = (vihollinenMinX, hy)
      oikeaYlä = (vihollinenMaxX, vihollinenMaxY)
      oikeaAla = (vihollinenMaxX, hy)
   in ((vasenYlä, vasenAla), (oikeaYlä, oikeaAla))

testiVihollinen :: Vihollinen
testiVihollinen = Vihollinen (1000, 1000) 120 65

testiVihollisia :: [Vihollinen]
testiVihollisia = [testiVihollinen, Vihollinen (800, 900) 120 65, Vihollinen (700, 500) 120 65]

mahdotonOsuaVihollinen :: Vihollinen
mahdotonOsuaVihollinen = Vihollinen (1000, 1000) 1 1

mahdotonOsuaVihollisia :: [Vihollinen]
mahdotonOsuaVihollisia = [mahdotonOsuaVihollinen, Vihollinen (2, 900) 1 1, Vihollinen (700, 500) 1 1]

testiVihollisia2 :: [Vihollinen]
testiVihollisia2 = [Vihollinen (100, 0) 120 65, Vihollinen (500, 0) 120 65, Vihollinen (-400, 0) 120 65]

{-
testaaOsuuko :: [((Point, b), (a, Point))] -> [Vihollinen] -> Maybe [Point]
testaaOsuuko viivat Vihollinent =
  let yhteen ((vy, va), (oy, oa)) Vihollinen1 =
        let ((h1, h2), (h3, h4)) = nurkkaPisteetVihollinen Vihollinen1
         in case (osuuko2 h1 vy h4 oa) of
              Nothing -> Nothing
              Just a -> Just a
      osuukoViivaan viiva = mapMaybe (yhteen viiva) hemmot
      osuukoHemmot = fmap head (nonEmpty (map osuukoViivaan viivat))
   in osuukoHemmot

-}