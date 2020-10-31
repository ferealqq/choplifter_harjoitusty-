module Hemmot where

import Aritmetiikka
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

data Hemmo = Hemmo
  { hemmo_sijainti :: Point,
    hemmo_pituus :: Float,
    hemmo_leveys :: Float
  }
  deriving (Show, Eq)

haluaakoLiikkua :: (Float -> Float) -> Point -> Hemmo -> Bool
haluaakoLiikkua korkeusKohdassa kopterinPaikka hemmo = haluaaLiikkua && not putoaako
  where
    putoaako = abs (korkeusEdessä - snd (hemmo_sijainti hemmo)) > 50
    korkeusEdessä = korkeusKohdassa (fst (hemmo_sijainti hemmo) + suunta * 2)

    haluaaLiikkua = magV (kopterinPaikka #- hemmo_sijainti hemmo) < 600
    suunta = minneHemmoMenisi kopterinPaikka hemmo

minneHemmoMenisi :: Point -> Hemmo -> Float
minneHemmoMenisi kopterinPaikka hemmo
  | fst kopterinPaikka < fst (hemmo_sijainti hemmo) =
    -15
  | otherwise =
    15

päivitäHemmoa :: (Float -> Float) -> Point -> Hemmo -> Hemmo
päivitäHemmoa korkeusKohdassa kopterinSijainti hemmo
  | haluaakoLiikkua korkeusKohdassa kopterinSijainti hemmo =
    hemmo {hemmo_sijainti = hemmo_sijainti hemmo #+ (suunta, 0)}
  | otherwise =
    hemmo
  where
    suunta = minneHemmoMenisi kopterinSijainti hemmo

piirräHemmo :: Float -> Hemmo -> Picture
piirräHemmo aika hemmo =
  let (x, y) = hemmo_sijainti hemmo
      lantio = (15, 40)
      vasenJalka = 15 + sin (12 * aika) * 7
      oikeaJalka = 15 + cos (12 * aika) * 7
      hemmonKuva =
        color
          (dark blue)
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
   in translate x y hemmonKuva

osuukoHemmoon :: Float -> Hemmo -> Float
osuukoHemmoon kohta hemmo
  | abs (fst (hemmo_sijainti hemmo) - kohta) < (hemmo_leveys hemmo / 2) =
    hemmo_pituus
      hemmo
  | otherwise = 0

nurkkaPisteetHemmo :: Hemmo -> ((Point, Point), (Point, Point))
nurkkaPisteetHemmo _hemmo@(Hemmo {hemmo_sijainti = (hx, hy), hemmo_pituus = pituus, hemmo_leveys = leveys}) =
  let hemmoMinX = hx - (leveys / 2)
      hemmoMaxX = hx + (leveys / 2)
      hemmoMaxY = hy + pituus
      vasenYlä = (hemmoMinX, hemmoMaxY)
      vasenAla = (hemmoMinX, hy)
      oikeaYlä = (hemmoMaxX, hemmoMaxY)
      oikeaAla = (hemmoMaxX, hy)
   in ((vasenYlä, vasenAla), (oikeaYlä, oikeaAla))

testiHemmo :: Hemmo
testiHemmo = Hemmo (1000, 1000) 120 65

testiHemmoja :: [Hemmo]
testiHemmoja = [testiHemmo, Hemmo (800, 900) 120 65, Hemmo (700, 500) 120 65]

mahdotonOsuaHemmo :: Hemmo
mahdotonOsuaHemmo = Hemmo (1000, 1000) 1 1

mahdotonOsuaHemmoja :: [Hemmo]
mahdotonOsuaHemmoja = [mahdotonOsuaHemmo, Hemmo (2, 900) 1 1, Hemmo (700, 500) 1 1]

{-
testaaOsuuko :: [((Point, b), (a, Point))] -> [Hemmo] -> Maybe [Point]
testaaOsuuko viivat hemmot =
  let yhteen ((vy, va), (oy, oa)) hemmo1 =
        let ((h1, h2), (h3, h4)) = nurkkaPisteetHemmo hemmo1
         in case (osuuko2 h1 vy h4 oa) of
              Nothing -> Nothing
              Just a -> Just a
      osuukoViivaan viiva = mapMaybe (yhteen viiva) hemmot
      osuukoHemmot = fmap head (nonEmpty (map osuukoViivaan viivat))
   in osuukoHemmot

-}