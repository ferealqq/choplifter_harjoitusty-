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
  deriving (Show)

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
   in translate x y hemmonKuva

{-
hemmoTörmäysViivat :: Hemmo -> ((Point, Point), (Point, Point))
hemmoTörmäysViivat kopteri =
  let paikka = kop_paikka kopteri
      kulma = kop_kulma kopteri
      vasen = -170
      oikea = 100
      kääntö = rotateV (- degToRad kulma)
   in ( ( kääntö (vasen, 0) #+ paikka,
          kääntö (oikea, 0) #+ paikka
        ),
        ( kääntö (vasen, 120) #+ paikka,
          kääntö (oikea, 120) #+ paikka
        )
      )
-}

osuukoHemmoon :: Float -> Hemmo -> Float
osuukoHemmoon kohta hemmo
  | abs (fst (hemmo_sijainti hemmo) - kohta) < (hemmo_leveys hemmo / 2) =
    hemmo_pituus
      hemmo
  | otherwise = 0

nurkkaPisteetHemmo :: Hemmo -> (Point, Point)
nurkkaPisteetHemmo hemmo =
  let hemmoMinX = fst (hemmo_sijainti hemmo) - ((hemmo_leveys hemmo) / 2)
      hemmoMaxX = fst (hemmo_sijainti hemmo) + ((hemmo_leveys hemmo) / 2)
      hemmoMaxY = snd (hemmo_sijainti hemmo) + ((hemmo_pituus hemmo))
      hemmoMinY = snd (hemmo_sijainti hemmo)
      vasenAla = (hemmoMinX, hemmoMinY)
      oikeaYlä = (hemmoMaxX, hemmoMaxY)
   in (vasenAla, oikeaYlä)

testiHemmo = Hemmo (1000, 1000) 120 65

testiHemmoja = [testiHemmo, Hemmo (800, 900) 120 65, Hemmo (700, 500) 120 65]
