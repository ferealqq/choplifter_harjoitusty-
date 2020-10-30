module Kopteri where

import Aritmetiikka
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Hemmot (Hemmo)

luoKopteri :: Point -> Kopteri
luoKopteri paikka =
  Kopteri
    paikka
    (0, 0)
    0
    0
    0

data Kopteri = Kopteri
  { -- | Missä kopteri?
    kop_paikka :: Point,
    -- | Kuinka nopeasti menee?
    kop_nopeus :: Vector,
    -- | Teho
    kop_teho :: Float,
    -- | Kuinka vinossa
    kop_kulma :: Float,
    kop_hemmojaKyydissä :: Natural -- Kuinka monta hemmoa kerätty
  }
  deriving (Show)

päivitäKopteri :: Float -> Kopteri -> Kopteri
päivitäKopteri aikaEdellisestä kopteri =
  kopteri
    { kop_paikka =
        ( kopteriX + aikaEdellisestä * vX,
          max 0 (kopteriY + aikaEdellisestä * vY)
        ),
      kop_nopeus = ((vX + dX) * 0.97, (vY + dY - 5) * 0.97)
    }
  where
    (dX, dY) = kulmaJaTehoKiihtyvyydeksi (kop_teho kopteri) (kop_kulma kopteri)
    (kopteriX, kopteriY) = kop_paikka kopteri
    (vX, vY) = kop_nopeus kopteri

noukiHemmot :: [Hemmo] -> Kopteri -> Kopteri
noukiHemmot hemmot kopteri =
  kopteri
    { kop_hemmojaKyydissä = (kop_hemmojaKyydissä kopteri + genericLength hemmot)
    }

kulmaJaTehoKiihtyvyydeksi :: Float -> Float -> (Float, Float)
kulmaJaTehoKiihtyvyydeksi teho kulma =
  rotateV (- degToRad kulma) (0, teho)

piirräKopteri :: Float -> Kopteri -> Picture
piirräKopteri aika kopteri = translate x y (rotate (kop_kulma kopteri) (scale 0.4 0.4 kopterinKuva))
  where
    (x, y) = kop_paikka kopteri
    teho = kop_teho kopteri
    kopterinKuva = translate 0 (150) (color white runko)
    runko =
      circleSolid 100
        <> translate (-200) 0 (rectangleSolid 300 30)
        <> translate (-350) 0 (rectangleSolid 50 100)
        <> lapa
        <> translate 0 90 (rectangleSolid 10 120)
        <> translate (-50) (-90) (rectangleSolid 10 120)
        <> translate (50) (-90) (rectangleSolid 10 120)
        <> translate 0 (-150) (rectangleSolid 200 15)

    lapa = translate 0 150 (rectangleSolid (350 * sin (aika * teho)) 10)

kopteriTörmäysviivat :: Kopteri -> ((Point, Point), (Point, Point))
kopteriTörmäysviivat kopteri =
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

laskeudu :: Kopteri -> Kopteri
laskeudu kopteri@(Kopteri {kop_nopeus = (_vX, vY)}) =
  kopteri {kop_kulma = 0, kop_nopeus = (0, max 0 vY)}

onkoHyväLaskeutuminen :: Kopteri -> Bool
onkoHyväLaskeutuminen kopteri =
  magV (kop_nopeus kopteri) < 80 && abs (kop_kulma kopteri) <= 10

kallista :: Float -> Kopteri -> Kopteri
muutaTehoa :: Float -> Kopteri -> Kopteri
kallista muutos kopteri = kopteri {kop_kulma = muutos + kop_kulma kopteri}

muutaTehoa muutos kopteri = kopteri {kop_teho = muutos + kop_teho kopteri}