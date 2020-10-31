module Main where

import Ampuminen
import Aritmetiikka
import Data.List (delete, partition)
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Hemmot
import Kopteri
import Talot
import Prelude hiding (Down)

pituus :: Num a => a
pituus = (100 + 15 + 5)

leveys :: Num a => a
leveys = (20 + 15 + 10 + 5)

alkutilanne :: PeliTilanne
alkutilanne =
  GameOn
    ( Peli
        0
        (luoKopteri (0, 0))
        [Talo 800 500 700]
        [ Hemmo (700, 800) pituus leveys,
          Hemmo (900, 800) pituus leveys,
          Hemmo (-200, 0) pituus leveys
        ]
        []
        0
    )

ruudunX :: Num p => p
ruudunX = 1000

ruudunY :: Num p => p
ruudunY = 800

main :: IO ()
main =
  play
    (InWindow "Choplifter" (ruudunX, ruudunY) (10, 10))
    (light blue)
    24
    alkutilanne
    piirräPeliTilanne
    reagoiPeliTilanne
    päivitäPelitilanne

reagoiPeliTilanne :: Event -> PeliTilanne -> PeliTilanne
reagoiPeliTilanne tapahtuma pelitilanne =
  case pelitilanne of
    GameOver cl -> GameOver cl
    GameOn cl -> GameOn (reagoi tapahtuma cl)

reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli =
  case tapahtuma of
    EventKey (Char 'w') Down _ _ -> kopterille (muutaTehoa 2.5) peli
    EventKey (Char 's') Down _ _ -> kopterille (muutaTehoa (-2.5)) peli
    EventKey (Char 'a') Down _ _ -> kopterille (kallista (-8)) peli
    EventKey (Char 'd') Down _ _ -> kopterille (kallista (8)) peli
    EventKey (Char 'f') Down _ _ -> ammu 1 peli
    _ -> peli

kopterille :: (Kopteri -> Kopteri) -> Choplifter -> Choplifter
kopterille f peli = peli {cl_kopteri = f (cl_kopteri peli)}

ammu :: Float -> Choplifter -> Choplifter
ammu monta peli = peli {cl_ammukset = (luoAmmukset monta (cl_kopteri peli)) ++ (cl_ammukset peli)}

poistaHemmoja :: [Hemmo] -> Choplifter -> Choplifter
poistaHemmoja hemmot peli = peli {cl_hemmot = (foldr poistaHemmo [] hemmot)}
  where
    poistaHemmo ph poistetut = delete ph poistetut

päivitäScore :: Choplifter -> [Hemmo] -> Choplifter
päivitäScore peli osuneetHemmot = peli {cl_score = (cl_score peli) + osumat}
  where
    osumat = length osuneetHemmot

päivitäPelitilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPelitilanne aikaEdellisestä pelitilanne =
  case pelitilanne of
    GameOver cl -> GameOver cl
    GameOn cl -> tarkistaTörmäystilanteet aikaEdellisestä cl

tarkistaTörmäystilanteet :: Float -> Choplifter -> PeliTilanne
tarkistaTörmäystilanteet ae cl =
  let ehkäPäivitettyCl = case length (cl_ammukset cl) > 0 of
        True -> case törmääköTaloonPäivitys ae cl of
          Just cl2 -> osuukoAmmusPäivitys ae cl2
          Nothing -> Nothing
        False -> törmääköTaloonPäivitys ae cl
   in case ehkäPäivitettyCl of
        Just cl3 ->
          GameOn
            ( päivitäPeliä
                ae
                cl3
            )
        Nothing -> GameOver cl

osuukoAmmusPäivitys :: Float -> Choplifter -> Maybe Choplifter
osuukoAmmusPäivitys ae cl =
  case osuukoAmmus ae (map ammusTörmäysViivat (cl_ammukset cl)) (cl_hemmot cl) of
    Just osunutHemmoihin ->
      case osunutHemmoihin of
        [] -> Just cl
        osumiaSaaneetHemmot ->
          let päivitettyPeli = poistaHemmoja osumiaSaaneetHemmot cl
           in Just (päivitäScore päivitettyPeli osumiaSaaneetHemmot)
    Nothing -> Just cl

törmääköTaloonPäivitys :: Float -> Choplifter -> Maybe Choplifter
törmääköTaloonPäivitys aikaEdellisestä cl =
  case törmääköTaloon (kopteriTörmäysviivat (cl_kopteri cl)) (cl_talot cl) of
    Nothing -> Just (päivitäPeliä aikaEdellisestä cl)
    Just Roottori -> Nothing
    Just Laskuteline
      | onkoHyväLaskeutuminen (cl_kopteri cl) ->
        Just
          ( (kopterille laskeudu cl)
          )
      | otherwise -> Nothing

päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila =
  case edellinenTila of
    Peli aika kopteri talot hemmot ammukset kc ->
      let nouseekoKyytiin hemmo = magV (hemmo_sijainti hemmo #- kop_paikka kopteri) < 50
          (hemmotKopteriin, hemmotUlkona) = partition nouseekoKyytiin hemmot
       in Peli
            (aika + aikaEdellisestä)
            (noukiHemmot hemmotKopteriin . päivitäKopteri aikaEdellisestä $ kopteri)
            talot
            ( map
                (päivitäHemmoa (flip korkeusKohdassa edellinenTila) (kop_paikka kopteri))
                hemmotUlkona
            )
            (map (päivitäAmmus aikaEdellisestä) ammukset)
            kc

data TörmäysKohta = Laskuteline | Roottori
  deriving (Eq, Ord, Show)

osuukoAmmus :: Float -> [((Point, Point), (Point, Point))] -> [Hemmo] -> Maybe [Hemmo]
osuukoAmmus _ae viivat hemmot =
  let yhteen törPist hemmo1 =
        case (osuuko (nurkkaPisteetHemmo hemmo1) törPist) of
          False -> Nothing
          _ -> Just hemmo1
      osuukoViivaan viiva = mapMaybe (yhteen viiva) hemmot
      osuukoHemmot = fmap head (nonEmpty (map osuukoViivaan viivat))
   in osuukoHemmot

törmääköTaloon :: ((Point, Point), (Point, Point)) -> [Talo] -> Maybe TörmäysKohta
törmääköTaloon törmäysviivat talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
  where
    -- case (nonEmpty (mapMaybe törmääköYhteen talot)) of
    --  Nothing -> Nothing
    --  Just kohdat -> Just (maximum1 kohdat)

    törmääköYhteen talo =
      let ((ala1, ala2), (ylä1, ylä2)) = törmäysviivat
          (va, oy) = nurkkaPisteet talo
       in case (not (segClearsBox ala1 ala2 va oy), not (segClearsBox ylä1 ylä2 va oy)) of
            (True, False) -> Just Laskuteline
            (False, False) -> Nothing
            _ -> Just Roottori

piirräPeliTilanne :: PeliTilanne -> Picture
piirräPeliTilanne pelitilanne =
  case pelitilanne of
    GameOver cl -> piirräPeli cl <> translate (-300) 0 (color yellow (text "GAME OVER"))
    GameOn cl -> piirräPeli cl

piirräPeli :: Choplifter -> Picture
piirräPeli peli =
  let talot = cl_talot peli

      kopterikuva = piirräKopteri (cl_aika peli) (cl_kopteri peli)

      hemmoKuvat = map (piirräHemmo (cl_aika peli)) (cl_hemmot peli)
      taloKuvat = map piirräTalo talot

      hemmoNurkat =
        map
          ( \d ->
              let (((x, y), (_x1, _y1)), ((_x2, _y2), (_x3, _y3))) = nurkkaPisteetHemmo d
                  t1 = translate x y (color black (circleSolid 10))
               in t1
          )
          (cl_hemmot peli)

      hemmoNurkat1 =
        map
          ( \d ->
              let (((_x, _y), (_x1, _y1)), ((_x2, _y2), (_x3, _y3))) = nurkkaPisteetHemmo d
                  t1 = translate _x1 _y1 (color black (circleSolid 10))
               in t1
          )
          (cl_hemmot peli)

      hemmoNurkat2 =
        map
          ( \d ->
              let (((_x, _y), (_x1, _y1)), ((_x2, _y2), (_x3, _y3))) = nurkkaPisteetHemmo d
                  t1 = translate _x2 _y2 (color black (circleSolid 10))
               in t1
          )
          (cl_hemmot peli)

      hemmoNurkat3 =
        map
          ( \d ->
              let (((_x, _y), (_x1, _y1)), ((_x2, _y2), (_x3, _y3))) = nurkkaPisteetHemmo d
                  t1 = translate _x3 _y3 (color black (circleSolid 10))
               in t1
          )
          (cl_hemmot peli)

      v1 =
        map
          ( \d ->
              let ((p1, p2), (p3, p4)) = nurkkaPisteetHemmo d
                  t1 = color yellow (line [p1, p4])
               in t1
          )
          (cl_hemmot peli)

      v2 =
        map
          ( \d ->
              let ((p1, p2), (p3, p4)) = ammusTörmäysViivat d
                  t1 = color red (line [p1, p4])
               in t1
          )
          (cl_ammukset peli)

      peliKuva = case length (cl_ammukset peli) > 0 of
        True ->
          maa
            <> translate (-250) (ruudunY * 2) (text ("Score: " ++ (show (cl_score peli))))
            <> pictures taloKuvat
            <> pictures hemmoKuvat
            <> kopterikuva
            <> pictures (map piirräAmmus (cl_ammukset peli))
            <> pictures hemmoNurkat
            <> pictures hemmoNurkat1
            <> pictures hemmoNurkat2
            <> pictures hemmoNurkat3
            <> pictures v2
            <> pictures v1
        False ->
          maa
            <> translate (-250) (ruudunY * 2) (text ("Score: " ++ (show (cl_score peli))))
            <> pictures taloKuvat
            <> pictures hemmoKuvat
            <> kopterikuva
            <> pictures hemmoNurkat
            <> pictures hemmoNurkat1
            <> pictures hemmoNurkat2
            <> pictures hemmoNurkat3
            <> pictures v1
            <> pictures v2
   in scale 0.25 0.25 (translate 0 (-180) peliKuva)

data PeliTilanne = GameOver Choplifter | GameOn Choplifter

data Choplifter = Peli
  { -- | Aika pelin alusta
    cl_aika :: Float,
    cl_kopteri :: Kopteri, -- kopterin tiedot
    cl_talot :: [Talo], -- Esteet pelissä
    cl_hemmot :: [Hemmo], -- Pelihahmot
    cl_ammukset :: [Ammus],
    cl_score :: Int
  }
  deriving (Show)

korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli =
  maybe 0 maximum1 . nonEmpty . map (osuukoTaloon kohta) . cl_talot $ peli

maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))

testiPeli =
  ( Peli
      0
      (luoKopteri (0, 0))
      [Talo 800 500 700]
      [ Hemmo (700, 800) pituus leveys,
        Hemmo (900, 800) pituus leveys
      ]
      montaAmmusta
      0
  )