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
import Vihollinen
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
        (luoKopteri (0, 400))
        [Talo 800 500 700]
        [ Hemmo (700, 800) pituus leveys,
          Hemmo (900, 800) pituus leveys
        ]
        []
        0
        [ Vihollinen (1100, 5) pituus leveys,
          Vihollinen (600, 5) pituus leveys,
          Vihollinen (100, 5) pituus leveys,
          Vihollinen (-600, 5) pituus leveys
        ]
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
    GameWin cl -> GameWin cl

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

poistaViholliset :: [Vihollinen] -> Choplifter -> Choplifter
poistaViholliset [] peli = peli
poistaViholliset (vihollinen : loput) peli = poistaViholliset loput päivitettyPeli
  where
    päivitettyPeli = peli {cl_viholliset = poistaVihollinen vihollinen (cl_viholliset peli)}
    poistaVihollinen ph viholliset = delete ph viholliset

päivitäScore :: Choplifter -> [Vihollinen] -> Choplifter
päivitäScore peli osututViholliset = peli {cl_score = (cl_score peli) + osumat}
  where
    osumat = toInteger (length osututViholliset)

päivitäPelitilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPelitilanne aikaEdellisestä pelitilanne =
  case pelitilanne of
    GameOver cl -> GameOver cl
    GameOn cl -> case (voittikoPelin cl) of
      False -> tarkistaTörmäystilanteet aikaEdellisestä cl
      True -> GameWin cl
    GameWin cl -> GameWin cl

voittikoPelin :: Choplifter -> Bool
voittikoPelin cl = (cl_score cl) == 24

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
  case osuukoAmmus ae (map ammusTörmäysViivat (cl_ammukset cl)) (cl_viholliset cl) of
    Just osuneetVihollisiin ->
      case osuneetVihollisiin of
        [] -> Just cl
        osumiaSaaneetViholliset ->
          let päivitettyPeli = poistaViholliset osumiaSaaneetViholliset cl
           in Just (päivitäScore päivitettyPeli osumiaSaaneetViholliset)
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

montaUuttaHemmoaNouseeKyytiin :: Kopteri -> (Kopteri -> Kopteri) -> Integer
montaUuttaHemmoaNouseeKyytiin kopteri kopterissaUusia =
  let uusi = kopterissaUusia kopteri
      j = compare (kop_hemmojaKyydissä uusi) (kop_hemmojaKyydissä kopteri)
   in case j of
        GT -> toInteger ((kop_hemmojaKyydissä uusi) - (kop_hemmojaKyydissä kopteri))
        otherwise -> 0

päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila =
  case edellinenTila of
    Peli aika kopteri talot hemmot ammukset score viholliset ->
      let nouseekoKyytiin hemmo = magV (hemmo_sijainti hemmo #- kop_paikka kopteri) < 50
          (hemmotKopteriin, hemmotUlkona) = partition nouseekoKyytiin hemmot
          kopterissaHemmoja = noukiHemmot hemmotKopteriin
          montaUuttaHemmoakyydissä = montaUuttaHemmoaNouseeKyytiin kopteri kopterissaHemmoja
          uusiScore = case compare montaUuttaHemmoakyydissä 0 of
            GT -> score + (montaUuttaHemmoakyydissä * 10)
            otherwise -> score
       in Peli
            (aika + aikaEdellisestä)
            (kopterissaHemmoja . päivitäKopteri aikaEdellisestä $ kopteri)
            talot
            ( map
                (päivitäHemmoa (flip korkeusKohdassa edellinenTila) (kop_paikka kopteri))
                hemmotUlkona
            )
            (map (päivitäAmmus aikaEdellisestä) ammukset)
            uusiScore
            ( map
                (päivitäVihollista (kop_paikka kopteri))
                viholliset
            )

data TörmäysKohta = Laskuteline | Roottori
  deriving (Eq, Ord, Show)

osuukoAmmus :: Float -> [((Point, Point), (Point, Point))] -> [Vihollinen] -> Maybe [Vihollinen]
osuukoAmmus _ae viivat viholliset =
  let yhteen törPist vihollinen =
        case (osuuko (nurkkaPisteetVihollinen vihollinen) törPist) of
          False -> Nothing
          _ -> Just vihollinen
      osuukoViivaan viiva = mapMaybe (yhteen viiva) viholliset
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
    GameWin cl -> piirräPeli cl <> translate (-300) 0 (color yellow (text "YOU WIN!"))

piirräPeli :: Choplifter -> Picture
piirräPeli peli =
  let talot = cl_talot peli

      kopterikuva = piirräKopteri (cl_aika peli) (cl_kopteri peli)

      hemmoKuvat = map (piirräHemmo (cl_aika peli)) (cl_hemmot peli)
      taloKuvat = map piirräTalo talot

      viholliset = map (piirräVihollinen (cl_aika peli)) (cl_viholliset peli)

      peliKuva = case length (cl_ammukset peli) > 0 of
        True ->
          maa
            <> translate (-250) (ruudunY * 2) (text ("Score: " ++ (show (cl_score peli))))
            <> pictures taloKuvat
            <> pictures hemmoKuvat
            <> pictures viholliset
            <> kopterikuva
            <> pictures (map piirräAmmus (cl_ammukset peli))
        False ->
          maa
            <> translate (-250) (ruudunY * 2) (text ("Score: " ++ (show (cl_score peli))))
            <> pictures taloKuvat
            <> pictures hemmoKuvat
            <> pictures viholliset
            <> kopterikuva
   in scale 0.25 0.25 (translate 0 (-180) peliKuva)

data PeliTilanne = GameOver Choplifter | GameOn Choplifter | GameWin Choplifter

data Choplifter = Peli
  { -- | Aika pelin alusta
    cl_aika :: Float,
    cl_kopteri :: Kopteri, -- kopterin tiedot
    cl_talot :: [Talo], -- Esteet pelissä
    cl_hemmot :: [Hemmo], -- Pelihahmot
    cl_ammukset :: [Ammus], -- Kopterin ampumat pommit
    cl_score :: Integer, -- Score
    cl_viholliset :: [Vihollinen] -- Viholliset
  }
  deriving (Show)

korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli =
  maybe 0 maximum1 . nonEmpty . map (osuukoTaloon kohta) . cl_talot $ peli

maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))

testiPeli :: Choplifter
testiPeli =
  ( Peli
      0
      (luoKopteri (0, 0))
      [Talo 800 500 700]
      [ Hemmo (700, 800) pituus leveys,
        Hemmo (900, 800) pituus leveys
      ]
      yksiAmmusOsuuTestiVihollisia2
      0
      testiVihollisia2
  )

osuuYhteenViholliseenTesti :: Maybe [Vihollinen]
osuuYhteenViholliseenTesti =
  let as = yksiAmmusOsuuTestiVihollisia2
      v = testiVihollisia2
      p = osuukoAmmus 0 (map ammusTörmäysViivat as) v
   in p

poistoTesti :: Maybe Choplifter
poistoTesti =
  let osunutVihollinen = osuuYhteenViholliseenTesti
      peli = case osunutVihollinen of
        Just x -> case x of
          [] -> Nothing
          y -> Just (poistaViholliset y testiPeli)
        Nothing -> Nothing
   in peli

tulos :: Maybe Choplifter
tulos =
  Just
    ( Peli
        { cl_aika = 0.0,
          cl_kopteri = Kopteri {kop_paikka = (0.0, 0.0), kop_nopeus = (0.0, 0.0), kop_teho = 0.0, kop_kulma = 0.0, kop_hemmojaKyydissä = 0},
          cl_talot = [Talo {talo_korkeus = 800.0, talo_leveys = 500.0, talo_sijainti = 700.0}],
          cl_hemmot =
            [ Hemmo {hemmo_sijainti = (700.0, 800.0), hemmo_pituus = 120.0, hemmo_leveys = 50.0},
              Hemmo {hemmo_sijainti = (900.0, 800.0), hemmo_pituus = 120.0, hemmo_leveys = 50.0}
            ],
          cl_ammukset =
            [ Ammus {ammus_paikka = (100.0, 0.0), ammus_nopeus = (1.0, 1.0)},
              Ammus {ammus_paikka = (-1.0e7, -1.0e11), ammus_nopeus = (-100.0, -100.0)},
              Ammus {ammus_paikka = (-1.0e8, -1.0e9), ammus_nopeus = (-999.0, -999.0)}
            ],
          cl_score = 0,
          cl_viholliset = []
        }
    )