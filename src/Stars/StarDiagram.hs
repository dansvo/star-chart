module Stars.StarDiagram where

import Stars.Types
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text
import Data.Maybe (catMaybes)
import Data.List (sortOn, find)
import ApparentStar
import Projections
import Luminous
import Location
import Data.Astro.Time
import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types (GeographicCoordinates)

connection :: ConstLine -> QDiagram SVG V2 Double Any -> QDiagram SVG V2 Double Any
connection cl d =
    connect' (with & arrowHead .~ noHead 
                   & shaftStyle %~ lc white . lw thick) (pt1 cl) (pt2 cl) d

perim = atPoints (trailVertices $ regPoly 240 0.07) rot_ticks

ticks = cycle [tick, tick, tick, tick, (starDiagram # scale 0.06 # scaleX 1.6)]

rot_ticks = zipWith (\a b -> a # (rotate ((((b-0.5)/240) @@ turn)))) ticks [0..]

tick = square 0.07
    # lw none
    # fc white
    # scaleY 0.15

starfig :: ApparentStar -> QDiagram SVG V2 Double Any
starfig apparentStar = starDiagram # scale (0.006 * (10-(Luminous.visualMagnitude apparentStar))) 
    # rotate ((Luminous.visualMagnitude apparentStar) @@ rad)

type StarChart = QDiagram SVG V2 Double Any

starDiagram :: Diagram B
starDiagram = polygon (with & polyType .~ PolyPolar (repeat (36 @@ deg)) (take 10 (cycle [0.25,0.5])))
    # fc white
    # lw veryThin
    # lc blue

isAboveHorizon :: GeographicCoordinates -> JulianDate -> EquatorialCoordinates1 -> Bool
isAboveHorizon geoCoords date equCoords = altitude > 0
  where
    altitude = hAltitude horizonCoords
    horizonCoords = ec1ToHC geoCoords date equCoords

-- what position should a given sky location take in the rendered diagram?
objectPosition :: GeographicCoordinates -> JulianDate -> Projection -> EquatorialCoordinates1 -> P2 Double
objectPosition geoCoords date projection equCoords = projection horizonCoords
  where
    horizonCoords = ec1ToHC geoCoords date equCoords

lineEndpoint :: [Star] -> HipparcosNumber -> Maybe EquatorialCoordinates1
lineEndpoint stars number = Stars.Types.location <$> find (\star -> hipparcosNumber star == Just number ) stars

pointPair :: [Star] -> ConstLine -> Maybe (EquatorialCoordinates1, EquatorialCoordinates1)
pointPair stars constLine = (\x y -> (x, y)) <$> maybeEndpoint1 <*> maybeEndpoint2
  where
    maybeEndpoint1 = lineEndpoint stars (pt1 constLine)
    maybeEndpoint2 = lineEndpoint stars (pt2 constLine)

diagramWithBorder :: [Star] -> [ApparentStar] -> [ConstLine] -> GeographicCoordinates -> JulianDate -> QDiagram SVG V2 Double Any
--diagramWithBorder allStars apparentStars cls geoCoords date = foldr ($) all_starfigs connections `beneath` all_starfigs `atop` perim `atop` square 5.48 # fc blue # lw none
diagramWithBorder allStars apparentStars cls geoCoords date = (foldr (atop) all_starfigs connections) `beneath` all_starfigs `atop` square 5.48 # fc blue # lw none
    where positioner = objectPosition geoCoords date azimuthalEquidistant
          points = (positioner . Location.location) <$> apparentStars
          starfigs = starfig <$> apparentStars
          all_starfigs = atPoints points starfigs
          line_endpoints = catMaybes $ (pointPair allStars) <$> cls
          filtered_endpoints = (filter (\(p1, p2) -> isAboveHorizon geoCoords date p1 && isAboveHorizon geoCoords date p2)) line_endpoints
          positioned_endpoints = (\(p1, p2) -> (objectPosition geoCoords date azimuthalEquidistant p1, objectPosition geoCoords date azimuthalEquidistant p2)) <$> filtered_endpoints
          connections = (\(p1, p2) -> strokeLine ( fromVertices [p1, p2]) # moveTo p1 # lc gold # lw veryThin) <$> positioned_endpoints

render_svg_starchart :: String -> StarChart -> IO ()
render_svg_starchart outPath starChart = do
    renderSVG outPath (mkSizeSpec2D (Just 1800) (Just 1800)) starChart

make_svg :: [Star] -> [ApparentStar] -> [ConstLine] -> GeographicCoordinates -> JulianDate -> String -> IO ()
make_svg allStars apparentStars cls geoCoords date outPath = do
        let visible_stars = (filter (\x-> (isAboveHorizon geoCoords date (Location.location x)) && Luminous.visualMagnitude x < 7.0)) apparentStars
        let sortedBrightest = sortOn Luminous.visualMagnitude visible_stars
        render_svg_starchart outPath $ diagramWithBorder allStars sortedBrightest cls geoCoords date

