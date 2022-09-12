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

holeyStar :: Double -> Double -> Int -> Diagram B
holeyStar innerDiameter outerDiameter spokeCount = polygon ( with & polyType .~ PolyPolar
    (cycle [0.0 @@ turn, 0.0 @@ turn, (1 / (fromIntegral spokeCount)) @@ turn])
    (take (3 * spokeCount) (cycle [innerDiameter, outerDiameter, innerDiameter])))
        # lwG 0.6
        # lc white
        # lineCap LineCapRound


mag0 = holeyStar 1.00 2.0 8
mag1 = holeyStar 1.00 2.0 6
mag2 = holeyStar 1.00 2.0 5
mag3 = holeyStar 0.0001 1.2 6
mag4 = holeyStar 0.0001 0.4 5
mag5 = square 0.005 # lwG 0.45 # lc white # fc white # lineCap LineCapRound

starfig :: ApparentStar -> QDiagram SVG V2 Double Any
starfig apparentStar 
    | Luminous.visualMagnitude apparentStar < 1.0 = mag0 # rotate ((Luminous.visualMagnitude apparentStar) @@ rad)
    | Luminous.visualMagnitude apparentStar < 2.0 = mag1 # rotate ((Luminous.visualMagnitude apparentStar) @@ rad)
    | Luminous.visualMagnitude apparentStar < 3.0 = mag2 # rotate ((Luminous.visualMagnitude apparentStar) @@ rad)
    | Luminous.visualMagnitude apparentStar < 4.0 = mag3 # rotate ((Luminous.visualMagnitude apparentStar) @@ rad)
    | Luminous.visualMagnitude apparentStar < 5.0 = mag4 # rotate ((Luminous.visualMagnitude apparentStar) @@ rad)
    | otherwise                                 = mag5 # rotate ((Luminous.visualMagnitude apparentStar) @@ rad)

type StarChart = QDiagram SVG V2 Double Any

isAboveHorizon :: GeographicCoordinates -> JulianDate -> EquatorialCoordinates1 -> Bool
isAboveHorizon geoCoords date equCoords = altitude > 0
  where
    altitude = hAltitude horizonCoords
    horizonCoords = ec1ToHC geoCoords date equCoords

-- what position should a given sky location take in the rendered diagram?
objectPosition :: GeographicCoordinates -> JulianDate -> Projection -> EquatorialCoordinates1 -> P2 Double
objectPosition geoCoords date projection equCoords = projection horizonCoords # scale 90.0
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
diagramWithBorder allStars apparentStars cls geoCoords date = (foldr (atop) mempty connections) `beneath` all_starfigs `atop` square 305.0 # fc blue # lw none
    where positioner = objectPosition geoCoords date azimuthalEquidistant
          points = (positioner . Location.location) <$> apparentStars
          starfigs = starfig <$> apparentStars
          all_starfigs = atPoints points starfigs
          line_endpoints = catMaybes $ (pointPair allStars) <$> cls
          filtered_endpoints = (filter (\(p1, p2) -> isAboveHorizon geoCoords date p1 || isAboveHorizon geoCoords date p2)) line_endpoints
          positioned_endpoints = (\(p1, p2) -> (objectPosition geoCoords date azimuthalEquidistant p1, objectPosition geoCoords date azimuthalEquidistant p2)) <$> filtered_endpoints
          connections = (\(p1, p2) -> strokeLine ( fromVertices [p1, p2]) # moveTo p1 # lc gold # lwG 0.6) <$> positioned_endpoints

render_svg_starchart :: String -> StarChart -> IO ()
render_svg_starchart outPath starChart = do
    renderSVG outPath (mkSizeSpec2D (Just 1800) (Just 1800)) starChart

make_svg :: [Star] -> [ApparentStar] -> [ConstLine] -> GeographicCoordinates -> JulianDate -> String -> IO ()
make_svg allStars apparentStars cls geoCoords date outPath = do
        let visible_stars = (filter (\x-> (isAboveHorizon geoCoords date (Location.location x)) && Luminous.visualMagnitude x < 7.0)) apparentStars
        let sortedBrightest = sortOn Luminous.visualMagnitude visible_stars
        render_svg_starchart outPath $ diagramWithBorder allStars sortedBrightest cls geoCoords date

