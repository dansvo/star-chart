module Stars.StarDiagram where

import Stars.Types
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import ApparentStar
import Projections
import Luminous
import Location

connection :: ConstLine -> QDiagram SVG V2 Double Any -> QDiagram SVG V2 Double Any
connection cl d =
    connect' (with & arrowHead .~ noHead 
                   & shaftStyle %~ lc white . lw veryThin) (pt1 cl) (pt2 cl) d

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

diagramWithBorder :: [ApparentStar] -> [ConstLine] -> QDiagram SVG V2 Double Any
diagramWithBorder apparentStars cls = foldr ($) all_starfigs connections `beneath` all_starfigs `atop` perim `atop` square 5.48 # fc blue # lw none
    where points = (azimuthalEquidistant . Location.location) <$> apparentStars
          starfigs = starfig <$> apparentStars
          all_starfigs = atPoints points starfigs
          connections = connection <$> cls

render_svg_starchart :: String -> StarChart -> IO ()
render_svg_starchart outPath starChart = do
    renderSVG outPath (mkSizeSpec2D (Just 1800) (Just 1800)) starChart

make_svg :: [ApparentStar] -> [ConstLine] -> String -> IO ()
make_svg apparentStars cls outPath = do
        let brightest = (filter (\x-> (declination . Location.location) x > -1 && Luminous.visualMagnitude x < 7.0)) apparentStars
        let sortedBrightest = sortOn Luminous.visualMagnitude brightest
        render_svg_starchart outPath $ diagramWithBorder sortedBrightest cls
