module Stars.StarDiagram where

import Stars.Types
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text
import Data.Maybe (catMaybes)
import Data.List (sortOn)

printEachElem :: Show a => [a] -> IO ()
printEachElem xs = traverse (putStrLn . show) xs *> return ()

onestar :: Diagram B
onestar = polygon (with & polyType .~ PolyPolar (repeat (36 @@ deg)) (take 10 (cycle [0.25,0.5])))
    # fc white
    # lw veryThin
    # lc blue

test_diag3 :: [[Star]] -> [ConstLine] -> QDiagram  SVG V2 Double Any
test_diag3 groups cls = foldr ($) all_starfigs connections `beneath` all_starfigs `atop` perim `atop` square 5.48 # fc blue
    where points = (star_to_point . head) <$> groups
          starfigs = starfig <$> groups
          all_starfigs = atPoints points starfigs
          connections = connection <$> cls

connection :: ConstLine -> QDiagram SVG V2 Double Any -> QDiagram SVG V2 Double Any
connection cl d = connect' (with & arrowHead .~ noHead 
                                 & shaftStyle %~ lc white . lw veryThin) (pt1 cl) (pt2 cl) d

--perim = atPoints (trailVertices $ regPoly 48 0.3) (repeat (square 1))
perim = atPoints (trailVertices $ regPoly 240 0.07) rot_ticks

ticks = cycle [tick, tick, tick, tick, (onestar # scale 0.06 # scaleX 1.6)]

rot_ticks = zipWith (\a b -> a # (rotate ((((b-0.5)/240) @@ turn)))) ticks [0..]

tick = square 0.07
    # lw none
    # fc white
    # scaleY 0.15

starfig :: [Star] -> QDiagram  SVG V2 Double Any
--starfig ss = text hip_string # scale 0.05 `atop` onestar # scale (0.012 * (7-(total_vmag ss))) # rotate ((total_vmag ss) @@ rad) # named hip_string
starfig ss = onestar # scale (0.006 * (10-(total_vmag ss))) 
    # rotate ((total_vmag ss) @@ rad) 
    # named hip_string
    where hip_string = ((show . get_hipnum) (brightest ss))


star_to_point :: Star -> P2 Double
star_to_point s = point_polar (pi/2 - (dec starloc)) (-(ra starloc))
    where starloc = lctn s

point_polar :: Double -> Double -> P2 Double
point_polar r theta = origin & _r +~r & _theta <>~ (theta @@ rad)

render_svg_starchart :: SVGFloat n => QDiagram SVG V2 n Any -> IO ()
render_svg_starchart = renderSVG "./chart.svg" (mkSizeSpec2D (Just 1800) (Just 1800))

make_svg :: [[Star]] -> [ConstLine] -> IO ()
make_svg grouped_stars cls = do
        --putStrLn $ show $ (take 10) grouped_stars
        let brightest = (filter (\x-> (dec . lctn . head) x > -1 && total_vmag x < 7.0)) grouped_stars
        let sortedBrightest = sortOn total_vmag brightest
        render_svg_starchart $ test_diag3 sortedBrightest cls
