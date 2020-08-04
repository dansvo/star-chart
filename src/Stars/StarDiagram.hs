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
test_diag3 groups cls = foldr ($) all_starfigs connections `beneath` all_starfigs `atop` perim `atop` square 5.48 # fc blue # lw none
    where points = (basic_projection . lctn . head) <$> groups
          starfigs = starfig <$> groups
          all_starfigs = atPoints points starfigs
          connections = connection <$> cls

connection :: ConstLine -> QDiagram SVG V2 Double Any -> QDiagram SVG V2 Double Any
connection cl d =
    connect' (with & arrowHead .~ noHead 
                   & shaftStyle %~ lc white . lw veryThin) (pt1 cl) (pt2 cl) d

perim = atPoints (trailVertices $ regPoly 240 0.07) rot_ticks

ticks = cycle [tick, tick, tick, tick, (onestar # scale 0.06 # scaleX 1.6)]

rot_ticks = zipWith (\a b -> a # (rotate ((((b-0.5)/240) @@ turn)))) ticks [0..]

tick = square 0.07
    # lw none
    # fc white
    # scaleY 0.15

starfig :: [Star] -> QDiagram  SVG V2 Double Any
starfig ss = onestar # scale (0.006 * (10-(total_vmag ss))) 
    # rotate ((total_vmag ss) @@ rad) 
--    # named hip_string
    where hip_string = ((show . get_hipnum) (brightest ss))

type StarChart = QDiagram SVG V2 Double Any

render_svg_starchart :: String -> StarChart -> IO ()
render_svg_starchart outPath diagram = do
    renderSVG outPath (mkSizeSpec2D (Just 1800) (Just 1800)) diagram

make_svg :: [[Star]] -> [ConstLine] -> String -> IO ()
make_svg grouped_stars cls outPath = do
        let brightest = (filter (\x-> (declination . lctn . head) x > -1 && total_vmag x < 7.0)) grouped_stars
        let sortedBrightest = sortOn total_vmag brightest
        render_svg_starchart outPath $ test_diag3 sortedBrightest cls
