module Projections where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text
import Stars.Types

type Projection = Location -> P2 Double

azimuthalEquidistant :: Projection
azimuthalEquidistant location = point_polar (pi/2 - (declination location)) (-(rightAscention location))

point_polar :: Double -> Double -> P2 Double
point_polar r theta = origin & _r +~r & _theta <>~ (theta @@ rad)
