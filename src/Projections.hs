module Projections where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text
import Stars.Types
import Location
import Data.Astro.Coordinate

type Projection = HorizonCoordinates -> P2 Double

azimuthalEquidistant :: Projection
azimuthalEquidistant (HC altitude_degrees azimuth_degrees) = point_polar (pi/2 - altitude) azimuth
  where
    altitude = degreesToRadians altitude_degrees
    azimuth = degreesToRadians azimuth_degrees

point_polar :: Double -> Double -> P2 Double
point_polar r theta = origin & _r +~r & _theta <>~ (theta + (pi/2) @@ rad)
