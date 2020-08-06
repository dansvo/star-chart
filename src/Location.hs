module Location where
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text

data Location = Location
    { rightAscention :: Double
    , declination :: Double
    } deriving (Eq, Show)

-- angular distance in radians between two points in spherical coordinates
angularDistance :: Location -> Location -> Double
angularDistance a b = acos $ (cos dec_a) * (cos dec_b) + (sin dec_a) * (sin dec_b) * cos (ra_a - ra_b)
    where
        ra_a  = rightAscention a
        ra_b  = rightAscention b
        dec_a = declination a
        dec_b = declination b

