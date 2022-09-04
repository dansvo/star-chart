module Location where
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text
import Data.Astro.Coordinate

-- angular distance in radians between two points in spherical coordinates
angularDistance :: EquatorialCoordinates1 -> EquatorialCoordinates1 -> Double
angularDistance a b = acos $ (cos dec_a) * (cos dec_b) + (sin dec_a) * (sin dec_b) * cos (ra_a - ra_b)
    where
        ra_a  = hoursToRadians $ e1RightAscension a
        ra_b  = hoursToRadians $ e1RightAscension b
        dec_a = degreesToRadians $ e1Declination a
        dec_b = degreesToRadians $ e1Declination b

class Located a where
    location :: a -> EquatorialCoordinates1

degreesToRadians :: DecimalDegrees -> Double
degreesToRadians (DD x) = x * pi / 180

hoursToRadians :: DecimalHours -> Double
hoursToRadians (DH x) = x * pi / 12
