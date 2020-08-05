module Stars.Types where
import Data.List (minimumBy)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text

data Location = Location
    { rightAscention :: Double
    , declination :: Double
    } deriving (Eq, Show)

type Projection = Location -> P2 Double

azimuthalEquidistant :: Projection
azimuthalEquidistant location = point_polar (pi/2 - (declination location)) (-(rightAscention location))

point_polar :: Double -> Double -> P2 Double
point_polar r theta = origin & _r +~r & _theta <>~ (theta @@ rad)

-- angular distance in radians between two points in spherical coordinates
angularDistance :: Location -> Location -> Double
angularDistance a b = acos $ (cos dec_a) * (cos dec_b) + (sin dec_a) * (sin dec_b) * cos (ra_a - ra_b)
    where
        ra_a  = rightAscention a
        ra_b  = rightAscention b
        dec_a = declination a
        dec_b = declination b

type HipparcosNumber = Int
type Constellation = String

data Star = Star 
    { hipparcosNumber :: Maybe HipparcosNumber
    , constellation   :: Constellation
    , bayer  :: String    -- Bayer designation
    , properName   :: String    -- Proper name, like "Sirius"
    , vmag   :: Double    -- visual magnitude
    , location   :: Location
    } deriving Show

instance Eq Star where
    (==) star1 star2 = (hipparcosNumber star1) == (hipparcosNumber star2)

----------------
-- functions to add visual magnitudes of stars
----------------
-- this method for adding star magnitudes taken from the following forum post:
-- www.physicsforum.com/threads/apparent-magnitude-of-two-stars.155292
vmag_to_lum :: Floating a => a -> a
vmag_to_lum vmag = 10 ** (vmag / (-2.5))

lum_to_vmag :: Floating a => a -> a
lum_to_vmag lum = (-2.5) * (logBase 10 lum)

add_vmags :: (Functor f, Foldable f, Floating a) => f a -> a
add_vmags  = lum_to_vmag . sum . (fmap vmag_to_lum)

total_vmag :: [Star] -> Double
total_vmag = add_vmags . (fmap vmag)

get_hipnum :: Star -> Int
get_hipnum (Star (Just x) _ _ _ _ _) = x
get_hipnum _ = (-1)

brightest :: [Star] -> Star
brightest = minimumBy (\s1 s2 -> compare (vmag s1) (vmag s2))

data ConstLine = ConstLine
    { pt1 :: String
    , pt2 :: String
    } deriving Show
