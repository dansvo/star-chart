module Stars.Types where

import Data.List (minimumBy)
import Location
import Luminous
import Data.Astro.Coordinate

type HipparcosNumber = Int
type Constellation = String

data Star = Star 
    { hipparcosNumber :: Maybe HipparcosNumber
    , constellation   :: Constellation
    , bayer  :: String    -- Bayer designation
    , properName   :: String    -- Proper name, like "Sirius"
    , vmag   :: VisualMagnitude
    , location :: EquatorialCoordinates1
    } deriving Show

instance Luminous.Luminous Star where
    visualMagnitude s = vmag s

instance Location.Located Star where
    location star = Stars.Types.location star

instance Eq Star where
    (==) star1 star2 = (hipparcosNumber star1) == (hipparcosNumber star2)

data ConstLine = ConstLine
    { pt1 :: HipparcosNumber
    , pt2 :: HipparcosNumber
    } deriving Show

brightest :: [Star] -> Star
brightest = minimumBy (\s1 s2 -> compare (vmag s1) (vmag s2))

total_vmag :: [Star] -> Double
total_vmag = Luminous.add_vmags . (fmap vmag)

