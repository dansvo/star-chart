module Stars.Types where

import Data.List (minimumBy)
import Location

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

brightest :: [Star] -> Star
brightest = minimumBy (\s1 s2 -> compare (vmag s1) (vmag s2))

data ConstLine = ConstLine
    { pt1 :: String
    , pt2 :: String
    } deriving Show
