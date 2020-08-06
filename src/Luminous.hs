module Luminous where

class Luminous a where
    visualMagnitude :: a -> VisualMagnitude

type VisualMagnitude = Double

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

