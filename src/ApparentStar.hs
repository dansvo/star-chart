module ApparentStar where

import Data.List.NonEmpty
import Luminous
import Location
import Stars.Types (Star(..))

-- Stars which are very close in the sky, either because they belong
-- to star systems or because they are visual doubles, appear as a single
-- point to the naked eye. Star catalogs list these as separate stars though,
-- since astronomers care about the separate stars. The ApparentStar type in
-- this module represents a single _apparent_ star, which may actually be a
-- group of several stars whose angular distances are very close from earth.

data ApparentStar = ApparentStar
    { stars :: NonEmpty Star
    }

instance Luminous ApparentStar where
    visualMagnitude (ApparentStar stars) = add_vmags $ vmag <$> stars

instance Located ApparentStar where
    location (ApparentStar stars) = Location.location $ Data.List.NonEmpty.head $ stars

