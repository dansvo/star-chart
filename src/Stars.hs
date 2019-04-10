module Stars where

{-# LANGUAGE FlexibleContexts #-}

import Stars.Types
import Stars.Parser
import Data.Maybe (catMaybes)
import Stars.StarDiagram
import Text.Parsec (ParseError)


-- functions to deal with the fact that visual binaries appear
-- as a single star to the unaided eye

star_belongs :: Star -> [Star] -> Bool
star_belongs star stars = any id [angDist (lctn star) (lctn x) < 0.0020 | x <- stars]

vis_list :: [Star] -> [[Star]]
vis_list stars = foldr f [] stars
    where
        f star []     = [[star]]
        f star (x:xs) = case star_belongs star x of
            True  -> (star:x):xs
            False -> x:(f star xs)

filterStars :: [Maybe Star] -> [[Star]]
filterStars xs = vis_list $ (filter (\x -> vmag x < 7.0 && prop x /= "Sol")) (catMaybes xs)

main :: IO ()
main = do
    putStrLn "parsing star file"
    ethStars <- (fmap.fmap) filterStars $ parseStarFile <$> readFile "./HYG-Database/hygdata_v3.csv"
    putStrLn "parsing constellation file"
    -- ethConstLines <- parseCLFile <$> readFile "path/to/constellation/file" -- ./src/constellationship.fab.empty"
    let ethConstLines = Right []
    sequenceA $ make_svg <$> ethStars <*> ethConstLines    
    return ()

