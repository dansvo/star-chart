module Stars where

{-# LANGUAGE FlexibleContexts #-}

import System.Environment (getArgs)
import Stars.Types
import Stars.Parser
import Data.Maybe (catMaybes)
import Stars.StarDiagram
import Text.Parsec (ParseError)
import Location
import Luminous (brighterThan)
import ApparentStar
import Data.List.NonEmpty hiding ((!!))

-- functions to deal with the fact that visual binaries appear
-- as a single star to the unaided eye

star_belongs :: (Location.Located a, Location.Located b) => a -> b -> Bool
star_belongs star apparentStar = Location.angularDistance (Location.location star) (Location.location apparentStar) < 0.0020

vis_list :: [Star] -> [ApparentStar]
vis_list stars = foldr f [] stars
    where
        f star []     = [ApparentStar (star:|[])]
        f star (x:xs) = case star_belongs star x of
            True  -> (ApparentStar (star<|(ApparentStar.stars x))):xs
            False -> x:(f star xs)

filterStars :: [Maybe Star] -> [ApparentStar]
filterStars xs = vis_list $ (Prelude.filter (\x -> x `brighterThan` 7.5 && properName x /= "Sol")) (catMaybes xs)

main :: IO ()
main = do
    args <- getArgs
    putStrLn "parsing star file"
    ethStars <- (fmap.fmap) filterStars $ parseStarFile <$> readFile ( args !! 0 )
    putStrLn "parsing constellation file"
    ethConstLines <- parseCLFile <$> readFile ( args !! 1 )
    let outPath = args !! 2
    putStrLn $ "rendering"
    sequenceA $ make_svg <$> ethStars <*> pure [] <*> pure outPath -- ethConstLines
    return ()

