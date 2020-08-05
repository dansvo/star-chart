module Stars where

{-# LANGUAGE FlexibleContexts #-}

import System.Environment (getArgs)
import Stars.Types
import Stars.Parser
import Data.Maybe (catMaybes)
import Stars.StarDiagram
import Text.Parsec (ParseError)

-- functions to deal with the fact that visual binaries appear
-- as a single star to the unaided eye

star_belongs :: Star -> [Star] -> Bool
star_belongs star stars = any id [angularDistance (location star) (location x) < 0.0020 | x <- stars]

vis_list :: [Star] -> [[Star]]
vis_list stars = foldr f [] stars
    where
        f star []     = [[star]]
        f star (x:xs) = case star_belongs star x of
            True  -> (star:x):xs
            False -> x:(f star xs)

filterStars :: [Maybe Star] -> [[Star]]
filterStars xs = vis_list $ (filter (\x -> vmag x < 7.0 && properName x /= "Sol")) (catMaybes xs)

main :: IO ()
main = do
    args <- getArgs
    putStrLn "parsing star file"
    ethStars <- (fmap.fmap) filterStars $ parseStarFile <$> readFile ( args !! 0 )
    putStrLn "parsing constellation file"
    ethConstLines <- parseCLFile <$> readFile ( args !! 1 )
    let outPath = args !! 2
    putStrLn $ "rendering"
    sequenceA $ make_svg <$> ethStars <*> pure [] <*> pure outPath-- ethConstLines
    return ()

