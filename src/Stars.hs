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
import Options.Applicative
import Data.Astro.Time.JulianDate
import Data.Astro.Types

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

data CommandLineOptions = CommandLineOptions
    { star_file :: String
    , constellation_file :: String
    , out_path :: String
    } deriving Show

commandline_parser :: Parser CommandLineOptions
commandline_parser = CommandLineOptions
    <$> strOption
        (  long "star_file"
        <> help "path to csv star file" )
    <*> strOption
        (  long "constellation_file"
        <> help "path to constellation line file" )
    <*> strOption
        (  long "out_path"
        <> help "path to file that should be produced" )

parser_info = info commandline_parser (header "TEST")

main :: IO ()
main = do
    options <- execParser parser_info
    putStrLn "parsing star file"
    ethMAllStars <- parseStarFile <$> readFile ( star_file options )
    let ethAllStars = catMaybes <$> ethMAllStars
    ethApparentStars <- (fmap.fmap) filterStars $ parseStarFile <$> readFile ( star_file options )
    putStrLn "parsing constellation file"
    ethConstLines <- parseCLFile <$> readFile ( constellation_file options )
    case ethConstLines of
        (Left e) -> error $ show e
        (Right constLines) -> do
            let outPath = out_path options
            putStrLn $ "rendering"
            sequenceA $ make_svg <$> ethAllStars <*> ethApparentStars <*> pure constLines <*> pure (GeoC 42 0) <*> pure (JD 0) <*> pure outPath -- ethConstLines
            return ()

