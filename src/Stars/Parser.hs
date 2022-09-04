module Stars.Parser where

import Stars.Types
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine, char)
import Text.Parsec.Number
import Control.Monad (ap)
import Location
import Data.Astro.Coordinate

-- Star Database Parser

starLine :: GenParser Char st (Maybe Star)
starLine = do
    _        <- cellComma
    hip      <- cellComma
    _        <- count 4 cellComma
    proper   <- cellComma
    ra_str   <- cellComma
    dec_str  <- cellComma
    _        <- count 4 cellComma
    vmag_str <- cellComma
    _        <- count 13 cellComma
    bayer    <- cellComma
    _        <- cellComma
    cstltn   <- cellComma
    _        <- count 6 cellComma *> cell
    let mbe_hip = parseInt hip :: Maybe Int
        mbe_dec = fmap (\x -> Data.Astro.Coordinate.DD x) $ parseDouble dec_str :: Maybe DecimalDegrees
        mbe_ra = fmap (\x -> Data.Astro.Coordinate.DH x) $ parseDouble ra_str :: Maybe DecimalHours
        mbe_vmag = parseDouble vmag_str :: Maybe Double
    return $ Star mbe_hip cstltn bayer proper <$> mbe_vmag <*> (Data.Astro.Coordinate.EC1 <$> mbe_dec <*> mbe_ra)

cellComma :: GenParser Char st String
cellComma = cell <* commaP

cell :: GenParser Char st String
cell = many $ noneOf ",\n"

commaP :: GenParser Char st Char
commaP = char ','

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

parseInt :: String -> Maybe Int
parseInt t = rightToMaybe $ parse int "" t

parseDouble :: String -> Maybe Double
parseDouble t = rightToMaybe $ parse (ap sign (floating3 False)) "" t

headerLine :: GenParser Char st [String]
headerLine = sepBy cell commaP <* char '\n'

starFile :: GenParser Char st [Maybe Star]
starFile = headerLine *> sepEndBy starLine endOfLine <* eof

parseStarFile :: String -> Either ParseError [Maybe Star]
parseStarFile = parse starFile "starfile input"

-- Constellation File Parser

hipnumPair :: GenParser Char st ConstLine
hipnumPair = do
    num1 <- int
    tab
    num2 <- int
    return $ ConstLine num1 num2

constLine :: GenParser Char st [ConstLine]
constLine = do
    many comment
    constName <- many1 (alphaNum <|> char '.')
    tab
    line_count <- many1 digit
    tab
    pairs <- sepEndBy hipnumPair tab
    pure pairs

comment :: GenParser Char st ()
comment = do
    Text.Parsec.Char.char '#'
    manyTill anyChar newline
    many newline
    pure ()
    

clFile :: GenParser Char st [ConstLine]
clFile = concat <$> sepEndBy constLine endOfLine <* eof

parseCLFile :: String -> Either ParseError [ConstLine]
parseCLFile = parse clFile "clfile input"

