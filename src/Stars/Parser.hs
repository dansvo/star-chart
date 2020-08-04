module Stars.Parser where

import Stars.Types
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine)
import Text.Parsec.Number
import Control.Monad (ap)

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
        mbe_ra = fmap (\x -> x * pi / 12) $ parseDouble ra_str :: Maybe Double
        mbe_dec = fmap (\x -> x * pi / 180) $ parseDouble dec_str :: Maybe Double
        mbe_vmag = parseDouble vmag_str :: Maybe Double
    return $ Star mbe_hip cstltn bayer proper <$> mbe_vmag <*> (Location <$> mbe_ra <*> mbe_dec)

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
    num1 <- many1 digit
    many1 (char ' ')
    num2 <- many1 digit
    return $ ConstLine num1 num2

constLine :: GenParser Char st [ConstLine]
constLine = do
    constName <- many1 alphaNum
    many1 (char ' ')
    line_count <- many1 digit
    hps <- many (many1 (char ' ') *> hipnumPair)
    many (char ' ')
    return hps

clFile :: GenParser Char st [ConstLine]
clFile = concat <$> sepEndBy constLine endOfLine <* eof

parseCLFile :: String -> Either ParseError [ConstLine]
parseCLFile = parse clFile "clfile input"

