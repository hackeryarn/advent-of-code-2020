{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Passport Processing

module Day4 where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Either
import           Text.Parsec
import           Text.Parsec.Perm
import           Text.Parsec.Text

data Passport = Passport
  { passportBirthYear      :: Year
  , passportIssueYear      :: Year
  , passportExpirationYear :: Year
  , passportHeight         :: Height
  , passportHairColor      :: HairColor
  , passportEyeColor       :: EyeColor
  , passportID             :: PassportId
  , passportCountryID      :: Maybe String
  }
  deriving (Show, Eq)

newtype Year = Year Int
  deriving (Show, Eq)

data Height = Height
  { heightNum     :: Int
  , heightMeasure :: String
  }
  deriving (Show, Eq)

newtype HairColor = HairColor String
  deriving (Show, Eq)

newtype EyeColor = EyeColor String
  deriving (Show, Eq)

newtype PassportId = PassportId Int
  deriving (Show, Eq)

parsePassport :: Parser Passport
parsePassport =
  permute
    $    Passport
    <$$> try (string "byr:" *> parseYear)
    <||> try (string "iyr:" *> parseYear)
    <||> try (string "eyr:" *> parseYear)
    <||> try (string "hgt:" *> parseHeight)
    <||> try (string "hcl:" *> parseHairColor)
    <||> try (string "ecl:" *> parseEyeColor)
    <||> try (string "pid:" *> parsePassportId)
    <|?> (Nothing, fmap Just (try (string "cid:" *> parsePassportField)))

parsePassportField :: Parser String
parsePassportField = many (alphaNum <|> char '#') <* skipMany space

parseYear :: Parser Year
parseYear = Year . read <$> count 4 digit <* skipMany space

parseHeight :: Parser Height
parseHeight = do
  heightNum     <- read <$> many1 digit
  heightMeasure <- count 2 letter
  skipMany space
  pure $ Height { .. }

parseHairColor :: Parser HairColor
parseHairColor =
  HairColor
    <$> (  char '#'
        *> count 6 (digit <|> oneOf ['a', 'b', 'c', 'd', 'e', 'f'])
        <* skipMany space
        )

parseEyeColor :: Parser EyeColor
parseEyeColor =
  EyeColor
    <$> (   try (string "amb")
        <|> try (string "blu")
        <|> try (string "brn")
        <|> try (string "gry")
        <|> try (string "grn")
        <|> try (string "hzl")
        <|> try (string "oth")
        )
    <*  skipMany space

parsePassportId :: Parser PassportId
parsePassportId = PassportId . read <$> count 9 digit <* skipMany space

validateBirthYear :: Year -> Bool
validateBirthYear (Year by) = by >= 1920 && by <= 2002

validateIssueYear :: Year -> Bool
validateIssueYear (Year iy) = iy >= 2010 && iy <= 2020

validateExpirationYear :: Year -> Bool
validateExpirationYear (Year ey) = ey >= 2020 && ey <= 2030

validateHeight :: Height -> Bool
validateHeight (Height cm     "cm") = cm >= 150 && cm <= 193
validateHeight (Height inches "in") = inches >= 59 && inches <= 76
validateHeight _                    = False

validatePassport :: Passport -> Bool
validatePassport Passport {..} =
  validateBirthYear passportBirthYear
    && validateIssueYear passportIssueYear
    && validateExpirationYear passportExpirationYear
    && validateHeight passportHeight

solve :: IO ()
solve = do
  let fname = "input/day4.txt"
  input <- T.readFile fname
  let passports = parse parsePassport fname <$> T.splitOn "\n\n" input
  print . length . filter validatePassport . rights $ passports
