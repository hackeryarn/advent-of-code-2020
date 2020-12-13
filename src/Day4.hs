{-# LANGUAGE OverloadedStrings #-}
-- | Passport Processing

module Day4 where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Either
import           Text.Parsec
import           Text.Parsec.Perm

data Passport = Passport
  { passportBirthYear      :: String
  , passportIssueYear      :: String
  , passportExpirationYear :: String
  , passportHeight         :: String
  , passportHairColor      :: String
  , passportEyeColor       :: String
  , passportID             :: String
  , passportCountryID      :: Maybe String
  }
  deriving (Show, Eq)

parsePassportField :: Parsec T.Text st String
parsePassportField = many (alphaNum <|> char '#') <* skipMany space

parsePassport :: Parsec T.Text st Passport
parsePassport =
  permute
    $    Passport
    <$$> try (string "byr:" *> parsePassportField)
    <||> try (string "iyr:" *> parsePassportField)
    <||> try (string "eyr:" *> parsePassportField)
    <||> try (string "hgt:" *> parsePassportField)
    <||> try (string "hcl:" *> parsePassportField)
    <||> try (string "ecl:" *> parsePassportField)
    <||> try (string "pid:" *> parsePassportField)
    <|?> (Nothing, fmap Just (try (string "cid:" *> parsePassportField)))

solve :: IO ()
solve = do
  let fname = "input/day4.txt"
  input <- T.readFile fname
  let passports = parse parsePassport fname <$> T.splitOn "\n\n" input
  print . length . filter isRight $ passports
