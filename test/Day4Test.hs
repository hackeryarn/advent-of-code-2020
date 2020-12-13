{-# LANGUAGE LambdaCase #-}
module Day4Test where

import qualified Data.Text                     as T
import           Test.Hspec
import           Text.Parsec
import           Day4

day4 :: Spec
day4 = do
  describe "parsePassport" $ do
    let
      passport =
        T.pack
          "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"
    it "parses passport with all fields"
      $          parse parsePassport "" passport
      `shouldBe` Right
                   (Passport { passportBirthYear      = 1937
                             , passportIssueYear      = 2017
                             , passportExpirationYear = 2020
                             , passportHeight         = "183cm"
                             , passportHairColor      = "#fffffd"
                             , passportEyeColor       = "gry"
                             , passportID             = 860033327
                             , passportCountryID      = Just 147
                             }
                   )
    let
      passport =
        T.pack
          "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm"
    it "parses passport with missing cid"
      $          parse parsePassport "" passport
      `shouldBe` Right
                   (Passport { passportBirthYear      = 1931
                             , passportIssueYear      = 2013
                             , passportExpirationYear = 2024
                             , passportHeight         = "179cm"
                             , passportHairColor      = "#ae17e1"
                             , passportEyeColor       = "brn"
                             , passportID             = 760753108
                             , passportCountryID      = Nothing
                             }
                   )
    let
      passport =
        T.pack
          "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929"
    it "does not parse passport with missing fields"
      $               parse parsePassport "" passport
      `shouldSatisfy` (\case
                        (Left _)  -> True
                        otherwise -> False
                      )
    let passport =
          T.pack "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
    it "does not parse another invalid passport"
      $               parse parsePassport "" passport
      `shouldSatisfy` (\case
                        (Left _)  -> True
                        otherwise -> False
                      )
