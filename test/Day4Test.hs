module Day4Test where

import qualified Data.Text                     as T
import           Test.Hspec
import           Text.Parsec
import           Day4

isValidPassport :: Either pe Passport -> Bool
isValidPassport (Right _) = True
isValidPassport (Left  _) = False

day4 :: Spec
day4 = do
  describe "parsePassport" $ do
    let
      passport =
        T.pack
          "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
    it "parses valid passport 1"
      $               parse parsePassport "" passport
      `shouldSatisfy` isValidPassport
    let
      passport =
        T.pack
          "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
    it "parses valid passport 2"
      $               parse parsePassport "" passport
      `shouldSatisfy` isValidPassport
    let
      passport =
        T.pack
          "hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022"
    it "parses valid passport 3"
      $               parse parsePassport "" passport
      `shouldSatisfy` isValidPassport
    let
      passport =
        T.pack
          "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
    it "parses valid passport 4"
      $               parse parsePassport "" passport
      `shouldSatisfy` isValidPassport
