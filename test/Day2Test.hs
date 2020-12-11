module Day2Test where

import           Test.Hspec
import           Day2
import qualified Data.ByteString.Char8         as BS

day2 :: Spec
day2 = do
  describe "countCorrectPasswords" $ do
    let sample = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
    it "finds the correct number of passwords"
      $          (countCorrectPasswordsBy checkPasswordInfo) (BS.pack sample)
      `shouldBe` 2
  describe "countCorrectPasswords'" $ do
    let sample = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
    it "finds the correct number of passwords"
      $          (countCorrectPasswordsBy checkPasswordInfo') (BS.pack sample)
      `shouldBe` 1
