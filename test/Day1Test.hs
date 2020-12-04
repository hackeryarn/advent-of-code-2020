module Day1Test where

import           Test.Hspec
import           Day1

day1 :: Spec
day1 = describe "productOfSumTo2020" $ do
  let sample = [1721, 979, 366, 299, 675, 1456]
  it "returns the product of two numbers that sum up to 2020"
    $          productOfSumTo2020 sample
    `shouldBe` 514579
