module Main
  ( main
  )
where

import           Test.Hspec
import           Day1Test
import           Day2Test
import           Day3Test

main :: IO ()
main = hspec $ do
  day1
  day2
  day3
