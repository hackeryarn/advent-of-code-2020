module Main
  ( main
  )
where

import           Test.Hspec
import           Day1Test
import           Day2Test

main :: IO ()
main = hspec $ do
  day1
  day2
