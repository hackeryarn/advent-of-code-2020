module Main
  ( main
  )
where

import           Test.Hspec
import           Day1Test

main :: IO ()
main = hspec $ do
  day1
