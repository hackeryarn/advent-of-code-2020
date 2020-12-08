-- | Day 1: Report Repair

module Day1 where

import           Data.Maybe
import qualified Data.ByteString.Char8         as BS

productOfSumTo2020 :: [Int] -> Int
productOfSumTo2020 xs = head $ do
  x <- xs
  y <- tail xs
  [ x * y | x + y == 2020 ]

productOf3SumTo2020 :: [Int] -> Int
productOf3SumTo2020 xs = head $ do
  x <- xs
  y <- tail xs
  z <- tail $ tail xs
  [ x * y * z | x + y + z == 2020 ]

parseInput :: IO [Int]
parseInput =
  map (fst . fromJust . BS.readInt) . BS.lines <$> BS.readFile "input/day1.txt"
