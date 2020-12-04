-- | Day 1: Report Repair

module Day1 where

import           Data.List

productOfSumTo2020 :: [Int] -> Int
productOfSumTo2020 fullList = go fullList
 where
  go []       = 0
  go (x : xs) = case find (\y -> x + y == 2020) fullList of
    Nothing -> go xs
    Just y  -> x * y
