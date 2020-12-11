module Day3Test where

import           Test.Hspec
import           Day3
import qualified Data.ByteString.Char8         as BS

sample :: [String]
sample =
  [ "..##......."
  , "#...#...#.."
  , ".#....#..#."
  , "..#.#...#.#"
  , ".#...##..#."
  , "..#.##....."
  , ".#.#.#....#"
  , ".#........#"
  , "#.##...#..."
  , "#...##....#"
  , ".#..#...#.#"
  ]

day3 :: Spec
day3 = do
  describe "treesHit" $ do
    let path = Path (MovesRight 3) (MovesDown 1)
    it "" $ treesHit path sample `shouldBe` 7
