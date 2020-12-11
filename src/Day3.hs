{-# LANGUAGE RecordWildCards #-}
-- | Day 3: Toboggan Trajectory

module Day3 where

import qualified Data.ByteString.Char8         as BS
import           Relude.List                    ( (!!?) )

newtype MovesRight = MovesRight Int

newtype MovesDown = MovesDown Int

data Path = Path
  { pathMovesRight :: MovesRight
  , pathMovesDown  :: MovesDown
  }

treesHit :: Path -> [String] -> Int
treesHit Path {..} ss = go 0
 where
  (MovesDown d) = pathMovesDown
  go i = case ss !!? (i * d) of
    (Just s) -> (if isTree pathMovesRight i s then 1 else 0) + go (i + 1)
    Nothing  -> 0

tree :: Char
tree = '#'

isTree :: MovesRight -> Int -> String -> Bool
isTree (MovesRight r) i s = (cycle s !! (i * r)) == tree

paths :: [Path]
paths =
  [ Path (MovesRight 1) (MovesDown 1)
  , Path (MovesRight 3) (MovesDown 1)
  , Path (MovesRight 5) (MovesDown 1)
  , Path (MovesRight 7) (MovesDown 1)
  , Path (MovesRight 1) (MovesDown 2)
  ]

readInput :: IO [[String]]
readInput = pure . map BS.unpack . BS.lines <$> BS.readFile "input/day3.txt"

-- product . (map treesHit paths <*>) <$> readInput
