{-# LANGUAGE RecordWildCards #-}
-- | Day2: Password Philosophy

module Day2 where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8         as BS

data Range = Range
  { rangeLow  :: !Int
  , rangeHigh :: !Int
  }
  deriving Show

data PasswordInfo = PasswordInfo
  { passwordInfoRange    :: !Range
  , passwordInfoChar     :: !Char
  , passwordInfoPassword :: !BS.ByteString
  }
  deriving Show

parseRange :: Parser Range
parseRange = do
  rangeLow  <- decimal
  _         <- char '-'
  rangeHigh <- decimal
  return Range { .. }


parsePasswordInfo :: Parser PasswordInfo
parsePasswordInfo = do
  passwordInfoRange <- parseRange
  skipSpace
  passwordInfoChar <- anyChar
  _                <- char ':'
  skipSpace
  passwordInfoPassword <- takeTill (== '\n')
  return PasswordInfo { .. }

countCorrectPasswordsBy :: (PasswordInfo -> Bool) -> BS.ByteString -> Int
countCorrectPasswordsBy check input =
  let infos = parseOnly (parsePasswordInfo `sepBy` endOfLine) input
  in  case infos of
        (Left  _ ) -> 0
        (Right is) -> length $ filter check is

checkPasswordInfo :: PasswordInfo -> Bool
checkPasswordInfo PasswordInfo {..} =
  let occurances =
        length . filter (== passwordInfoChar) . BS.unpack $ passwordInfoPassword
      Range {..} = passwordInfoRange
  in  rangeLow <= occurances && occurances <= rangeHigh

checkPasswordInfo' :: PasswordInfo -> Bool
checkPasswordInfo' PasswordInfo {..} =
  let Range {..} = passwordInfoRange
      lowMatch   = passwordInfoPassword `BS.index` (rangeLow - 1)
      highMatch  = passwordInfoPassword `BS.index` (rangeHigh - 1)
  in  (lowMatch == passwordInfoChar) /= (highMatch == passwordInfoChar)
