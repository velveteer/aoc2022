{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- |
-- Module      : Day3
-- Description : Day 3 Solutions

-- <https://adventofcode.com/2022/day/3>

module Day3 where

import           Control.Arrow
import qualified Data.List         as List
import qualified Data.List.Split   as List
import qualified Data.Maybe        as Maybe
import           Data.Text         (Text)
import qualified Data.Text         as Text
import qualified Data.Text.IO      as Text
import           Text.RawString.QQ

day3 :: IO ()
day3 = do
  Text.putStrLn "day 3"
  input <- Text.readFile "text/day3.txt"
  print $ day3a input
  print $ day3b input

day3Example :: Text
day3Example = [r|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
|]

-- | Solve Day 3 Part One
-- Find the item type that appears in both compartments of each rucksack. What
-- is the sum of the priorities of those item types?
-- >>> day3a day3Example
-- 157
day3a :: Text -> Int
day3a
  = Text.lines
  >>>
    fmap (((Text.length >>> (`div` 2)) &&& id)
    >>> uncurry Text.splitAt
    >>> (\(a, b) -> List.intersect (Text.unpack a) (Text.unpack b))
    >>> head
    >>> (`List.lookup` charScoreTable)
    >>> Maybe.fromMaybe 0
    )
  >>> sum

-- | Solve Day 3 Part Two
-- Find the item type that corresponds to the badges of each three-Elf group.
-- What is the sum of the priorities of those item types?
-- >>> day3b day3Example
-- 70
day3b :: Text -> Int
day3b
  = Text.lines
  >>> fmap Text.unpack
  >>> List.chunksOf 3
  >>>
    fmap ((List.foldl1 List.intersect)
    >>> head
    >>> (`List.lookup` charScoreTable)
    >>> Maybe.fromMaybe 0)
  >>> sum

charScoreTable :: [(Char, Int)]
charScoreTable =
  zip ['a'..'z'] [1..26] <>
  zip ['A'..'Z'] [27..52]
