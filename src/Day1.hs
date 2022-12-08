{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Day1
-- Description : Day 1 Solutions

-- <https://adventofcode.com/2022/day/1>

module Day1 where

import           Control.Arrow
import qualified Data.List         as List
import qualified Data.List.Split   as List
import qualified Data.Maybe        as Maybe
import qualified Data.Ord          as Ord
import           Data.Text         (Text)
import qualified Data.Text         as Text
import qualified Data.Text.IO      as Text
import qualified Text.Read         as Text

day1 :: IO ()
day1 = do
  Text.putStrLn "day 1"
  input <- Text.readFile "text/day1.txt"
  print $ day1a input
  print $ day1b input

-- | Solve Day 1 Part One
-- Find the Elf carrying the most Calories. How many total Calories is that Elf
-- carrying?
-- >>> day1a <$> Text.readFile "text/day1.example.txt"
-- 24000
day1a :: Text -> Int
day1a = sortedSums >>> head

-- | Solve Day 1 Part Two
-- Find the top three Elves carrying the most Calories. How many Calories are
-- those Elves carrying in total?
-- >>> day1b <$> Text.readFile "text/day1.example.txt"
-- 45000
day1b :: Text -> Int
day1b = sortedSums >>> take 3 >>> sum

sortedSums :: Text -> [Int]
sortedSums
  = Text.lines
  >>> fmap (Maybe.fromMaybe 0 . Text.readMaybe . Text.unpack)
  >>> List.splitOn [0]
  >>> fmap sum
  >>> List.sortOn Ord.Down
