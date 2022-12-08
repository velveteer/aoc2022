{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- |
-- Module      : Day2
-- Description : Day 2 Solutions

-- <https://adventofcode.com/2022/day/2>

module Day2 where

import           Control.Arrow
import qualified Data.List         as List
import qualified Data.Maybe        as Maybe
import           Data.Text         (Text)
import qualified Data.Text         as Text
import qualified Data.Text.IO      as Text

day2 :: IO ()
day2 = do
  Text.putStrLn "day 2"
  input <- Text.readFile "text/day2.txt"
  print $ day2a input
  print $ day2b input

-- | Solve Day 2 Part One
-- What would your total score be if everything goes exactly according to your
-- strategy guide?
-- >>> day2a <$> Text.readFile "text/day2.example.txt"
-- 15
day2a :: Text -> Int
day2a = compute tableA

-- | Solve Day 2 Part Two
-- Following the Elf's instructions for the second column, what would your
-- total score be if everything goes exactly according to your strategy guide?
-- >>> day2b <$> Text.readFile "text/day2.example.txt"
-- 12
day2b :: Text -> Int
day2b = compute tableB

type Table = [((Text, Text), Int)]

compute :: Table -> Text -> Int
compute table
  = Text.lines
  >>>
    fmap ((Text.words >>> \[x, y] -> (x, y))
    >>> ((`List.lookup` table) >>> Maybe.fromMaybe 0))
  >>> sum

tableA :: Table
tableA =
  [ (("B", "X"), 1)
  , (("C", "Y"), 2)
  , (("A", "Z"), 3)
  , (("A", "X"), 4)
  , (("B", "Y"), 5)
  , (("C", "Z"), 6)
  , (("C", "X"), 7)
  , (("A", "Y"), 8)
  , (("B", "Z"), 9)
  ]

tableB :: Table
tableB =
  [ (("B", "X"), 1)
  , (("C", "X"), 2)
  , (("A", "X"), 3)
  , (("A", "Y"), 4)
  , (("B", "Y"), 5)
  , (("C", "Y"), 6)
  , (("C", "Z"), 7)
  , (("A", "Z"), 8)
  , (("B", "Z"), 9)
  ]
