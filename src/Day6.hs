{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
-- Module      : Day6
-- Description : Day 6 Solutions

-- <https://adventofcode.com/2022/day/6>

module Day6 where

import           Data.Text       (Text)
import qualified Data.List       as List
import qualified Data.Text       as Text
import qualified Data.Text.IO    as Text

day6 :: IO ()
day6 = do
  Text.putStrLn "day 6"
  input <- Text.readFile "text/day6.txt"
  print $ day6a input
  print $ day6b input

-- | Solve Day 6 Part One
-- How many characters need to be processed before the first start-of-packet
-- marker is detected?
-- >>> day6a <$> Text.readFile "text/day6.example.txt"
-- 7
day6a :: Text -> Int
day6a = solve 4

-- | Solve Day 6 Part Two
-- How many characters need to be processed before the first start-of-message
-- marker is detected?
-- 19
day6b :: Text -> Int
day6b = solve 14

solve :: Int -> Text -> Int
solve c (Text.unpack -> signal) = go 0
  where
    go :: Int -> Int
    go !n
      | (==) <$> List.nub <*> id $ (take c . drop n $ signal)
      = n + c
    go !n
      = go (n + 1)
