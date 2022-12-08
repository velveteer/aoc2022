{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- |
-- Module      : Day4
-- Description : Day 4 Solutions

-- <https://adventofcode.com/2022/day/4>

module Day4 where

import           Control.Arrow
import           Data.Monoid (Sum (..))
import           Data.Text         (Text)
import qualified Data.Text         as Text
import qualified Data.Text.IO      as Text

day4 :: IO ()
day4 = do
  Text.putStrLn "day 4"
  input <- Text.readFile "text/day4.txt"
  print $ day4a input
  print $ day4b input

-- | Solve Day 4 Part One
-- In how many assignment pairs does one range fully contain the other?
-- >>> day4a <$> Text.readFile "text/day4.example.txt"
-- 2
day4a :: Text -> Int
day4a = solve (\a b c d -> (a >= c && b <= d) || (a <= c && b >= d))

-- | Solve Day 4 Part Two
-- In how many assignment pairs do the ranges overlap?
-- >>> day4b <$> Text.readFile "text/day4.example.txt"
-- 4
day4b :: Text -> Int
day4b = solve (\a b c d -> max a c <= min b d)

solve :: (Int -> Int -> Int -> Int -> Bool) -> Text -> Int
solve check
  = Text.lines
  >>>
    foldMap
      (\(Text.splitOn "," -> [parseRange -> (a, b), parseRange -> (c, d)])
      -> Sum . fromEnum $ check a b c d)
  >>> getSum

parseRange :: Text -> (Int, Int)
parseRange
  (Text.splitOn "-" ->
  [read . Text.unpack -> x, read . Text.unpack -> y])
  = (x, y)
