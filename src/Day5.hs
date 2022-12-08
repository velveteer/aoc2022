{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- |
-- Module      : Day5
-- Description : Day 5 Solutions

-- <https://adventofcode.com/2022/day/5>

module Day5 where

import           Control.Arrow
import           Data.Char       (isAlpha)
import           Data.Function   ((&))
import qualified Data.List       as List
import qualified Data.List.Split as List
import qualified Data.IntMap.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as Text
import qualified Data.Text.IO    as Text

day5 :: IO ()
day5 = do
  Text.putStrLn "day 5"
  input <- Text.readFile "text/day5.txt"
  print $ day5a input
  print $ day5b input

-- | Solve Day 5 Part One
-- After the rearrangement procedure completes, what crate ends up on top of
-- each stack?
-- >>> day5a <$> Text.readFile "text/day5.example.txt"
-- "CMZ"
day5a :: Text -> Text
day5a = solve reverse

-- | Solve Day 5 Part Two
-- After the rearrangement procedure completes, what crate ends up on top of
-- each stack? (But now multiple crates retain ordering when moved)
-- "MCD"
day5b :: Text -> Text
day5b = solve id

solve :: ([Text] -> [Text]) -> Text -> Text
solve rev
  = Text.lines
  >>> List.splitOn [""]
  >>> \[init -> crates, concatMap Text.lines -> proc]
      -> crates
       & fmap (fmap (Text.filter isAlpha) . Text.chunksOf 4)
       & List.transpose
       & Map.fromAscList . zip [1..]
       & fmap (List.dropWhile (== ""))
       & moveCrates rev proc
       & foldMap head

parseMove :: [Text] -> (Int, Int, Int)
parseMove
  [ _, read . Text.unpack -> amt
  , _, read . Text.unpack -> from
  , _, read . Text.unpack -> to
  ] = (amt, from, to)

moveCrates :: ([Text] -> [Text]) -> [Text] -> Map.IntMap [Text] -> Map.IntMap [Text]
moveCrates rev (fmap (parseMove . Text.words) -> moves) stacks
  = go moves stacks
  where
    updateFrom amt _key = Just . List.drop amt
    go [] s = s
    go ((amt,from,to):ms) s =
      let (Just oldFrom, newMap)
            = Map.updateLookupWithKey (updateFrom amt) from s
       in newMap
            & Map.adjust (rev (take amt oldFrom) <>) to
            & go ms
