{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
module Day1 where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Common (solutions)
import Data.Maybe (fromJust)

day1 :: IO ()
day1 = do
  bs <- ByteString.readFile "input/day1.dat"
  let
    input = parseInput bs
    sol1 = sum_2020 input
    sol2 = sum3_2020 input
  putStrLn $ (solutions 1 sol1 sol2)

parseInput :: ByteString -> [Int]
parseInput = fmap (fst . fromJust . ByteString.readInt) . ByteString.lines


sum_2020 :: [Int] -> Int
sum_2020 xs = head [ x * y | x <- xs, y <- tail xs, x + y == 2020]
  

sum3_2020 :: [Int] -> Int
sum3_2020 xs = head [ x * y * z | x <- xs, y <- tail xs, z <- tail (tail xs), x + y + z == 2020]
