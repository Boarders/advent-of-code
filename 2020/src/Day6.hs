{-# language OverloadedStrings #-}
module Day6 where

import Common
import qualified Data.ByteString.Char8 as ByteString hiding (split)
import Data.ByteString.Char8 (ByteString)
import Data.List(foldl')
import Data.Bits
import Data.Char (ord)
import qualified Data.ByteString.Search as ByteString (split)

day6 :: IO ()
day6 = do
  input <- parseInput
  let
    sol1  = s1 input
    sol2  = s2 input
  solutions 6 sol1 sol2


day6' :: IO (Int, Int)
day6' = do
  input <- parseInput
  let
    sol1  = s1 input
    sol2  = s2 input
  pure (sol1, sol2)


parseInput :: IO [[ByteString]]
parseInput = do
  bs <- ByteString.readFile "input/day6.dat"
  let bs' = ByteString.split "\n\n" bs
  pure (fmap ByteString.lines bs')

s1 :: [[ByteString]] -> Int
s1 bss =
    foldl' (+) 0
  . fmap unionAnswers
  $ bss

s2 :: [[ByteString]] -> Int
s2 bss  =
    foldl' (+) 0
  . fmap intersectAnswers
  $ bss

intersectAnswers :: [ByteString] -> Int
intersectAnswers = popCount . foldl' (\acc bs -> acc .&. countAnswers bs) maxBound

unionAnswers :: [ByteString] -> Int
unionAnswers =
  popCount . foldl' (\acc bs -> acc .|. countAnswers bs) 0

countAnswers :: ByteString -> Int
countAnswers bs = ByteString.foldl' acc 0 bs
  where
    acc :: Int -> Char -> Int
    acc n c = n .|. bit (ord c - 97)
