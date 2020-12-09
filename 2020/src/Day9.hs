{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Day9 where

import qualified Data.ByteString.Char8 as ByteString
import Common (solutions, bsToInt)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

day9 :: IO ()
day9 = do
  inp <- parseInput
  let
    sol1, sol2 :: Int
    sol1 = s1 inp
    sol2 = s2 sol1 inp
  print $ bsToInt "-10000"
  solutions 9 sol1 sol2


day9' :: IO (Int, Int)
day9' = do
  inp <- parseInput
  let
    sol1, sol2 :: Int
    sol1 = s1 inp
    sol2 = s2 sol1 inp
  pure (sol1, sol2)


parseInput :: IO (Vector Int)
parseInput = do
  bs <- ByteString.readFile "input/day9.dat"
--  let bs = test
  let vs = Vector.fromList . fmap bsToInt . ByteString.lines $ bs
  pure $ vs

isValid :: Vector Int -> IntSet -> Int -> Bool
isValid slice window target = Vector.any hasSum slice
  where
  hasSum :: Int -> Bool
  hasSum w = (target - w) `IntSet.member` window

s1 :: Vector Int -> Int
s1 = part1 25

s2 :: Int -> Vector Int -> Int
s2 target vs = part2 vs target

part1 :: Int -> Vector Int -> Int
part1 windowSize vs = go (windowSize + 1) preamble
  where
    preamble = Vector.foldl' (flip IntSet.insert) mempty (Vector.take windowSize vs)
    go :: Int -> IntSet -> Int
    go !ind !window =
      let
        vs' = Vector.slice (ind - (windowSize + 1)) windowSize vs
        target = vs  Vector.! ind
        start  = vs' Vector.! 0
        window' = IntSet.insert target . IntSet.delete start $ window
      in
        if isValid vs' window target
          then go (ind + 1) window'
          else target

minMax :: Vector Int -> (Int, Int)
minMax = Vector.foldl' (\ ~(lo, hi) x -> (min lo x, max hi x)) (maxBound, minBound)

part2 :: Vector Int -> Int -> Int
part2 vs target = (\ ~(x, y) -> x + y) . minMax $ contiguous
  where
    contiguous = Vector.slice loInd (hiInd - loInd) vs
    (loInd, hiInd) = findBounds 0 0
    sums = Vector.scanl' (+) 0 vs
    findBounds loI hiI | hiLo < target  = findBounds loI (hiI + 1)
                       | hiLo > target  = findBounds (loI + 1) hiI
                       | otherwise      = (loI, hiI)
      where
        hi = sums Vector.! hiI
        lo = sums Vector.! loI
        hiLo = hi - lo
