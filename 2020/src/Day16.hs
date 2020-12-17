{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
module Day16 where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Common (solutions, bsToInt, showBitsI)
import Data.Foldable (for_, traverse_)
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed (Vector)
import qualified Data.ByteString.Search as Search (split)
import qualified Data.IntervalMap.FingerTree as IntervalMap
import Data.IntervalMap.FingerTree (IntervalMap, Interval)
import Data.List (foldl', foldl1')
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Data.Vector.Unboxed.Mutable (MVector)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector as Boxed
import Debug.Trace
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Bifunctor (second)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Control.DeepSeq
import GHC.Generics (Generic)


data Range = Range
  { low  :: !Int
  , high :: !Int
  } deriving Generic

instance NFData Range

derivingUnbox "Range"
    [t|  Range -> (Int, Int)  |]
    [| \(Range l h) -> (l, h) |]
    [| \(l, h) -> Range l h   |]

day16 :: IO ()
day16 = do
  inp <- parseInput  
  let
    sol1, sol2 :: Int
    sol1 = s1 inp
    sol2 = s2 inp
  solutions 16 sol1 sol2


day16' :: IO (Int, Int)
day16' = do
  inp <- parseInput
  pure (s1 inp, s2 inp)


parseInput :: IO (Vector (Int, Range), Vector Int, [Vector Int])
parseInput = do
  bs <- ByteString.readFile "input/day16.dat"
  let res@[rangesBS, ticketBS, ticketsBS] = Search.split "\n\n" bs
  let ticket = parseTicket . (!! 1) .ByteString.lines $ ticketBS
  let tickets = fmap parseTicket . tail . ByteString.lines $ ticketsBS
  let
    ranges :: Vector (Int, Range)
    ranges =
        Vector.fromList .concatMap parseRange . zip [0..] . ByteString.lines $ rangesBS
  let 
  pure (ranges, ticket, tickets)


parseRange :: (Int, ByteString) -> [(Int, Range)]
parseRange (n, bs) =
  let
    [name, ranges] = ByteString.split ':' bs
    [int1, int2]   = Search.split " or " (ByteString.tail ranges)
    [lower1, upper1] = ByteString.split  '-' int1
    [lower2, upper2] = ByteString.split  '-' int2
    interval1 = Range (bsToInt lower1) (bsToInt upper1)
    interval2 = Range (bsToInt lower2) (bsToInt upper2)
    
  in [(n , interval1), (n, interval2)]

parseTicket :: ByteString -> Vector Int
parseTicket = Vector.fromList . fmap bsToInt . ByteString.split ',' 


getSolution :: Vector Int -> Vector (Int, Int)
getSolution v = runST $ do
  !mv <- Vector.unsafeThaw (Vector.indexed v)
  sortBy (\ n m -> compare (popCount . snd $ n) (popCount . snd $ m)) mv
  go 0 0 mv
  for_ [0..(len - 1)] $ \i ->
    Mutable.modify mv (\(n, s) -> (n , countTrailingZeros s)) i
  v <- Vector.unsafeFreeze mv   
  pure v
  where
    len = Vector.length v
    go :: Int -> Int -> MVector s (Int, Int) -> ST s ()
    go !ind !acc !mv
      | ind == len = pure ()
      | otherwise = do
          !possible <- fmap snd (Mutable.read mv ind)
          let !sol  = possible `xor` acc
          let !acc' = acc .|. sol
          Mutable.modify mv (second (const sol)) ind
          go (ind + 1) acc' mv


inRange :: Int -> (Int, Range) -> Bool
inRange n (_, (Range l h)) = l <= n && n <= h

inRanges :: Int -> Vector (Int, Range) -> Bool
inRanges n = Vector.any (inRange n)

{-# inline validRanges #-}
validRanges :: Int -> Vector (Int, Range) -> Vector Int
validRanges n = Vector.map fst . Vector.filter (inRange n)

s1 :: (Vector (Int, Range), Vector Int, [Vector Int]) -> Int
s1 (ranges, _, tickets) =
     (Vector.foldl' countInvalid 0) $ Vector.concat tickets    
  where
    countInvalid :: Int -> Int -> Int
    countInvalid !acc !n =
      if inRanges n ranges then acc else acc + n

s2 :: (Vector (Int, Range), Vector Int, [Vector Int]) -> Int
s2 (ranges, ticket, tickets) =
      Vector.foldl' (\acc i -> acc * ticket Vector.! (fst i)) 1
    . Vector.filter (\x -> snd x <= 5)
    . getSolution
    . foldl1' (Vector.zipWith (.&.))
    . fmap (setRow . Vector.indexed)
    . filter valid
    $ tickets
  where
    valid :: Vector Int -> Bool
    valid = Vector.all (\ !n -> inRanges n ranges)
    
    getBits :: (Int, Int) -> Int
    getBits (_, !val) =
      Vector.foldl' (\acc n ->  setBit acc n) 0 (validRanges val ranges)

    setRow :: Vector (Int, Int) -> Vector Int
    setRow = Vector.map getBits
