{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language BangPatterns #-}

module Day10 where

import qualified Data.ByteString.Char8 as ByteString
import Common (solutions, bsToInt)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Algorithms.Radix (sort)
import Control.Monad.ST

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


day10 :: IO ()
day10 = do
  inp <- parseInput
  let
    sol1 = s1 inp
    sol2 = s2 inp
  solutions 10 sol1 sol2

day10' :: IO (Int, Int)
day10' = do
  inp <- parseInput
  let
    sol1 = s1 inp
    sol2 = s2 inp
  pure (sol1, sol2)


parseInput :: IO (Vector Int)
parseInput = do
  bs <- ByteString.readFile "input/day10.dat"
  let vs = Vector.fromList . fmap bsToInt . ByteString.lines $ bs
  pure $ Vector.create $ do
    mv <- Vector.unsafeThaw vs
    sort mv
    pure mv

sortInPlace :: forall s . Vector Int -> ST s (MVector s Int)
sortInPlace vs = do
  mv <- Vector.unsafeThaw vs
  sort mv
  pure mv

data State = State
  { lastEntry :: !Int
  , oneJolt   :: !Int
  , threeJolt :: !Int 
  }
  deriving Show

multJolt :: State -> Int
multJolt (State _ oneJ threeJ) = oneJ * threeJ

s1 :: Vector Int -> Int
s1 v =
  let
    h  = Vector.head v
    v' = Vector.tail v
    start = State h 1 1
  in
    multJolt . Vector.foldl' count start $ v'
  where
    count :: State -> Int -> State
    count (State lastEnt oneJ threeJ) n =
        case n - lastEnt of
          1 -> State n (oneJ + 1) (threeJ)
          3 -> State n oneJ       (threeJ + 1)
          _ -> State n oneJ       threeJ


buildGraph :: Vector Int -> IntMap (Vector Int)
buildGraph v = Vector.ifoldl' addVert startMap v
  where
    startMap = IntMap.insert highest mempty mempty
    highest  = (Vector.last v + 3)
    len = Vector.length v
    addVert :: IntMap (Vector Int) -> Int -> Int -> IntMap (Vector Int)
    addVert acc ind val =
      let
        entries =
            Vector.filter (<= val + 3)
          . Vector.slice (ind + 1) (min (len - (ind + 1)) 3)
          $ v
      in
        IntMap.insert val entries acc


paths :: Vector Int -> IntMap (Vector Int) -> Int
paths v g =
      Vector.foldl' (\acc x -> acc + (intMap IntMap.! x)) 0
    . Vector.filter (<= 3)
    . Vector.take 3
    $ v
  where
    !highest = Vector.last v + 3
    !intMap  = IntMap.fromSet pathsFromNode (IntMap.keysSet g)
    pathsFromNode :: Int -> Int
    pathsFromNode !ind | highest - ind <= 3 = 1
          | otherwise =
              Vector.foldl' (\acc x -> acc + (intMap IntMap.! x)) 0 (g IntMap.! ind)
    
s2 :: Vector Int -> Int
s2 v = paths v (buildGraph v)


{-
test = ByteString.unlines $
  [ "16"
  , "10"
  , "15"
  , "5"
  , "1"
  , "11"
  , "7"
  , "19"
  , "6"
  , "12"
  , "4"
  ]

test2 =
  ByteString.unlines
  [ "28"
  , "33"
  , "18"
  , "42"
  , "31"
  , "14"
  , "46"
  , "20"
  , "48"
  , "47"
  , "24"
  , "23"
  , "49"
  , "45"
  , "19"
  , "38"
  , "39"
  , "11"
  , "1"
  , "32"
  , "25"
  , "35"
  , "8"
  , "17"
  , "7"
  , "9"
  , "4"
  , "2"
  , "34"
  , "10"
  , "3"
  ]
--}
