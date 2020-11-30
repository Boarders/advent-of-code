{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
module Day3 where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Common (solutions, altFoldl')


day3 :: IO ()
day3 = do
  bs <- ByteString.readFile "input/day3.dat"
  let
    sol1 = length (runGrid1 bs)
    sol2 = (totalVisited . runGrid2) bs
  putStrLn $ (solutions 2 sol1 sol2)


data Point = Point
  { xCoord :: !Int
  , yCoord :: !Int
  } deriving (Show, Eq, Generic)

instance Hashable Point

type Grid = HashSet Point
data State = State
  { pos  :: !Point
  , grid :: !Grid
  }

initialState :: State
initialState = State origin (HashSet.singleton origin)
  where
    origin = Point 0 0

addToGrid :: State -> Char -> State
addToGrid (State (Point x y) g) = \case
  '>' -> let newPos = Point (x + 1) y in State newPos (HashSet.insert newPos g)
  '<' -> let newPos = Point (x - 1) y in State newPos (HashSet.insert newPos g)
  '^' -> let newPos = Point x (y + 1) in State newPos (HashSet.insert newPos g)
  'v' -> let newPos = Point x (y - 1) in State newPos (HashSet.insert newPos g)
  c   -> error $ "unexpected input: " <> [c]

runGrid1 :: ByteString -> Grid
runGrid1 = grid . (ByteString.foldl' addToGrid initialState)

runGrid2 :: ByteString -> (Grid, Grid)
runGrid2 = (\ ~(s1, s2) -> (grid s1, grid s2)) . (altFoldl' addToGrid initialState)

totalVisited :: (Grid, Grid) -> Int
totalVisited (g1, g2) = length (HashSet.union g1 g2)


