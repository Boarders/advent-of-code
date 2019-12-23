{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString  as ByteString hiding (unpack)
import qualified Data.ByteString.Char8 as ByteString
import Data.Set (Set)
import qualified Data.Set  as Set
import Data.Map (Map)
import qualified Data.Map  as Map
import Data.Key (foldMapWithKey)
import Data.Foldable
import Data.List.Extra (maximumOn)
import Control.Parallel.Strategies

import Debug.Trace

main :: IO ()
main =
  do
    inputText <- ByteString.readFile "input.txt"
    let asteroids = parseInput inputText
--    let testIn = parseInput test
    let puzzle1Sol = puzzle1 asteroids
    let stationPt = fst puzzle1Sol
    putStrLn "puzzle1 :"
    putStrLn $ (replicate 25 ' ') <> (show puzzle1Sol)
    putStrLn ""
    putStrLn "puzzle2 :"
    putStrLn ""
    putStrLn $ (replicate 25 ' ') <> (show $ puzzle2 stationPt asteroids)


puzzle1 :: [Point] -> (Point, Int)
puzzle1 =  maximumOn snd . asteroidCount

puzzle2 :: Point -> [Point] -> Int
puzzle2 stationPt = transform . asteroidsDestroyed stationPt 199
  where
    transform :: Point -> Int
    transform Point{..} = 100 * x + y

test :: ByteString
test =
  "###\n.#.\n..#"
  
recentre :: Point -> [Point] -> [Point]
recentre p = fmap (\q -> q - p)

asteroidsDestroyed :: Point -> Int -> [Point] -> Point
asteroidsDestroyed center nth points = destroyedOrder newPoints !! nth
  where
    targetMap :: [Point] -> Map Line (Map Int Point)
    targetMap = foldMap f . filter (/= Point 0 0)

    destroyedOrder :: [Point] -> [Point]
    destroyedOrder =
        fold
      . sequenceA
      . Map.elems
      . fmap (Map.elems)
      . targetMap

    f :: Point -> Map Line (Map Int Point)
    f p =
      let  -- to do: you don't want the line! you want to work out the angle from the vertical
        line = pointToLine p
        dist = manhattanNorm p
      in
        Map.singleton line (Map.singleton dist p)
      
      
    newPoints = recentre center points
    
    
    

-- |
-- This function takes a point p and another point v
-- and returns True iff the first point lies on the line 
liesOnLine :: Point -> Point -> Bool
liesOnLine p1 p2 = p1 .||. p2


asteroidCount :: [Point] -> [(Point, Int)]
asteroidCount points = parMap rpar (\p -> asteroidsInSight p points) points


tr :: (Show a) => String -> a -> a
tr s x = trace (s <> (show x)) x


asteroidsInSight :: Point -> [Point] -> (Point, Int)
asteroidsInSight focus asteroids =
    (focus, length $ groupFn asteroids)
  where
    groupFn :: [Point] -> Set Line
    groupFn = foldMap (Set.singleton . pointToLine . (subtract focus)) . filter (/= focus)


parseInput :: ByteString -> [Point]
parseInput =
      foldMapWithKey f
    . ByteString.lines
  where
    f :: Int -> ByteString -> [Point]
    f y bs =
      let
        chars :: [Char]
        chars = ByteString.unpack bs
      in
        foldMapWithKey (h y) chars

    h :: Int -> Int -> Char -> [Point]
    h y x = \case
       '#' -> [(Point x y)]
       _   -> mempty


pointToLine :: Point -> Line
pointToLine (Point 0 0) = error "Can't construct a line from the zero point!"
pointToLine (Point x y) =
  let
    d = gcd x y
  in
    Line (x `div` d) (y `div` d)



data Line = Line
  { lx :: {-# UNPACK #-} !Int
  , ly :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord)




data Point = Point
  { x :: {-# UNPACK #-} !Int
  , y :: {-# UNPACK #-} !Int
  }
  deriving (Eq)

instance Num Point where
  (+) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (-) (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)
  negate (Point x y) = (Point (- x) (- y))
  (*) = error "undefined"
  abs = error "undefined"
  signum = error "undefined"
  fromInteger = error "undefined"

manhattanNorm :: Point -> Int
manhattanNorm Point{..} = abs x + abs y


(.||.) :: Point -> Point -> Bool
(Point 0 0) .||. _  = True
_ .||. (Point 0 0)  = True
(Point x1 y1) .||. (Point x2 y2) =
  let
    d1 = gcd x1 y1
    d2 = gcd x2 y2
    p1 = (Point (x1 `div` d1) (y1 `div` d1))
    p2 = (Point (x2 `div` d2) (y2 `div` d2))
  in
    p1 == p2
 
    

instance Ord Point where
  (Point x0 y0) <= (Point x1 y1)
    = y0 <= y1 || (y0 == y1 && x0 <= x1)

instance Show Point where
  show (Point x y) = fold ["<x=", show x, ",y=", show y, ">"]
