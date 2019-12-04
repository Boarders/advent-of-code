module Main where

import Data.List (dropWhile)

main :: IO ()
main =
  do
    let range = fmap show ([254032 .. 789860] :: [Int])
    print $ puzzle1 range
    print $ puzzle2 range

equalAdjacent :: String -> Bool
equalAdjacent (x:y:ys) = (x == y) || (equalAdjacent (y:ys))
equalAdjacent _ = False

twoEqualAdjacent :: String -> Bool
twoEqualAdjacent (x0 : x1 : x2 : xs) =
  if (x0 == x1) then x1 /= x2 || twoEqualAdjacent (dropWhile (== x0) xs)
  else twoEqualAdjacent (x1 : x2 : xs)
twoEqualAdjacent (x : y : []) = x == y
twoEqualAdjacent _ = False

increasing :: String -> Bool
increasing (x:y:ys) = (x <= y) && increasing (y:ys)
increasing _ = True

criteria1 :: String -> Bool
criteria1 = (&&) <$> increasing <*> equalAdjacent

criteria2 :: String -> Bool
criteria2 = (&&) <$> increasing <*> twoEqualAdjacent

puzzle1 :: [String] -> Int
puzzle1 = length . filter criteria1

puzzle2 :: [String] -> Int
puzzle2 = length . filter criteria2
