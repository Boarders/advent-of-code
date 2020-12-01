module Day1 where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Common (solutions)
import Data.Maybe (fromJust)
import qualified Data.IntSet as IntSet

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


s1 :: [Int] -> Int
s1 xs = head [ x * (2020 - x) | x <- xs, (2020 - x) `IntSet.member` hs]
  where
    hs :: IntSet.IntSet
    hs = IntSet.fromList xs

s2 :: [Int] -> Int
s2 xs = head [ x * y * (2020 - x - y) | x <- xs, y <- tail xs, (2020 - x - y) `IntSet.member` hs]
  where
    hs :: IntSet.IntSet
    hs = IntSet.fromList xs
