module Day5 where

import Common
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.List(foldl')
import Data.Bits
import Data.Bifunctor (bimap)

day5 :: IO ()
day5 = do
  input <- parseInput
  let
    sol1  = s1 input
    sol2  = s2 input
  putStrLn $ (solutions 5 sol1 sol2)

parseInput :: IO [ByteString]
parseInput = do
  bs <- ByteString.readFile "input/day5.dat"
  pure $ ByteString.lines bs

processStart :: ByteString -> Int
processStart bs = ByteString.foldl' acc 0 bs
  where
    acc :: Int -> Char -> Int
    acc n c = (n `shiftL` 1) `xor` (fromEnum (c == 'B'))

processEnd :: ByteString -> Int
processEnd bs = ByteString.foldl' acc 0 bs
  where
    acc :: Int -> Char -> Int
    acc n c = (n `shiftL` 1) `xor` (fromEnum (c == 'R'))

s1 :: [ByteString] -> Int
s1 =
    foldl' max 0
  . fmap (\ ~(s,e) -> 8 * s + e)
  . fmap (bimap processStart processEnd)
  . fmap (ByteString.splitAt 7)

s2 :: [ByteString] -> Int
s2 bs  = tot - summed
  where
  tot = (hi * (hi + 1) + lo * (1 - lo)) `shiftR` 1
  (hi, lo, summed) =
      maxMinSum
    . fmap (\ ~(s,e) -> 8 * s + e)
    . fmap (bimap processStart processEnd)
    . fmap (ByteString.splitAt 7)
    $ bs

maxMinSum :: [Int] -> (Int, Int, Int)
maxMinSum = foldl' go (0, maxBound, 0)
  where
    go (hi, lo, tot) x = (max hi x, min lo x, tot + x)
