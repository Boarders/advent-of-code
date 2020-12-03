module Day3 where

import Common
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString, index)
import Data.List(foldl')


day3 :: IO ()
day3 = do
  input <- parseInput
  let
    sol1  = s1 input
    sol2  = s2 input
  putStrLn $ (solutions 2 sol1 sol2)

parseInput :: IO [ByteString]
parseInput = do
  bs <- ByteString.readFile "input/day3.dat"
  pure $ ByteString.lines bs


data Step = Step
  { rowLenS :: !Int
  , stepS   :: !Int
  , startS  :: !Int
  }

findInd :: Step -> Int -> Int
findInd (Step rowLen step start) row =
  start + ((row - 1) * step `mod` rowLen) - 1


countTrees :: Int -> Int -> [ByteString] -> Int
countTrees step start bss = snd . foldl' c (1, 0) $ bss
  where
    rowLen = ByteString.length . head $ bss
    st = Step rowLen step start
    
    c :: (Int, Int) -> ByteString -> (Int, Int)
    c (row, count) bs =
       (row + 1, count + (fromEnum (bs `index` findInd st row == '#')))

countTrees2 :: Int -> Int -> [ByteString] -> Int
countTrees2 step start bss = (\(_,_,cnt) -> cnt) . foldl' c (True, 1, 0) $ bss
  where
    rowLen = ByteString.length . head $ bss
    st = Step rowLen step start
    
    c :: (Bool, Int, Int) -> ByteString -> (Bool, Int, Int)
    c (alt, row, count) bs =
      if alt
        then
          (not alt, row + 1 , count + (fromEnum (bs `index` findInd st row == '#')))
        else
          (not alt, row, count)

s1 :: [ByteString] -> Int
s1 = countTrees 3 1

s2 :: [ByteString] -> Int
s2 input =
  (product . map (\i -> countTrees i 1 input) $ [1,3,5,7]) * countTrees2 1 1 input
