module Day1 where

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Word
import Data.Primitive.ByteArray
import Common
import Data.Primitive.Types

day1 :: IO ()
day1 = do
  ba <- readFileBA "input/day1.dat"
  let sol1 = countBracketsBA ba
  let sol2 = firstNeg ba
  putStrLn $ unlines (solutions sol1 sol2)


solutions :: Int -> Int -> [String]
solutions s1 s2 =
  ["~~~ Day 1 ~~~"
  ,""
  ,"solution 1: " <> (show s1)
  ,"solution 2: " <> (show s2)
  ]

countBrackets :: ByteString -> Int
countBrackets = ByteString.foldl' addBS 0
  where
    addBS :: Int -> Word8 -> Int
    addBS acc 40 = acc + 1
    addBS acc 41 = acc - 1
    addBS acc _  = acc


countBracketsBA :: ByteArray -> Int
countBracketsBA = foldlBA add 0

add :: Int -> Word8 -> Int
add acc 40 = acc + 1
add acc 41 = acc - 1
add acc _  = acc


firstNeg :: ByteArray -> Int
firstNeg arr = go 0 0
  where
    go i acc
      | acc == -1 = i
      | i < maxI  = go (i + 1) (add acc (indexByteArray arr i)) 
      | otherwise = error "Not found"
    maxI = sizeofByteArray arr `quot` sizeOf (undefined :: Word8)


