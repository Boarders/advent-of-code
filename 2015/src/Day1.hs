module Day1 where

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Word
import Data.Primitive.ByteArray
import Common

day1 :: IO ()
day1 = do
--  ByteString.writeFile "test" test
  ba <- readFileBA "test"
  print $ countBracketsBA ba
  putStrLn "solution 1: "


n :: Int
n = 10000000

test :: ByteString
test = ByteString.replicate n 40 <> ByteString.replicate n 41

countBrackets :: ByteString -> Int
countBrackets = ByteString.foldl' add 0
  where
    add :: Int -> Word8 -> Int
    add acc 40 = acc + 1
    add acc 41 = acc - 1
    add acc _  = acc



countBracketsBA :: ByteArray -> Int
countBracketsBA = foldlBA add 0
  where
    add :: Int -> Word8 -> Int
    add acc 40 = acc + 1
    add acc 41 = acc - 1
    add acc _  = acc




