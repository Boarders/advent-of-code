{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
module Day2 where

import Common
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as ByteString

day2 :: IO ()
day2 = do
  bs <- ByteString.readFile "input/day2.dat"
  let
    input = traverse (parseOnly parser) (ByteString.lines bs)
    sol1  = const 5 input
    sol2  = const 5 input
  putStrLn $ (solutions 1 sol1 sol2)    
   


parser ::  Parser Int
parser = undefined


