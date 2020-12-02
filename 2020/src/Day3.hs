module Day3 where

import Common
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.Functor (void)

day2 :: IO ()
day2 = do
  bs <- ByteString.readFile "input/day2.dat"
  let
    input :: Either String [Int]
    input = traverse (parseOnly parser) (ByteString.lines bs)
    sol1, sol2 :: Either String Int
    sol1  = undefined <$> input
    sol2  = undefined <$> input
  putStrLn $ (solutions 2 sol1 sol2)    

parser :: Parser Int
parser = do
  undefined
