{-# LANGUAGE DeriveGeneric #-}
module Day2 where

import Common
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.Functor (void)
import Control.DeepSeq
import GHC.Generics


day2 :: IO ()
day2 = do
  input <- parseInput
  let
    sol1  = s1 input
    sol2  = s2 input
  solutions 2 sol1 sol2


day2' :: IO (Int, Int)
day2' = do
  input <- parseInput
  let
    sol1  = s1 input
    sol2  = s2 input
  pure (sol1, sol2)


parseInput :: IO [PassInfo]
parseInput = do
  bs <- ByteString.readFile "input/day2.dat"
  let
    input = traverse (parseOnly parsePassInfo) (ByteString.lines bs)
  case input of
    Left err -> error err
    Right ps -> pure ps


data Range = Range
  { low  :: !Int
  , high :: !Int
  }
  deriving (Generic)

instance NFData Range

data PassInfo = PassInfo
  { passRange :: !Range
  , target    :: !Char
  , password  :: !ByteString
  } deriving (Generic)

instance NFData PassInfo

parsePassInfo :: Parser PassInfo
parsePassInfo = do
  l <- decimal
  void $ char '-'
  h <- decimal
  void $ space
  tgt <- anyChar
  void $ char ':'
  void $ space
  pass <- takeByteString
  pure (PassInfo (Range l h) tgt pass)

s1 :: [PassInfo] -> Int
s1 = length . filter valid1

valid1 :: PassInfo -> Bool
valid1 (PassInfo (Range l h) tgt pass) =

  let c = countLetter pass tgt in
    (l <= c && c <= h)

s2 :: [PassInfo] -> Int
s2 = length . filter valid2

valid2 :: PassInfo -> Bool
valid2 (PassInfo range tgt pass) = occursOnce pass range tgt

countLetter :: ByteString -> Char -> Int
countLetter bs tgt = ByteString.foldl occurs 0 bs
  where
    occurs :: Int -> Char -> Int
    occurs acc c | c == tgt  = acc + 1
                 | otherwise = acc


occursOnce :: ByteString -> Range -> Char -> Bool
occursOnce bs (Range l' h') c =
  bs `ByteString.index` l == c && bs `ByteString.index` h /= c ||
  bs `ByteString.index` l /= c && bs `ByteString.index` h == c
  where
    l = l' - 1
    h = h' - 1
