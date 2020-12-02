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
  bs <- ByteString.readFile "input/day2.dat"
  let
    input :: Either String [PassInfo]
    input = traverse (parseOnly parsePassInfo) (ByteString.lines bs)
    sol1  = length . filter s1 <$> input
    sol2  = length . filter s2 <$> input
  putStrLn $ (solutions 2 sol1 sol2)

d2 = do
  bs <- ByteString.readFile "input/day2.dat"
  let
    input = traverse (parseOnly parsePassInfo) (ByteString.lines bs)
  pure input
      

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

s1 :: PassInfo -> Bool
s1 (PassInfo (Range l h) tgt pass) =
  let c = countLetter pass tgt in
    (l <= c && c <= h)

s2 :: PassInfo -> Bool
s2 (PassInfo range tgt pass) = occursOnce pass range tgt

countLetter :: ByteString -> Char -> Int
countLetter bs tgt = ByteString.foldl occurs 0 bs
  where
    occurs :: Int -> Char -> Int
    occurs acc c | c == tgt = acc + 1
                 | otherwise   = acc


occursOnce :: ByteString -> Range -> Char -> Bool
occursOnce bs (Range l' h') c =
  bs `ByteString.index` l == c && bs `ByteString.index` h /= c ||
  bs `ByteString.index` l /= c && bs `ByteString.index` h == c
  where
    l = l' - 1
    h = h' - 1
