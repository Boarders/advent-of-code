{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module Day4 where

import Common
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Data.List(foldl')
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)


day4 :: IO ()
day4 = do
  input <- parseInput
  let
    sol1  = s1 input
    sol2  = s2 input

  putStrLn $ (solutions 4 sol1 sol2)

parseInput :: IO [HashMap Text Text]
parseInput = do
  txt <- Text.readFile "input/day4.dat"
  let
    processEntry =
          foldl' (\acc ~(k,v) -> HashMap.insert k v acc) mempty
        . fmap (breakDrop (== ':'))
        . Text.words
    entries =
        fmap processEntry
      . Text.splitOn "\n\n"
      $ txt
  pure $ entries


validate1 :: HashMap Text Text -> Bool
validate1 hm =
  let l = length hm in
  l == 8 || (l == 7 && (not $ HashMap.member "cid" hm))

s1 :: [HashMap Text Text] -> Int
s1 = length . filter validate1

s2 :: [HashMap Text Text] -> Int
s2 = length . mapMaybe parseInput2 . filter validate1

parseInput2 :: HashMap Text Text -> Maybe PassInfo
parseInput2 hm = do
  byrRaw <- HashMap.lookup "byr" hm
  byr <- readInt byrRaw
  iyrRaw <- HashMap.lookup "iyr" hm
  iyr <- readInt iyrRaw
  eyrRaw <- HashMap.lookup "eyr" hm
  eyr <- readInt eyrRaw
  hgtRaw <- HashMap.lookup "hgt" hm
  hgt <- readIntR hgtRaw
  hclRaw <- HashMap.lookup "hcl" hm
  let hcl = Text.takeWhile (not . isSpace) hclRaw
  eclRaw <- HashMap.lookup "ecl" hm
  let ecl = Text.takeWhile (not . isSpace) eclRaw
  pidRaw <- HashMap.lookup "pid" hm
  let pid = Text.takeWhile (not . isSpace) pidRaw
  passInfo byr iyr eyr hgt hcl ecl pid


data PassInfo = PassInfo
  { byrPI :: Int
  , iyrPI :: Int
  , eyrPI :: Int
  , hgtPI :: (Int, Text)
  , hclPI :: Text
  , eclPI :: Text
  , pidPI :: Text
  }
  deriving Show

between :: Int -> Int -> Int -> Bool
between l h v = l <= v && v <= h

passInfo ::
  Int -> Int -> Int -> (Int, Text) -> Text -> Text -> Text -> Maybe PassInfo
passInfo byr iyr eyr hgt hcl ecl pid = do
  guard (between 1920 2002 byr)
  guard (between 2010 2020 iyr)
  guard (between 2010 2030 eyr)
  guard (hgtRule hgt)
  guard (hclRule hcl)
  guard (ecl `HashSet.member` eyeColours)
  guard ((Text.length pid) == 9)
  pure (PassInfo byr iyr eyr hgt hcl ecl pid)

hgtRule :: (Int, Text) -> Bool
hgtRule (hgt, unit) | unit == "cm" = 150 <= hgt && hgt <= 193
                    | unit == "in" = 59 <= hgt && hgt <= 76
                    | otherwise = False

hclRule :: Text -> Bool
hclRule (Text.uncons -> Just ('#', rest)) = Text.all ((||) <$> p1 <*> p2) rest
  where
    p1 c = '0' <= c && c <= '9'
    p2 c = 'a' <= c && c <= 'f'
hclRule _ = False

eyeColours :: HashSet.HashSet Text
eyeColours = HashSet.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
