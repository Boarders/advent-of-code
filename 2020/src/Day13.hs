{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeApplications #-}

module Day13 where

import qualified Data.ByteString.Char8 as ByteString
import Common (solutions, bsToInt)
import Data.Maybe (fromJust)
import Data.List (foldl')
import Math.NumberTheory.Moduli.Chinese (chinese )
import Data.Bifunctor (second, bimap)

day13 :: IO ()
day13 = do
  inp <- parseInput
  let
    sol1 = s1 inp
    sol2 = s2 inp
  solutions 10 sol1 sol2

day13' :: IO (Int, Maybe Integer)
day13' = do
  inp <- parseInput
  let
    sol1 = s1 inp
    sol2 = s2 inp
  pure (sol1, sol2)

parseInput :: IO (Int, [(Int, Int)])
parseInput = do
  bs <- ByteString.readFile "input/day13.dat"
  let nlInd = fromJust . ByteString.elemIndex '\n' $ bs
  let (bs1, bs2) = ByteString.splitAt nlInd bs
  let ints =
          fmap (second bsToInt)
        . filter (\x -> snd x /= "x")
        . zip [0, -1 ..]
        . ByteString.split ','
        . ByteString.tail $ bs2
  pure (bsToInt bs1, ints)

s1 :: (Int, [(Int, Int)]) -> Int
s1 (targ, ints) =
    uncurry (*)
  . foldl' (\ acc p -> if fst p < fst acc then p else acc) maxBound
  . fmap (\x -> (x - targ `mod` x , x))
  . fmap snd
  $ ints

s2 :: (Int, [(Int, Int)]) -> Maybe Integer
s2 inp =
    fmap (\x -> if x < 0 then x + prod else x)
  . chineseRemainder
  . fmap (bimap (fromIntegral @Int @Integer) (fromIntegral @Int @Integer))
  . snd
  $ inp
  where
    !prod = fromIntegral . foldl' (*) 1 . fmap snd . snd $ inp

chineseRemainder :: [(Integer, Integer)] -> Maybe Integer
chineseRemainder (h1 : h2 : [])   = chinese h1 h2
chineseRemainder (h1 : h2 : rest) =
  chinese h1 h2 >>= \ h1h2 -> chineseRemainder $ (h1h2, ((snd h1) * (snd h2))) : rest
chineseRemainder _ = error "chineseRemaider: input must have length 2 or more"
