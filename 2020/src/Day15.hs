{-# language BangPatterns #-}
{-# language TypeApplications #-}

module Day15 where

import Common (solutions)
import Data.Foldable (for_)
import qualified Data.Primitive.PrimArray as Array
import Control.Monad.ST
import Data.Int (Int32)

day15 :: IO ()
day15 = do
  inp <- parseInput
  let
    sol1, sol2 :: Int
    sol1 = s1 inp
    sol2 = s2 inp
  solutions 15 sol1 sol2

day15' :: IO (Int, Int)
day15' = do
  inp <- parseInput
  pure (s1 inp, s2 inp)

parseInput :: IO [(Int, Int)]
parseInput = do
  let inp = zip [6,13,1,15,2,0] [1..]
  pure inp

s1 :: [(Int,  Int)] -> Int
s1 inp = runGame 2020 inp 0

s2 :: [(Int, Int)] -> Int
s2 inp = runGame 30000000 inp 0

runGame :: Int -> [(Int, Int)] -> Int -> Int
runGame !targ !inp !lastInput = fromIntegral @Int32 @Int $ runST $ do
  prim <- Array.newPrimArray targ
  Array.setPrimArray prim 0 targ 0
  for_ inp $ \(!v, !t) -> do
    Array.writePrimArray prim v (fromIntegral t)
  go prim (fromIntegral lastInput) 7
  where
    go :: Array.MutablePrimArray s Int32 -> Int32 -> Int -> ST s Int32
    go _ !lastSeen !n | n == targ + 1 = pure lastSeen
    go !prim !lastSeen !n = do
      let lastSeen' = fromIntegral lastSeen
      let nPred = fromIntegral (n - 1)
      lastSeenTurn <- Array.readPrimArray prim lastSeen'
      if lastSeenTurn == 0
        then do
          Array.writePrimArray prim lastSeen' nPred
          go prim 0 (n + 1)
        else do
          Array.writePrimArray prim lastSeen' nPred
          go prim (nPred - lastSeenTurn) (n + 1)
