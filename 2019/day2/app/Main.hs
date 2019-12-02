{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments      #-}
module Main where

import Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed as V
import Data.Text.IO as IO
import Data.Text
import Control.Monad.ST
import qualified Criterion.Main        as C (bench, bgroup, defaultMain, whnf, env)
import Data.List.Extra (minimumOn)


bench :: IO ()
bench = C.defaultMain [
  C.env readInput \inputProg ->
    C.bgroup "day2"
    [ C.bench "part 1"  $ C.whnf puzzle1 inputProg
    , C.bench "part 2"  $ C.whnf puzzle2 inputProg
    ]
  ]

main :: IO ()
main =
  do
    inputProg <- readInput
    print $ puzzle1 inputProg
    print $ puzzle2 inputProg

puzzle1 :: [Int] -> Int
puzzle1 = runProgram 12 2

puzzle2 :: [Int] -> Int
puzzle2 inputProg =
  let
    (n, v) = searchForTarget inputProg
  in
    100 * n + v

runProgram :: Int -> Int -> [Int] -> Int
runProgram noun verb listInput =
  runST
    do
      mv <- toMVector listInput
      initialiseProgram noun verb mv
      let len = MV.length mv
      handleBlocks 0 len mv
      MV.read mv 0


searchForTarget :: [Int] -> (Int, Int)
{-# INLINE searchForTarget #-}
searchForTarget inputProg =
    let
      args = [0..99]
      nounMin
        = minimumOn
          (\n -> abs (target - (runProgram n 2 inputProg)))
          args
      verbMin
        = minimumOn
          (\v -> abs (target - (runProgram nounMin v inputProg)))
          args
    in
      (nounMin, verbMin)
  where
    target :: Int
    target = 19690720


readInput :: IO [Int]
{-# INLINE readInput #-}
readInput =
  do
    file <- IO.readFile "input2.txt"
    let inps = splitOn "," file
    pure $ fmap (Prelude.read . unpack) inps


toMVector :: forall s . [Int] -> ST s (MVector s Int)
{-# INLINE toMVector #-}
toMVector = unsafeThaw . V.fromList


initialiseProgram :: Int -> Int -> MVector s Int -> ST s ()
{-# INLINE initialiseProgram #-}
initialiseProgram noun verb mv =
  do
    MV.write mv 1 noun
    MV.write mv 2 verb


handleBlocks :: Int -> Int -> MVector s Int -> ST s ()
{-# INLINE handleBlocks #-}
handleBlocks blockStart blockEnd mv =
  if blockStart >= blockEnd then pure ()
    else
      do
        opCode <- MV.read mv blockStart
        case opCode of
          1  ->
            do
              addCode blockStart mv
              handleBlocks (blockStart + 4) blockEnd mv
          2  ->
            do
              mulCode blockStart mv
              handleBlocks (blockStart + 4) blockEnd mv
          99 -> pure ()
          n  -> error $ "incorrect opcode" <> show n


binCode :: (Int -> Int -> Int) -> Int -> MVector s Int -> ST s ()
{-# INLINE binCode #-}
binCode op blockStart mv =
  do
    first  <- MV.read mv (blockStart + 1)
    second <- MV.read mv (blockStart + 2)
    third  <- MV.read mv (blockStart + 3)
    firstVal  <- MV.read mv first
    secondVal <- MV.read mv second
    let newVal = firstVal `op` secondVal
    MV.write mv third newVal


{-# INLINE addCode #-}
{-# INLINE mulCode #-}
addCode = binCode (+)
mulCode = binCode (*)
