{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.ST
import Data.Bits
import Data.Text                   hiding (head)
import Data.Text.IO                as IO
import Data.Vector.Unboxed         as V hiding (head)
import Data.Vector.Unboxed.Mutable as MV


main :: IO ()
main =
  do
    inputProg <- readInput
    print $ puzzle1 inputProg
    print $ puzzle2 inputProg


puzzle1 :: [Int] -> Int
puzzle1 = head . (runProgram 1)


puzzle2 :: [Int] -> Int
puzzle2 = head . (runProgram 5)


runProgram :: Int -> [Int] -> [Int]
runProgram input listInput =
  runST
    do
      mv <- toMVector listInput
      handleBlocks input mv


readInput :: IO [Int]
readInput =
  do
    file <- IO.readFile "input5.txt"
    let inps = splitOn "," file
    pure $ fmap (Prelude.read . unpack) inps


toMVector :: forall s . [Int] -> ST s (MVector s Int)
toMVector = unsafeThaw . V.fromList


handleBlocks :: forall s . Int -> MVector s Int -> ST s [Int]
handleBlocks input program = go [] 0 program
  where
    go :: [Int] -> Int -> MVector s Int -> ST s [Int]
    go currOutput blockStart mv =
      do
        instruction <- MV.read mv blockStart
        let (modes, opCode) = instruction `divMod` 100
        case opCode of
          1  ->
            do
              addCode blockStart modes mv
              go currOutput (blockStart + 4) mv
          2  ->
            do
              mulCode blockStart modes mv
              go currOutput (blockStart + 4) mv
          3 ->
            do
              inputCode blockStart input mv
              go currOutput (blockStart + 2) mv
          4 ->
            do
              newOutput <- outputCode blockStart modes currOutput mv
              go newOutput (blockStart + 2) mv
          5 ->
            do
              jumpTo <- jumpCode (== 0) blockStart modes mv
              go currOutput jumpTo mv
          6 ->
            do
              jumpTo <- jumpCode (/= 0) blockStart modes mv
              go currOutput jumpTo mv
          7  ->
            do
              leCode blockStart modes mv
              go currOutput (blockStart + 4) mv
          8  ->
            do
              eqCode blockStart modes mv
              go currOutput (blockStart + 4) mv
          99 -> pure currOutput
          n  -> error $ "incorrect opcode " <> show n


inputCode :: Int -> Int -> MVector s Int -> ST s ()
inputCode blockStart input mv =
  do
    address <- MV.read mv (blockStart + 1)
    MV.write mv address input


outputCode :: Int -> Int -> [Int] -> MVector s Int -> ST s [Int]
outputCode blockStart modes currOutput mv =
  do
    address <- MV.read mv (blockStart + 1)
    value   <- if testBit modes 0
                  then pure address
                  else MV.read mv address
    pure (value : currOutput)


jumpCode :: (Int -> Bool) -> Int -> Int -> MVector s Int -> ST s Int
jumpCode prop blockStart modes mv =
  do
    first  <- MV.read mv (blockStart + 1)
    second <- MV.read mv (blockStart + 2)
    firstVal <- if testBit modes 0
                  then pure first
                  else MV.read mv first
    if prop firstVal then pure (blockStart + 3)
      else
          if testBit modes 1
            then pure second
            else MV.read mv second


binCode :: (Int -> Int -> Int) -> Int -> Int -> MVector s Int -> ST s ()
binCode op blockStart modes mv =
  do
    first  <- MV.read mv (blockStart + 1)
    second <- MV.read mv (blockStart + 2)
    third  <- MV.read mv (blockStart + 3)
    firstVal <- if testBit modes 0
                  then pure first
                  else MV.read mv first
    secondVal <- if testBit modes 1
                   then pure second
                   else MV.read mv second
    let newVal = firstVal `op` secondVal
    MV.write mv third newVal


addCode, mulCode, leCode, eqCode :: Int -> Int -> MVector s Int -> ST s ()
addCode = binCode (+)
mulCode = binCode (*)
leCode  = binCode (\x y -> if x < y  then 1 else 0)
eqCode  = binCode (\x y -> if x == y then 1 else 0)
