{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.ST
import           Data.Foldable               (traverse_)
import           Data.STRef
import           Data.STRef.Unboxed
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as IO
import qualified Data.Vector.Unboxed         as V hiding (head)
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Pipes                       (Pipe, Producer, lift, (>->))
import qualified Pipes                       as Pipes
import qualified Pipes.Prelude               as Pipes


main :: IO ()
main =
  do
    inputProg <- readInput
    putStrLn "puzzle 1: "
    putStrLn $ (replicate 10 ' ') <> (show $ puzzle1 inputProg)
    putStrLn "puzzle 2: "
    putStrLn $ (replicate 10 ' ') <> (show $ puzzle2 inputProg)


puzzle1 :: [Int] -> [Int]
puzzle1 programInput =  outputLoop programInput [1]


puzzle2 :: [Int] -> [Int]
puzzle2 programInput = outputLoop programInput [2]


data Program s = Program
  { machineStateRef :: {-# UNPACK #-} !(STRef  s (MVector s Int))
  , addressRef      :: {-# UNPACK #-} !(STRefU s Int )
  , relativeBaseRef :: {-# UNPACK #-} !(STRefU s Int )
  }


-- |
-- Initialises a program with the copy of the input code
-- and the starting memory address.
initialiseProgram :: MVector s Int -> ST s (Program s)
initialiseProgram machineState  =
  do
    addressRef      <- newSTRefU 0
    relativeBaseRef <- newSTRefU 0
    machineStateRef <- newSTRef machineState
    pure $ Program{..}


outputLoop :: [Int] -> [Int] -> [Int]
outputLoop programInput inputs = runST $ getResults
  where
    getResults :: forall s . ST s [Int]
    getResults = Pipes.toListM runPipe

    runPipe :: forall s . Producer Int (ST s) ()
    runPipe =
        do
          mv       <- lift $ toMVector programInput
          prog     <- lift $ initialiseProgram mv
          traverse_ (\i -> Pipes.yield i >-> (runProgram prog)) inputs

-- |
-- This runs the program updating the address pointer and the machine state
-- and giving back the output where a Right value means the program has
-- reached a termination state and a Left value is an output code
runProgram :: forall s . Program s -> Pipe Int Int (ST s) ()
runProgram Program{..} = go
  where
    go :: Pipe Int Int (ST s) ()
    go =
      do
        address      <- lift $ readSTRefU addressRef
        machineState <- lift $ readSTRef  machineStateRef
        relativeBase <- lift $ readSTRefU relativeBaseRef
        instruction  <- lift $ MV.read machineState  address
        let (modes, opCode) = instruction `divMod` 100
        case opCode of
          1  ->
            do
              lift $ addCode address modes relativeBase machineStateRef
              lift $ modifySTRefU addressRef (+ 4)
              go
          2  ->
            do
              lift $ mulCode address modes relativeBase machineStateRef
              lift $ modifySTRefU addressRef (+ 4)
              go
          3 ->
            do
              currInput <- Pipes.await
              lift $ inputCode address currInput modes relativeBase  machineStateRef
              lift $ modifySTRefU addressRef (+ 2)
              go
          4 ->
            do
              output <- lift $ outputCode address modes relativeBase machineState
              lift $ modifySTRefU addressRef (+ 2)
              Pipes.yield output
              go
          5 ->
            do
              jumpTo <- lift $ jumpCode (== 0) address modes relativeBase machineState
              lift $ writeSTRefU addressRef jumpTo
              go
          6 ->
            do
              jumpTo <- lift $ jumpCode (/= 0) address modes relativeBase machineState
              lift $ writeSTRefU addressRef jumpTo
              go
          7  ->
            do
              lift $ leCode address modes relativeBase machineStateRef
              lift $ modifySTRefU addressRef (+ 4)
              go
          8  ->
            do
              lift $ eqCode address modes relativeBase machineStateRef
              lift $ modifySTRefU addressRef (+ 4)
              go
          9  ->
            do
              baseAdjust <- lift $ baseCode address modes relativeBase machineState
              lift $ modifySTRefU relativeBaseRef (+ baseAdjust)
              lift $ modifySTRefU addressRef (+ 2)
              go
          99 -> pure ()
          n  ->
            do
            v <- lift $ V.unsafeFreeze machineState
            error $ unlines $ ["incorrect opcode: " <> show n
                              , "address:         " <> show address
                              , "instruction:     " <> show instruction
                              , "machine state:   " <> show v
                              ]


-------------------------
-- Intcode Iterpreters --
-------------------------
inputCode :: Int -> Int -> Int -> Int -> STRef s (MVector s Int) -> ST s ()
inputCode blockStart input modes relativeBase mvRef =
  do
    mv      <- readSTRef mvRef
    address <- MV.read mv (blockStart + 1)
    modeWrite modes 0 address relativeBase input mvRef



outputCode :: Int -> Int -> Int -> MVector s Int -> ST s Int
outputCode blockStart modes relativeBase mv  =
  do
    address <- MV.read mv (blockStart + 1)
    value   <- modeRead modes 0 address relativeBase mv
    pure value


jumpCode :: (Int -> Bool) -> Int -> Int -> Int -> MVector s Int -> ST s Int
jumpCode prop blockStart modes relativeBase mv =
  do
    first  <- MV.read mv (blockStart + 1)
    second <- MV.read mv (blockStart + 2)
    firstVal <- modeRead modes 0 first relativeBase mv
    if prop firstVal then pure (blockStart + 3)
      else modeRead modes 1 second relativeBase mv



binCode :: (Int -> Int -> Int) -> Int -> Int -> Int -> STRef s (MVector s Int) -> ST s ()
binCode op blockStart modes relativeBase mvRef =
  do
    mv     <- readSTRef mvRef
    first  <- MV.read mv (blockStart + 1)
    second <- MV.read mv (blockStart + 2)
    third  <- MV.read mv (blockStart + 3)
    firstVal  <- modeRead modes 0 first  relativeBase mv
    secondVal <- modeRead modes 1 second relativeBase mv
    let newVal = firstVal `op` secondVal
    modeWrite modes 2 third relativeBase newVal mvRef


addCode, mulCode, leCode, eqCode :: Int -> Int -> Int ->  STRef s (MVector s Int) -> ST s ()
addCode = binCode (+)
mulCode = binCode (*)
leCode  = binCode (\x y -> if x < y  then 1 else 0)
eqCode  = binCode (\x y -> if x == y then 1 else 0)


baseCode :: Int -> Int -> Int -> MVector s Int -> ST s Int
baseCode blockStart modes relativeBase  mv =
  do
    address <- MV.read mv (blockStart + 1)
    value   <- modeRead modes 0 address relativeBase mv
    pure value

-------------
-- Utility --
-------------

getDigit :: Int -> Int -> Int
getDigit modes unitPosition =
  let
    decimalPosition = 10 ^ unitPosition
  in
    (modes `mod` (decimalPosition * 10)) `div` decimalPosition

modeWrite :: (MV.Unbox a) => Int -> Int -> Int -> Int -> a -> STRef s (MVector s a) -> ST s ()
modeWrite modes unitPosition ind relativeBase newVal mvRef =
  let
    writeMode = getDigit modes unitPosition
  in
    case writeMode of
        0 -> dynamicWrite mvRef ind                  newVal
        2 -> dynamicWrite mvRef (ind + relativeBase) newVal
        _ -> error $ "Received mode outside range {0, 2} in write mode: " <> show writeMode


modeRead :: Int -> Int -> Int -> Int -> MVector s Int -> ST s Int
modeRead modes unitPosition ind relativeBase mv =
  let
    readMode = getDigit modes unitPosition
  in
    case readMode of
      0 -> readWithDefault 0 mv ind
      1 -> pure ind
      2 -> readWithDefault 0 mv (ind + relativeBase)
      _ -> error $ "Received mode outside range {0, 1, 2} in read mode: " <> show readMode


dynamicWrite :: (MV.Unbox a) => STRef s (MVector s a) -> Int -> a  -> ST s ()
dynamicWrite mvRef ind val =
  do
    mv <- readSTRef mvRef
    let maxInd = MV.length mv - 1
    if ind > maxInd then
      do
        let growLength = max (ind - maxInd) maxInd
        mv' <- MV.grow mv growLength
        MV.write mv' ind val
        writeSTRef mvRef mv'
    else
      MV.write mv ind val


readWithDefault :: (MV.Unbox a) => a -> MVector s a -> Int -> ST s a
readWithDefault def mv ind =
  let
    maxInd = MV.length mv - 1
  in
    if ind > maxInd then (pure def)
    else MV.read mv ind

toMVector :: forall s . [Int] -> ST s (MVector s Int)
toMVector = V.unsafeThaw . V.fromList

-------------
-- Parsing --
-------------
readInput :: IO [Int]
readInput =
  do
    file <- IO.readFile "input.txt"
    let inps = Text.splitOn "," file
    pure $ fmap (Prelude.read . Text.unpack) inps

