{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.ST
import           Data.Bits
import           Data.List                   (permutations)
import           Data.STRef
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as IO
import qualified Data.Vector.Unboxed         as V hiding (head)
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV


main :: IO ()
main =
  do
    inputProg <- readInput
    print $ puzzle1 inputProg
    print $ puzzle2 inputProg


puzzle1 :: [Int] -> Int
puzzle1 program = maximum $ (runAmplifiers program) <$> (permutations [0..4])


puzzle2 :: [Int] -> Int
puzzle2 program =  maximum $ (runTimeFeedback program) <$> (permutations [5..9])


-- |
-- Run through the amplifiers with the given phase inputs ending after the
-- final amplifiers.
runAmplifiers :: [Int] -> [Int] -> Int
runAmplifiers programCode = go 0
  where
    go :: Int  -> [Int] -> Int
    go lastOutput []       = lastOutput
    go lastOutput (t : ts) = go (runCode programCode [t , lastOutput]) ts


-- |
-- Run program code with inputs returning the output regardless
-- of whether the program halted.
runCode :: [Int] -> [Int] -> Int
runCode  programCode inputs = fromEither result
  where
    result :: Either Int Int
    result =
      runST
        do
          mv      <- toMVector programCode
          program <- initialiseProgram mv
          runProgram program inputs


data Program s = Program
  { machineState :: {-# UNPACK #-} !(MVector s Int)
  , addressRef   :: {-# UNPACK #-} !(STRef s Int)
  }


-- |
-- Initialises a program with the copy of the input code
-- and the starting memory address.
initialiseProgram :: MVector s Int -> ST s (Program s)
initialiseProgram mv =
  do
    ref <- newSTRef 0
    pure $ Program mv ref


data Amplifier s = Amplifier
  { amp1 :: Program s
  , amp2 :: Program s
  , amp3 :: Program s
  , amp4 :: Program s
  , amp5 :: Program s
  }


-- |
-- This initialises the amplifiers with copies of the input
-- int code.
initialiseAmplifier :: MVector s Int -> ST s (Amplifier s)
initialiseAmplifier program =
  do
    let len = MV.length program
    prog1 <- MV.new len
    prog2 <- MV.new len
    prog3 <- MV.new len
    prog4 <- MV.new len
    prog5 <- MV.new len
    MV.copy prog1 program
    MV.copy prog2 program
    MV.copy prog3 program
    MV.copy prog4 program
    MV.copy prog5 program
    amp1 <- initialiseProgram prog1
    amp2 <- initialiseProgram prog2
    amp3 <- initialiseProgram prog3
    amp4 <- initialiseProgram prog4
    amp5 <- initialiseProgram prog5
    pure $ Amplifier{..}


-- |
-- This runs the program with the given phase inputs until it hits a halt code.
runTimeFeedback :: [Int]  -> [Int] -> Int
runTimeFeedback program phaseInputs =
  runST
    do
      amps <- startAmplifiers
      firstOutput <- firstPhase amps phaseInputs
      go amps firstOutput 1

  where
  -- this initialises each amplifiers controller software with the input code
    startAmplifiers :: forall s .  ST s (Amplifier s)
    startAmplifiers = (toMVector program) >>= initialiseAmplifier


  -- This runs through the amps once with the initial phase inputs
    firstPhase :: forall s . Amplifier s -> [Int] -> ST s (Either Int Int)
    firstPhase Amplifier{..} inputs =
      do
        let phase1 = inputs !! 0
        let phase2 = inputs !! 1
        let phase3 = inputs !! 2
        let phase4 = inputs !! 3
        let phase5 = inputs !! 4
        output1 <- runProgram amp1 [phase1, 0]
        output2 <- runProgram amp2 [phase2, fromEither output1]
        output3 <- runProgram amp3 [phase3, fromEither output2]
        output4 <- runProgram amp4 [phase4, fromEither output3]
        output5 <- runProgram amp5 [phase5, fromEither output4]
        pure output5


 -- after running through the first round this loops the signals until
 -- we hit the halt code
    go :: forall s . Amplifier s -> Either Int Int -> Int -> ST s Int
    go amps@(Amplifier{..}) output ampNo  =
      case output of
        Left outputValue ->
          case ampNo of
            1 ->
              do
                programValue <- runProgram amp1 [outputValue]
                go amps programValue 2
            2 ->
              do
                programValue <- runProgram amp2 [outputValue]
                go amps programValue 3
            3 ->
              do
                programValue <-  runProgram amp3 [outputValue]
                go amps programValue 4
            4 ->
              do
                programValue <- runProgram amp4 [outputValue]
                go amps programValue 5
            5 ->
              do
                programValue <- runProgram amp5 [outputValue]
                go amps programValue 1
            _ ->
              error $ unlines ["called amp number outside range!"
                              , "amp number: " <> show ampNo
                              ]
        Right finalOutput -> pure finalOutput


-- |
-- This runs the program updating the address pointer and the machine state
-- and giving back the output where a Right value means the program has
-- reached a termination state and a Left value is an output code
runProgram :: forall s . Program s -> [Int]  -> ST s (Either Int Int)
runProgram Program{..} = go
  where
    go :: [Int] -> ST s (Either Int Int)
    go currInputs@(~(currInput : nextInputs)) =
      do
        address     <- readSTRef addressRef
        instruction <- MV.read machineState  address
        let (modes, opCode) = instruction `divMod` 100
        case opCode of
          1  ->
            do
              addCode address modes machineState
              modifySTRef addressRef (+ 4)
              go currInputs
          2  ->
            do
              mulCode address modes machineState
              modifySTRef addressRef (+ 4)
              go currInputs
          3 ->
            do
              inputCode address currInput machineState
              modifySTRef addressRef (+ 2)
              go nextInputs
          4 ->
            do
              output <- outputCode address modes machineState
              modifySTRef addressRef (+ 2)
              pure (Left output)
          5 ->
            do
              jumpTo <- jumpCode (== 0) address modes machineState
              writeSTRef addressRef jumpTo
              go currInputs
          6 ->
            do
              jumpTo <- jumpCode (/= 0) address modes machineState
              writeSTRef addressRef jumpTo
              go currInputs
          7  ->
            do
              leCode address modes machineState
              modifySTRef addressRef (+ 4)
              go currInputs
          8  ->
            do
              eqCode address modes machineState
              modifySTRef addressRef (+ 4)
              go currInputs
          99 -> pure (Right currInput)
          n  ->
            do
            v <- V.unsafeFreeze machineState
            error $ unlines $ ["incorrect opcode: " <> show n
                              , "curr input:      " <> show currInput
                              , "address:         " <> show address
                              , "instruction:     " <> show instruction
                              , "machine state:   " <> show v
                              ]


-------------------------
-- Intcode Iterpreters --
-------------------------
inputCode :: Int -> Int -> MVector s Int -> ST s ()
inputCode blockStart input mv =
  do
    address <- MV.read mv (blockStart + 1)
    MV.write mv address input


outputCode :: Int -> Int -> MVector s Int -> ST s Int
outputCode blockStart modes mv =
  do
    address <- MV.read mv (blockStart + 1)
    value   <- if testBit modes 0
                  then pure address
                  else MV.read mv address
    pure value


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


-------------
-- Utility --
-------------
toMVector :: forall s . [Int] -> ST s (MVector s Int)
toMVector = V.unsafeThaw . V.fromList

fromEither :: Either a a -> a
fromEither = either id id


-------------
-- Parsing --
-------------
readInput :: IO [Int]
readInput =
  do
    file <- IO.readFile "input.txt"
    let inps = Text.splitOn "," file
    pure $ fmap (Prelude.read . Text.unpack) inps
