{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language DeriveGeneric #-}

module Day14 where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString (ByteString)
import Common (solutions, bsToInt, showBits)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Bits
import Data.List (foldl')
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.Vector.Unboxed  as Vector

day14 :: IO ()
day14 = do
  inp <- parseInput
  let
    sol1 = s1 inp
    sol2 = s2 inp
  solutions 14 sol1 sol2

day14' :: IO (Int, Int)
day14' = do
  inp <- parseInput
  let
    sol1, sol2 :: Int
    sol1 = s1 inp
    sol2 = s2 inp
  pure (sol1, sol2)

parseInput :: IO [Instr]
parseInput = do
  bs <- ByteString.readFile "input/day14.dat"
  let instrs = fmap parseInstr . ByteString.lines $ bs
  pure instrs

parseInstr :: ByteString -> Instr
parseInstr bs = case ByteString.index bs 1 of
  'e' -> MemInstr  $ parseMem  bs
  'a' -> MaskInstr $ parseMask bs
  _   -> error "parseInstr: received unusual instruction"

parseMem :: ByteString -> Mem
parseMem bs = Mem (bsToInt locBS) (bsToInt valBS)
  where
    (locBS, rest) = ByteString.span (/= ']') . ByteString.drop 4 $ bs
    valBS = ByteString.drop 4 $ rest

parseMask :: ByteString -> BitMask
parseMask = parseBits . ByteString.drop 7
  where
    startMask = BitMask 0 0
    parseBits :: ByteString -> BitMask
    parseBits bs =
        (\(BitMask z o) -> BitMask (botBits .&. (complement z)) o)
      . ByteString.foldl' parseBit startMask
      $ bs

    parseBit :: BitMask -> Char -> BitMask
    parseBit (BitMask z o) = \case
      'X' -> BitMask ( z `shiftL` 1)      ( o `shiftL` 1     )
      '1' -> BitMask ( z `shiftL` 1)      ((o `shiftL` 1) + 1)
      '0' -> BitMask ((z `shiftL` 1) + 1) ( o `shiftL` 1     )
      c   -> error $ "parseBit: error with input " <> [c]

s1 :: [Instr] -> Int
s1 = foldl' (+) 0 . memory . runInstr

s2 :: [Instr] -> Int
s2 = foldl' (+) 0 . memory . runInstr2

data Mem = Mem
  { loc :: !Int
  , val :: !Int
  }
  deriving (Show, Generic)

instance NFData Mem

data BitMask = BitMask
  { zBits :: !Word
  , oBits :: !Word
  }
  deriving Generic

instance NFData BitMask

instance Show BitMask where
  show (BitMask z o) = unlines
    [ "zero_mask:"
    , showBits z
    , "one_mask:"
    , showBits o
    ]

data Instr =
    MaskInstr BitMask
  | MemInstr  Mem
  deriving (Show, Generic)

instance NFData Instr

data State = State
  { mask   :: !BitMask
  , memory :: !(IntMap Int)
  }
  deriving Show

startState :: State
startState = State (BitMask 0 0) mempty

runInstr :: [Instr] -> State
runInstr = foldl' interp startState
  where
    interp :: State -> Instr -> State
    interp (State bitMask mem) = \case
      MaskInstr bm -> State bm mem
      MemInstr (Mem newLoc newVal) ->
        State bitMask (IntMap.insert newLoc (applyMask bitMask newVal) mem)

    applyMask :: BitMask -> Int -> Int
    applyMask (BitMask z o) n = (n .|. (fromIntegral o)) .&. (fromIntegral z)


runInstr2 :: [Instr] -> State
runInstr2 = foldl' interp startState
  where
    interp :: State -> Instr -> State
    interp (State !currBM@(BitMask z o) mem) = \case
      MaskInstr bm -> State bm mem
      MemInstr (Mem l v) ->
        let
          xMask = z `xor` o
          locM = (l .|. (fromIntegral o)) .&. (fromIntegral $ complement xMask)
        in
          State currBM (insertFloating mem xMask locM v)

botBits :: Word
botBits = complement 18446744004990074880

toBits :: Word -> Vector.Vector Bool
toBits i = Vector.generate count fn
  where
    count = 64 - (countLeadingZeros i)
    fn = testBit i

insertFloating :: IntMap Int -> Word -> Int -> Int -> IntMap Int
insertFloating mem xBits locM newVal = newMem `IntMap.union` mem
  where
    newMem = Vector.ifoldl' addBits (IntMap.singleton locM newVal) (toBits xBits)
    addBits :: IntMap Int -> Int -> Bool -> IntMap Int
    addBits !acc _   False = acc
    addBits !acc ind True
      = acc `IntMap.union` (IntMap.mapKeysMonotonic (\n -> setBit n ind) acc)
