{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language BangPatterns #-}
module Day8 where

import qualified Data.ByteString.Char8 as ByteString
import Common (solutions, bsToInt)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as Mutable
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Text as Text
import TextShow (TextShow(..))
import Data.Functor (void)
import Control.Monad.ST

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Vector.Unboxed.Deriving (derivingUnbox)

data Op =
    Nop
  | Acc
  | Jmp
  deriving (Generic)

instance Enum Op where
  toEnum n | n == 0 = Nop
           | n == 1 = Acc
           | n == 2 = Jmp
           | otherwise = errorWithoutStackTrace "Enum.Op.toEnum: bad argument"
  fromEnum = \case
    Nop -> 0
    Acc -> 1
    Jmp -> 2

instance NFData Op

prettyOp :: Op -> Text.Text
prettyOp = \case
  Nop -> "nop"
  Acc -> "acc"
  Jmp -> "jmp"

prettyInstr :: Instr -> Text.Text
prettyInstr (Instr instr par) | par >= 0  = prettyOp instr <> " +" <> showt par
                              | otherwise = prettyOp instr <> " " <>  showt par

data Instr = Instr
  { op    :: !Op
  , param :: !Int
  }
  deriving (Generic)

instance NFData Instr

derivingUnbox "Instr"
    [t|  Instr -> (Int, Int) |]
    [| \ (Instr o p) ->  (fromEnum o, p) |]
    [| \ (o, p) ->  Instr (toEnum o) p   |]

day8 :: IO ()
day8 = do
  input <- parseInput
  let
    sol1 = s1 input
    sol2 = s2 input
  solutions 8 sol1 sol2

day8' :: IO (Int, Int)
day8' = do
  input <- parseInput
  let
    sol1 = s1 input
    sol2 = s2 input
  pure (sol1, sol2)

flipInstr :: Instr -> Instr
flipInstr (Instr Nop n) = Instr Jmp n
flipInstr (Instr Jmp n) = Instr Nop n
flipInstr instr = instr

data FinalState = Halt Int |  Loop Int

toInt :: FinalState -> Int
toInt (Halt n) = n
toInt (Loop n) = n


parseInput :: IO (Vector Instr)
parseInput = do
  bs <- ByteString.readFile "input/day8.dat"
  let parsedM = fmap parseInstr (ByteString.lines bs)
  pure $ Vector.fromList parsedM

{-
parseInput :: IO (Vector Instr)
parseInput = do
  bs <- ByteString.readFile "input/day8.dat"
  let parsedM = traverse (Parser.parseOnly parseInstr) (ByteString.lines bs)
  case parsedM of
    Left  str     -> error str
    Right parsed -> do
      let instrs = Vector.fromList $ parsed
      pure instrs
-}
parseInstr :: ByteString.ByteString -> Instr
parseInstr bs =
  let instr : numBS : _ = ByteString.words bs in
  let ds = bsToInt numBS in
  case instr of
    "nop" -> Instr Nop ds
    "acc" -> Instr Acc ds
    "jmp" -> Instr Jmp ds
    other -> error $ "parseInstr: Instruction not implemented " <> (ByteString.unpack other)

{-
parseInstr :: Parser Instr
parseInstr = do
  instr <- Parser.takeTill (== ' ')
  void (Parser.space)
  ds <- Parser.signed Parser.decimal
  case instr of
    "nop" -> pure $ Instr Nop ds
    "acc" -> pure $ Instr Acc ds
    "jmp" -> pure $ Instr Jmp ds
    other -> error $ "parseInstr: Instruction not implemented " <> (ByteString.unpack other)
-}
s1 :: Vector Instr -> Int
s1 instrs = runST $ do
  mInstrs <- Vector.unsafeThaw instrs
  toInt <$> run mInstrs


run :: forall s . MVector s Instr -> ST s FinalState
run !mInstrs = go mInstrs  0 0 mempty
  where
    !len = Mutable.length mInstrs
    go :: MVector s Instr -> Int -> Int -> IntSet -> ST s FinalState
    go !mv !i !acc !s
      | i >= len = pure (Halt acc)
      | i `IntSet.member` s = pure (Loop acc)
      | otherwise = do
          nextInstr <- Mutable.unsafeRead mv i
          case nextInstr of
            Instr Nop _ -> go mv (i + 1) acc       (IntSet.insert i s)
            Instr Acc a -> go mv (i + 1) (acc + a) (IntSet.insert i s)
            Instr Jmp j -> go mv (i + j) acc       (IntSet.insert i s)


flipNext  :: forall s .  MVector s Instr -> Int -> ST s Int
flipNext mv currInd = go currInd
  where
    go :: Int -> ST s Int
    go !n = do
      nextInstr <- Mutable.unsafeRead mv n
      case nextInstr of
        Instr Acc _ -> go (n + 1)
        _ -> do
          Mutable.unsafeModify mv flipInstr n
          pure n

s2 :: Vector Instr -> Int
s2 instrs = runST $ do
  mInstrs <- Vector.unsafeThaw instrs
  go mInstrs 0
  where
    go :: MVector s Instr -> Int -> ST s Int
    go mv !ind = do
      n'  <- flipNext mv ind
      res <- run mv
      case res of
        Halt acc -> pure acc
        Loop _   -> do
          Mutable.unsafeModify mv flipInstr n'
          go mv (n' + 1)
