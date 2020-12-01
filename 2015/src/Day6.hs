{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
module Day6 where

import qualified Data.ByteString.Char8 as ByteString
import Common (solutions)
import Control.Monad.ST
import Data.Matrix.Unboxed.Mutable as Mutable
import Data.Matrix.Unboxed as Matrix
import Data.Foldable (for_, traverse_)
import Data.Attoparsec.ByteString.Char8



day6 :: IO ()
day6 = do
  bs <- ByteString.readFile "input/day6.dat"
  let
    instrs = traverse (parseOnly parseInstr) (ByteString.lines bs)
    sol1 = countOn . runGrid  <$> instrs
    sol2 = countBrightness . runGrid2 <$> instrs
  putStrLn $ (solutions 2 sol1 sol2)

type Switch = Bool

data Range = Range
  { xRange :: !(Int, Int)
  , yRange :: !(Int, Int)
  }
  deriving Show

type Grid s = MMatrix s Bool
pattern On :: Bool
pattern On = True

pattern Off :: Bool
pattern Off = False
{-# complete On, Off #-}

data Instr =
    Toggle
  | Turn Switch
  deriving Show

parseInstr :: Parser (Instr, Range)
parseInstr = do
  _ <- char 't'
  c <- peekChar'
  case c of
    'u' -> do
      _ <- string "urn "
      _ <- char 'o'
      c' <- peekChar'
      case c' of
        'n' -> do
          _ <- string "n "
          r <- parseRanges
          pure (Turn On, r)
        _ -> do
          _ <- string "ff "
          r <- parseRanges
          pure (Turn Off, r)
    _ -> do
      _ <- string "oggle "
      r <- parseRanges
      pure (Toggle, r)
  where
    parseRanges = do
      x1 <- decimal
      _ <- char ','
      y1 <- decimal
      _ <- string " through "
      x2 <- decimal
      _ <- char ','
      y2 <- decimal
      pure (Range (x1, x2) (y1, y2))



startGrid :: ST s (MMatrix s Bool)
startGrid = Mutable.replicate (1000, 1000) False

toggle :: Grid s -> (Int, Int) -> ST s ()
toggle grid i = do
  curr <- Mutable.read grid i
  write grid i (not curr)

interp :: (MMatrix s Bool) -> (Instr, Range) -> ST s ()
interp grid instr =
  case instr of
    (Toggle, Range (x1, x2) (y1, y2)) ->
      for_ [x1 .. x2] $ \x ->
        for_ [y1 .. y2] $ \y ->
          toggle grid (x, y)
    (Turn tog, Range (x1, x2) (y1, y2)) ->
      for_ [x1 .. x2] $ \x ->
        for_ [y1 .. y2] $ \y ->
          write grid (x, y) tog

runGrid :: [(Instr, Range)] -> Matrix Bool
runGrid is = Matrix.create $ do
  grid <- startGrid
  traverse_ (interp grid) is
  pure grid

countOn :: Matrix Bool -> Int
countOn = Matrix.foldl c 0
  where
    c :: Int -> Bool -> Int
    c acc = \case
      True  -> acc + 1
      False -> acc


startGrid2 :: ST s (MMatrix s Int)
startGrid2 = Mutable.replicate (1000, 1000) 0

toggle2 :: MMatrix s Int -> (Int, Int) -> ST s ()
toggle2 grid i = do
  curr <- Mutable.read grid i
  write grid i (curr + 2)

on2 :: MMatrix s Int -> (Int, Int) -> ST s ()
on2 grid i = do
  curr <- Mutable.read grid i
  write grid i (curr + 1)

off2 :: MMatrix s Int -> (Int, Int) -> ST s ()
off2 grid i = do
  curr <- Mutable.read grid i
  write grid i (max (curr - 1) 0)


interp2 :: (MMatrix s Int) -> (Instr, Range) -> ST s ()
interp2 grid instr =
  case instr of
    (Toggle, (Range (x1, x2) (y1, y2))) ->
      for_ [x1 .. x2] $ \x ->
        for_ [y1 .. y2] $ \y ->
          toggle2 grid (x, y)
    (Turn On, Range (x1, x2) (y1, y2)) ->
      for_ [x1 .. x2] $ \x ->
        for_ [y1 .. y2] $ \y ->
          on2 grid (x, y)
    (Turn Off, Range (x1, x2) (y1, y2)) ->
      for_ [x1 .. x2] $ \x ->
        for_ [y1 .. y2] $ \y ->
          off2 grid (x, y)

countBrightness :: Matrix Int -> Int
countBrightness = Matrix.foldl (+) 0


runGrid2 :: [(Instr, Range)] -> Matrix Int
runGrid2 is = Matrix.create $ do
  grid <- startGrid2
  traverse_ (interp2 grid) is
  pure grid
