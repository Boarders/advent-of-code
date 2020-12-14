{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
module Day12 where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Common (solutions, bsToInt)
import Data.Foldable (for_, traverse_)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.List (foldl')
import Data.Semigroup (Semigroup(stimes))

day12 :: IO ()
day12 = do
  inp <- parseInput
  let
    sol1, sol2 :: Int
    sol1 = 1 -- s1 inp
    sol2 = 1 -- s2 inp
--  print inp
--  print $ s1 inp
--  solutions 12 sol1 sol2
  print $ s2 inp

  

day12' :: IO (Int, Int)
day12' = do
  inp <- parseInput
  let
    sol1, sol2 :: Int
    sol1 = 1 -- s1 inp
    sol2 = 1 -- s2 inp
  pure (sol1, sol2)


parseInput :: IO [Instr]
parseInput = do
  bs <- ByteString.readFile "input/day12.dat"
--  let bs = test
  let instrs = fmap parseEntry . ByteString.lines $ bs
  pure instrs

parseEntry :: ByteString -> Instr
parseEntry bs = case ByteString.head bs of
  'N' -> MoveInstr (Move N n)
  'S' -> MoveInstr (Move S n)
  'E' -> MoveInstr (Move E n)
  'W' -> MoveInstr (Move W n)
  'L' -> case n of
    90  -> TurnInstr L
    180 -> TurnInstr F
    270 -> TurnInstr R
  'R' -> case n of
    90  -> TurnInstr R
    180 -> TurnInstr F
    270 -> TurnInstr L
    n   -> error $ show n
  'F' -> FrwdInstr n
  where
    n = bsToInt (ByteString.tail bs)

s1 :: [Instr] -> Int
s1 = manhattan . pos . runInstr

s2 :: [Instr] -> Int
s2 = manhattan . ship . interp2


data Dir  = N | E | S | W
  deriving Show
data Turn = L | R | F

instance Show Turn where
  show = \case
    L  -> "L90"
    R  -> "R90"
    F  -> "Full"

data Move = Move
  { dir :: !Dir
  , len :: !Int
  }
instance Show Move where
  show (Move d l) = show d <> show l

data Instr =
    MoveInstr !Move
  | TurnInstr !Turn
  | FrwdInstr !Int

instance Show Instr where
  show = \case
    MoveInstr m -> show m
    TurnInstr t -> show t
    FrwdInstr n -> "F" <> show n

data Point = Point
  { xP :: !Int
  , yP :: !Int
  }
  deriving Show

instance Semigroup Point where
  (Point x1 y1) <> (Point x2 y2) = (Point (x1 + x2) (y1 + y2))



data State = State
  { pos :: Point
  , dir :: Dir
  }

startState :: State
startState = State (Point 0 0) E
  

runInstr :: [Instr] -> State
runInstr = foldl' interp startState
  where
    interp :: State -> Instr -> State
    interp (State pos dir) = \case
      MoveInstr m -> State (movePos pos m)            dir
      TurnInstr t -> State pos                       (changeDir dir t)
      FrwdInstr n -> State (movePos pos (Move dir n)) dir

    movePos (Point x y) (Move d n) =
      case d of
        E -> (Point (x + n) y)
        W -> (Point (x - n) y)
        S -> (Point x       (y - n))
        N -> (Point x       (y + n))
      
changeDir :: Dir -> Turn -> Dir
changeDir E =
  \case
    L -> N
    R -> S
    F -> W
changeDir N =
  \case
    L -> W
    R -> E
    F -> S  
changeDir S =
  \case
    L -> E
    R -> W
    F -> N    
changeDir W =
  \case
    L  -> S
    R  -> N
    F  -> E    

startState2 = State2 (Point 0 0) (Point 10 1)

data State2 = State2
  { ship     :: Point
  , waypoint :: Point
  }
  deriving Show

interp2 :: [Instr] -> State2
interp2 = foldl' interp startState2
  where
  interp :: State2 -> Instr -> State2
  interp (State2 ship wayP) = \case
    MoveInstr m -> State2 ship (movePos wayP m)
    TurnInstr t -> State2 ship (rotate wayP t)
    FrwdInstr n -> State2 (ship <> (stimes n wayP)) wayP
    
  movePos (Point x y) (Move d n) =
    case d of
      E -> (Point (x + n) y)
      W -> (Point (x - n) y)
      S -> (Point x       (y - n))
      N -> (Point x       (y + n))

rotate :: Point -> Turn -> Point
rotate (Point x y) = \case
  R -> Point y     (- x)
  F -> Point (- x) (- y)
  L -> Point (- y)  x

manhattan :: Point -> Int
manhattan (Point x y) = abs x + abs y


test = ByteString.unlines
  [ "F10"
  , "N3"
  , "F7"
  , "R90"
  , "F11"
  ]
