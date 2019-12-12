{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Control.Monad    (forever)
import           Control.Monad.ST
import           Data.STRef
import qualified IntCode          as IntCode
import           Pipes            (Consumer, Producer, lift, runEffect, (>->))
import qualified Pipes            as Pipes


main :: IO ()
main =
  do
    inputProg <- IntCode.readInput
    putStrLn "puzzle 1: "
    putStrLn $ (replicate 10 ' ') <> (show $ puzzle1 inputProg)
    putStrLn "puzzle 2: "
    putStrLn $ (puzzle2 inputProg)


puzzle1 :: [Int] -> Int
puzzle1 = length . grid . runRobot startPanel1


puzzle2 :: [Int] -> String
puzzle2 = visualiseGrid 10 . grid . runRobot startPanel2 -- IntCode.outputLoop programInput [2]


--------------------
--Process pipeline--
--------------------
runRobot :: Panel -> [Int] -> Panel
runRobot startPanel programInput = runST go
  where
    go :: forall s . ST s Panel
    go =
      runEffect $
        do
          mv       <- lift $ IntCode.toMVector programInput
          prog     <- lift $ IntCode.initialiseProgram mv
          panelRef <- lift $ newSTRef startPanel
          (giveColour panelRef >-> IntCode.runProgram prog >-> processIntCodeOutput panelRef)
          lift $ readSTRef panelRef


giveColour :: STRef s Panel -> Producer Int (ST s) ()
giveColour panelRef = forever $
  do
    Panel{..} <- lift $ readSTRef panelRef
    case Map.findWithDefault Black location grid of
      Black -> Pipes.yield 0
      White -> Pipes.yield 1


processIntCodeOutput :: STRef s Panel -> Consumer Int (ST s) ()
processIntCodeOutput panelRef = forever $
  do
    Panel{..} <- lift $ readSTRef panelRef
    output1 <- Pipes.await
    output2 <- Pipes.await
    let locationColour =
          case output1 of
            0 -> Black
            1 -> White
            _ -> error $ "Recieved output code which is not in {0, 1}: " <> show output1
    let newDir =
          case output2 of
            0 -> turnLeft dir
            1 -> turnRight dir
            _ -> error $ "Recieved output code which is not in {0, 1}: " <> show output1
    let newLocation = advancePoint location newDir
    let newGrid = Map.insert location locationColour grid
    let newPanel = Panel newLocation newDir newGrid
    lift $ writeSTRef panelRef newPanel


----------------
-- Robot types--
----------------
data Colour = White | Black
  deriving (Eq, Show)

data Dir = N | S | E | W
  deriving (Eq, Show, Enum)

turnRight :: Dir -> Dir
turnRight =
  \case
    N -> E
    E -> S
    S -> W
    W -> N

turnLeft :: Dir -> Dir
turnLeft =
  \case
    N -> W
    W -> S
    S -> E
    E -> N

advancePoint :: Point -> Dir -> Point
advancePoint (Point x y) =
  \case
    N -> Point x (y + 1)
    S -> Point x (y - 1)
    W -> Point (x - 1) y
    E -> Point (x + 1) y


data Point = Point
  { x :: {-# UNPACK #-} !Int
  , y :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show, Ord)


enumFromToX :: Int -> Int -> Int -> [Point]
enumFromToX x0 x1 y = [Point xi y | xi <- [x0 .. x1]]


startPoint :: Point
startPoint = Point 0 0


type Grid = Map Point Colour

data Panel = Panel
  { location :: Point
  , dir      :: Dir
  , grid     :: Grid
  }
  deriving (Eq, Show)


startPanel1 :: Panel
startPanel1 = Panel startPoint N mempty


startPanel2 :: Panel
startPanel2 = Panel startPoint N (Map.singleton (Point 0 0) White)


visualiseGrid :: Int -> Grid -> String
visualiseGrid whitespace grid =
  let
    points = Map.keys . Map.filter (== White) $ grid
    minX = minimum . fmap x $ points
    maxX = maximum . fmap x $ points
    minY = minimum . fmap y $ points
    maxY = maximum . fmap y $ points
    processRow :: [Point] -> String
    processRow
      = foldr (\p acc ->
                 case p `Map.lookup` grid of
                   Nothing    -> ' ' : acc
                   Just Black -> ' ' : acc
                   Just White -> 'O' : acc
              )
        mempty
    rows :: [[Point]]
    rows = [enumFromToX minX maxX yi | yi <- [maxY, maxY - 1 .. minY]]
  in
    unlines $
      (replicate whitespace ' ' <>) . processRow <$> rows
