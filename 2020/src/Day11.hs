{-# language OverloadedStrings #-}
{-# language LambdaCase        #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language BangPatterns #-}
{-# language Strict #-}
module Day11 where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Common (solutions)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as Mutable
import qualified Data.Text as Text
import Control.Monad.ST

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector  as Builder
import Control.Arrow ((&&&))
import Text.Builder as TextBuilder
import Data.Foldable(foldrM, traverse_)
import Data.Monoid (All(..))
import Control.Monad (filterM)
import Data.Word (Word8)


data Position =
    Empty
  | Occupied
  | Floor
  deriving (Eq, Generic, Show)

fromW8 :: Word8 -> Position
fromW8 n | n == 0 = Empty
         | n == 1 = Occupied
         | n == 2 = Floor
         | otherwise = errorWithoutStackTrace "Enum.Op.toEnum: bad argument"

toW8 :: Position -> Word8
toW8 = \case
    Empty    -> 0
    Occupied -> 1
    Floor    -> 2

instance NFData Position

prettyPos :: Position -> Char
prettyPos = \case
  Occupied -> '🧘'
  Empty    -> '💺' 
  Floor    -> '　'

asciiPos :: Position -> Char
asciiPos = \case
  Occupied -> '#'
  Empty    -> 'L' 
  Floor    -> '.'  

derivingUnbox "Position"
    [t|  Position -> Word8  |]
    [| \ p ->  toW8   p     |]
    [| \ p ->  fromW8 p     |]

rowsToText :: Int -> Vector Position -> Text.Text
rowsToText r v =
    Text.unlines
  . fmap (Text.center (r + 30) ' ')
  . Text.chunksOf r
  . TextBuilder.run
  . Vector.foldl' (\acc x -> acc <> (TextBuilder.char . prettyPos $ x)) mempty
  $ v

rows :: Int -> Vector Position -> Text.Text
rows r v =
    Text.unlines
  . fmap (Text.center (r + 30) ' ')
  . Text.chunksOf r
  . TextBuilder.run
  . Vector.foldl' (\acc x -> acc <> (TextBuilder.char . asciiPos $ x)) mempty
  $ v  

day11 :: IO ()
day11 = do
  input <- parseInput
  let
    sol1 = s1 input
    sol2 = s2 input
  solutions 11 sol1 sol2
      
parseInput :: IO (Int, Vector Position)
parseInput = do
  bs <- ByteString.readFile "input/day11.dat"
  let
    (rowS, rowB) = (ByteString.length . head) &&& (foldMap buildRow) $ (ByteString.lines bs)
  pure $ (rowS, Builder.build rowB)

buildRow :: ByteString -> Builder.Builder Position
buildRow bs = ByteString.foldl' fromEntry mempty bs
  where
    fromEntry :: Builder.Builder Position -> Char -> Builder.Builder Position
    fromEntry acc = \case
      'L' -> acc <> Builder.singleton Empty
      '.' -> acc <> Builder.singleton Floor
      '#' -> acc <> Builder.singleton Occupied
      c   -> error $ "buildRow: unexpected character: " <> [c] 

s1 :: (Int, Vector Position) -> Int
s1 (r, v) = countOccupied (runGrid  r v)

s2 :: (Int, Vector Position) -> Int
s2 (r, v) = countOccupied (runGrid2 r v)


computeIndex :: Int -> (Int, Int) -> Int
computeIndex r (i,j) = r * i + j

index :: (Int, Int) -> (Int, MVector s Position) -> ST s (Maybe Position)
index p@(i, j) (r, mv)
    | i < 0 || j < 0 || i >= cols || j >= r = pure Nothing
    | otherwise = Just <$> Mutable.read mv ind
  where
  cols = Mutable.length mv `div` r
  ind  = computeIndex r p

eqGrid :: MVector s Position -> MVector s Position -> ST s Bool
eqGrid mv1 mv2 = do
  v1  <- Vector.unsafeFreeze mv1
  v2  <- Vector.unsafeFreeze mv2
  pure (v1 == v2)

countOccupied :: Vector Position -> Int
countOccupied = Vector.foldl' (\acc x -> acc + (fromEnum . (== Occupied) $ x)) 0


runGrid :: Int -> Vector Position -> Vector Position
runGrid r inputV = runST $ do
  oldMV <- Vector.thaw inputV
  newMV <- Mutable.clone oldMV
  updateUntilEq r oldMV newMV
  Vector.freeze newMV

updateUntilEq
  :: Int
  -> MVector s Position
  -> MVector s Position
  -> ST s ()
updateUntilEq r oldMV newMV = do
  updateAll r oldMV newMV
  eq <- eqGrid oldMV newMV
  if eq
    then pure ()
    else
      do
        Mutable.copy oldMV newMV
        updateUntilEq r oldMV newMV

updateAll
  :: forall s . Int
  -> MVector s Position
  -> MVector s Position
  -> ST s ()
updateAll r oldMV newMV = do
  let len  = Mutable.length oldMV
  let inds = [(i,j) | i <- [0..(div len r) - 1], j <- [0..(r - 1)]]
  traverse_ (update r oldMV newMV) inds

update
  :: forall s .
     Int 
  -> MVector s Position
  -> MVector s Position
  -> (Int, Int)
  -> ST s ()
update r oldMV newMV ind@(i,j) = do
  posM <- index  (i,j) (r, oldMV)
  case posM of
    Nothing -> error "update: internal index error"
    Just pos ->
      case pos of
        Empty -> do
          allUnoccupied <- foldrM (unoccupied ind) mempty dirs
          if getAll allUnoccupied
            then Mutable.write newMV vInd Occupied
            else pure ()
        Occupied -> do
          someOccupied <- (>= 4) . Prelude.length <$> filterM (occupied ind) dirs
          if someOccupied
            then Mutable.write newMV vInd Empty >> pure ()
            else pure ()
        _ -> pure ()
  where
    vInd = computeIndex r ind
    unoccupied :: (Int, Int) -> (Int, Int) -> All -> ST s All
    unoccupied p dir allB = do
      pos <- index (addP p dir) (r, oldMV)
      pure (All (pos /= Just Occupied) <> allB)

    occupied :: (Int, Int) -> (Int, Int) -> ST s Bool
    occupied p dir = do
      pos <- index (addP p dir) (r, oldMV)
      pure (pos == Just Occupied)


runGrid2 :: Int -> Vector Position -> Vector Position
runGrid2 r inputV = runST $ do
  oldMV <- Vector.thaw inputV
  newMV <- Mutable.clone oldMV
  updateUntilEq2 r oldMV newMV
  Vector.freeze newMV  

updateUntilEq2
  :: Int
  -> MVector s Position
  -> MVector s Position
  -> ST s ()
updateUntilEq2 r oldMV newMV = do
  updateAll2 r oldMV newMV
  eq <- eqGrid oldMV newMV
  if eq
    then pure ()
    else
      do
        Mutable.copy oldMV newMV
        updateUntilEq2 r oldMV newMV

updateAll2
  :: forall s . Int
  -> MVector s Position
  -> MVector s Position
  -> ST s ()
updateAll2 r oldMV newMV = do
  let len  = Mutable.length oldMV
  let inds = [(i,j) | i <- [0..(div len r) - 1], j <- [0..(r - 1)]]
  traverse_ (update2 r oldMV newMV) inds

update2
  :: forall s .
     Int 
  -> MVector s Position
  -> MVector s Position
  -> (Int, Int)
  -> ST s ()
update2 r oldMV newMV ind@(i,j) = do
  posM <- index  (i,j) (r, oldMV)
  case posM of
    Nothing -> error "update: internal index error"
    Just pos ->
      case pos of
        Empty -> do
          allUnoccupied <- foldrM (unoccupied ind) mempty dirs
          if getAll allUnoccupied
            then Mutable.write newMV vInd Occupied
            else pure ()
        Occupied -> do
          someOccupied <- (>= 5) . Prelude.length <$> filterM (occupied ind) dirs
          if someOccupied
            then Mutable.write newMV vInd Empty >> pure ()
            else pure ()
        _ -> pure ()
  where
    vInd = computeIndex r ind
    unoccupied :: (Int, Int) -> (Int, Int) -> All -> ST s All
    unoccupied p dir allB = do
      occ <- occupiedRay r (addP p dir) dir oldMV
      pure (All (not occ) <> allB)

    occupied :: (Int, Int) -> (Int, Int) -> ST s Bool
    occupied p dir = occupiedRay r (addP p dir) dir oldMV


occupiedRay :: Int -> (Int, Int) -> (Int, Int) -> MVector s Position -> ST s Bool
occupiedRay r p dir mv = do
  pos <- index p (r, mv)
  case pos of
    Nothing    -> pure False
    Just Floor -> occupiedRay r (addP p dir) dir mv
    Just seat  -> pure (seat == Occupied)
    
      
addP :: (Int, Int) -> (Int, Int) -> (Int, Int)
addP (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dirs :: [(Int, Int)]
dirs = [(i, j) | i <- [-1..1], j <- [-1..1], (i, j) /= (0,0)]
