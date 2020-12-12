{-# language OverloadedStrings #-}
{-# language LambdaCase        #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language BangPatterns #-}
--{-# language Strict #-}
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
import Data.Foldable(traverse_)
import Data.Monoid (All(..))
import Data.Word (Word8)
import Data.Coerce

import qualified Data.Vector.Primitive as Primitive


data Position =
    Empty
  | Occupied
  | Floor
  deriving (Eq, Generic, Show)

data Point = Point
  { xCoord :: !Int
  , yCoord :: !Int
  }
  deriving (Eq)

{-# inline fromW8 #-}
fromW8 :: Word8 -> Position
fromW8 n | n == 0 = Empty
         | n == 1 = Occupied
         | n == 2 = Floor
         | otherwise = errorWithoutStackTrace "Enum.fromW8: bad argument"

{-# inline toW8 #-}
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
    [| toW8     |]
    [| fromW8   |]

derivingUnbox "Point"
    [t|  Point -> (Int, Int)  |]
    [| \ (Point x y) ->  (x,y)   |]
    [| \ (x, y) ->  Point x y  |]

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
    (!rowS, rowB) = (ByteString.length . head) &&& (foldMap buildRow) $ (ByteString.lines bs)
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


computeIndex :: Int -> Point -> Int
computeIndex r (Point i j) = r * i + j
{-# INLINE computeIndex #-}

index :: Int -> Point ->  MVector s Position -> ST s (Maybe Position)
index !r !p@(Point i j) !mv
    | i < 0 || j < 0 || i >= cols || j >= r = pure Nothing
    | otherwise = Just <$> Mutable.unsafeRead mv ind
  where
  !cols = Mutable.length mv `div` r
  !ind  = computeIndex r p
{-# INLINE index #-}


eqGrid :: MVector s Position -> MVector s Position -> ST s Bool
eqGrid mv1 mv2 = do
  v1  <- Vector.unsafeFreeze mv1
  v2  <- Vector.unsafeFreeze mv2
  pure (v1 == v2)
{-# INLINE eqGrid #-}

{-
eqGrid :: MVector s Position -> MVector s Position -> Bool
eqGrid = coerce eqWord8Prim
{-# INLINE eqGrid #-}

eqWord8Prim :: Primitive.MVector s Word8 -> Primitive.MVector s Word8 -> Bool
eqWord8Prim (Primitive.MVector _ _ !mba1) (Primitive.MVector _ _ !mba2) =
  mba1 == mba2


eqGrid :: MVector s Position -> MVector s Position -> ST s Bool
eqGrid (Primitive.MVector off1 len1 mba1) (Primitive.MVector off2 len2 mba2) =
  pure $  mba1 == mba2
{-# INLINE eqGrid #-}
-}
countOccupied :: Vector Position -> Int
countOccupied = Vector.foldl' (\acc x -> acc + (fromEnum . (== Occupied) $ x)) 0


runGrid :: Int -> Vector Position -> Vector Position
runGrid !r !inputV = runST $ do
  !oldMV <- Vector.thaw inputV
  !newMV <- Mutable.clone oldMV
  updateUntilEq r oldMV newMV
  Vector.freeze newMV

updateUntilEq
  :: Int
  -> MVector s Position
  -> MVector s Position
  -> ST s ()
updateUntilEq !r !oldMV !newMV = do
  updateAll r oldMV newMV
  eq <- eqGrid oldMV newMV
  if eq
    then pure ()
    else
      do
        Mutable.unsafeCopy oldMV newMV
        updateUntilEq r oldMV newMV

updateAll
  :: forall s . Int
  -> MVector s Position
  -> MVector s Position
  -> ST s ()
updateAll !r oldMV newMV = do
  let !len  = Mutable.length oldMV
  traverse_ (update r oldMV newMV) [Point i j | i <- [0..(div len r) - 1], j <- [0..(r - 1)]]

update
  :: forall s .
     Int 
  -> MVector s Position
  -> MVector s Position
  -> Point
  -> ST s ()
update !r !oldMV !newMV !ind = do
  !posM <- index r ind oldMV
  case posM of
--    Nothing -> error "update: internal index error"
    ~(Just pos) ->
      case pos of
        Empty -> do
          !allUnoccupied <- foldrM (unoccupied ind) mempty dirs
          if getAll allUnoccupied
            then Mutable.unsafeWrite newMV vInd Occupied
            else pure ()
        Occupied -> do
          someOccupied <- (>= 4) . Vector.length <$> Vector.filterM (occupied ind) dirs
          if someOccupied
            then Mutable.unsafeWrite newMV vInd Empty >> pure ()
            else pure ()
        _ -> pure ()
  where
    !vInd = computeIndex r ind
    unoccupied :: Point -> Point -> All -> ST s All
    unoccupied !p !dir allB = do
      !pos <- index r (addP p dir) oldMV
      pure (All (pos /= Just Occupied) <> allB)

    occupied :: Point -> Point -> ST s Bool
    occupied !p !dir = do
      !pos <- index r (addP p dir) oldMV
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
  let inds = [Point i j | i <- [0..(div len r) - 1], j <- [0..(r - 1)]]
  traverse_ (update2 r oldMV newMV) inds

update2
  :: forall s .
     Int 
  -> MVector s Position
  -> MVector s Position
  -> Point
  -> ST s ()
update2 r oldMV newMV ind = do
  posM <- index r ind oldMV
  case posM of
--    Nothing -> error "update: internal index error"
    Just pos ->
      case pos of
        Empty -> do
          allUnoccupied <- foldrM (unoccupied ind) mempty dirs
          if getAll allUnoccupied
            then Mutable.write newMV vInd Occupied
            else pure ()
        Occupied -> do
          someOccupied <- (>= 5) . Vector.length <$> Vector.filterM (occupied ind) dirs
          if someOccupied
            then Mutable.write newMV vInd Empty >> pure ()
            else pure ()
        _ -> pure ()
  where
    vInd = computeIndex r ind
    unoccupied :: Point -> Point -> All -> ST s All
    unoccupied p dir allB = do
      occ <- occupiedRay r (addP p dir) dir oldMV
      pure (All (not occ) <> allB)

    occupied :: Point -> Point -> ST s Bool
    occupied p dir = occupiedRay r (addP p dir) dir oldMV


occupiedRay :: Int -> Point -> Point -> MVector s Position -> ST s Bool
occupiedRay r p dir mv = do
  pos <- index r p mv
  case pos of
    Nothing    -> pure False
    Just Floor -> occupiedRay r (addP p dir) dir mv
    Just seat  -> pure (seat == Occupied)
    
      
addP :: Point -> Point -> Point
addP (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)


dirs :: Vector Point
dirs = Vector.fromList [Point i j | i <- [-1..1], j <- [-1..1], (i, j) /= (0,0)]

foldrM :: forall a b m . (Vector.Unbox a, Monad m) => (a -> b -> m b) -> b -> Vector a -> m b
foldrM f z0 xs = Vector.foldr c (pure z0) xs
  -- See Note [List fusion and continuations in 'c']
  where
    c :: a -> m b -> m b
    c a mb = mb >>= f a
    {-# INLINE c #-}
