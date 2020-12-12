{-# language OverloadedStrings #-}
{-# language LambdaCase        #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language BangPatterns #-}
{-# language PatternSynonyms #-}
--{-# language Strict #-}
module Day11m where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Common (solutions)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as Mutable
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
import qualified Data.Massiv.Array as Massiv

import qualified Data.Massiv.Array as Massiv
import Data.Massiv.Array (Ix2(..), Sz(..), U(..), DW, Value(..))
import Control.Applicative (liftA2)

import Debug.Trace (traceShowId)


data Position =
    Empty
  | Occupied
  | Floor
  deriving (Eq, Generic, Show)



{-# inline fromW8 #-}
fromW8 :: Word8 -> Position
fromW8 n | n == 0 = Empty
         | n == 1 = Occupied
         | n == 2 = Floor
         | otherwise = errorWithoutStackTrace
             $ "Enum.fromW8: bad argument " <> (show n)

{-# inline toW8 #-}
toW8 :: Position -> Word8
toW8 = \case
    Empty    -> 0
    Occupied -> 1
    Floor    -> 2

data Point = Point
  { xCoord :: !Int
  , yCoord :: !Int
  }
  deriving (Eq)

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
    [t|  Position -> Word8 |]
    [| toW8     |]
    [| fromW8   |]

derivingUnbox "Point"
    [t|  Point -> (Int, Int)     |]
    [| \ (Point x y) ->  (x,y)   |]
    [| \ (x, y) ->  Point x y    |]

rowsP :: Massiv.Array U Ix2 Position -> Text.Text
rowsP v =
    Text.unlines
  . Massiv.toList
  . Massiv.map (Text.center (10 + 30) ' ')
  . Massiv.map TextBuilder.run  
  . Massiv.foldlInner (\acc x -> acc <> (TextBuilder.char . asciiPos $ x)) mempty
  $ v

rowsToText :: Int -> Vector Position -> Text.Text
rowsToText r v =
    Text.unlines
  . fmap (Text.center (r + 30) ' ')
  . Text.chunksOf r
  . TextBuilder.run
  . Vector.foldl' (\acc x -> acc <> (TextBuilder.char . asciiPos $ x)) mempty
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
    sol2 = s1 input
  print input
  Text.putStrLn $ rowsP input
  Text.putStrLn $ rowsP (update input)
--  solutions 11 sol1 sol2
      
parseInput :: IO (Massiv.Array U Ix2 Position)
parseInput = do
  bs <- ByteString.readFile "input/day11.dat"
  let bs = test
  let
    (!rowS, rowB) = (ByteString.length . head) &&& (foldMap buildRow) $ (ByteString.lines bs)
    mArr =
        (\ ~(v,l) -> Massiv.resize' (Massiv.Sz2 rowS (l `div` rowS)) v)
      . (Massiv.fromUnboxedVector &&& Vector.length)
      . Builder.build
      $ rowB
  pure $ mArr

buildRow :: ByteString -> Builder.Builder Position
buildRow bs = ByteString.foldl' fromEntry mempty bs
  where
    fromEntry :: Builder.Builder Position -> Char -> Builder.Builder Position
    fromEntry acc = \case
      'L' -> acc <> Builder.singleton Empty
      '.' -> acc <> Builder.singleton Floor
      '#' -> acc <> Builder.singleton Occupied
      c   -> error $ "buildRow: unexpected character: " <> [c] 

s1 :: Massiv.Array U Ix2 Position -> Int
s1 = countOccupied . runGrid

countOccupied ::
  Massiv.Array U Ix2 Position -> Int
countOccupied =
  Massiv.foldlS
  (\acc pos -> acc + (fromEnum (pos == Occupied)))
  0

  
runGrid :: Massiv.Array U Ix2 Position -> Massiv.Array U Ix2 Position
runGrid !inputV = runST $ do
  undefined
{-  
  !oldMV <- Vector.thaw inputV
  !newMV <- Mutable.clone oldMV
  updateUntilEq r oldMV newMV
  Vector.freeze newMV
-}

updateUntilEq
  :: Massiv.Array U  Ix2 Position
  -> Massiv.Array U Ix2 Position
updateUntilEq = 
  Massiv.iterateUntil
  (\i old new -> old == new)
  (const update)
  

update :: Massiv.Array U Ix2 Position -> Massiv.Array U Ix2 Position
update =
  Massiv.computeAs U . Massiv.mapStencil border updateStencil

border :: Massiv.Border Position
border = Massiv.Fill Floor

updateStencil :: Massiv.Stencil Ix2 Position Position
updateStencil =
  Massiv.makeStencilDef Floor (Sz (3 :. 3)) (0 :. 0) $ \ get ->
    let
      chair   = get (0 :. 0)
      isOcc c = fromEnum . (== Occupied) <$> (get c)
      isChair = not . (== Floor) <$> chair
      occ = 
        (  isOcc (-1 :. -1) + isOcc (-1 :. 0) + isOcc (-1 :. 1) +
           isOcc ( 0 :. -1)                   + isOcc ( 0 :. 1) +
           isOcc ( 1 :. -1) + isOcc ( 1 :. 0) + isOcc ( 1 :. 1)
        )
    in
      liftA2 threshold chair occ
{-# inline updateStencil #-}

        
threshold :: Position -> Int -> Position
threshold p n | p == Floor              = p
              | n == 0                  = Occupied
              | n >= 4 && p == Occupied = Empty



{-
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

{-
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
--}
--}

test =
  ByteString.unlines
  [ "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"
  ]
