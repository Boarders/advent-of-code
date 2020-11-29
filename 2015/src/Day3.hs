{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
module Day3 where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(peek))
import Common (solutions)
import Data.Word (Word8)
import GHC.Base (unsafeChr)

day3 :: IO ()
day3 = do
  bs <- ByteString.readFile "input/day3.dat"
  let b = "^>v><"
  let
    sol1 = length (runGrid1 bs)
    sol2 = (totalVisited . runGrid2) bs
  putStrLn $ (solutions 2 sol1 sol2)


data Point = Point
  { x :: !Int
  , y :: !Int
  } deriving (Show, Eq, Generic)

instance Hashable Point

type Grid = HashSet Point
data State = State
  { pos  :: !Point
  , grid :: !Grid
  }

initialState :: State
initialState = State origin (HashSet.singleton origin)
  where
    origin = Point 0 0

addToGrid :: State -> Char -> State
addToGrid (State (Point x y) grid) = \case
  '>' -> let newPos = Point (x + 1) y in State newPos (HashSet.insert newPos grid)
  '<' -> let newPos = Point (x - 1) y in State newPos (HashSet.insert newPos grid)
  '^' -> let newPos = Point x (y + 1) in State newPos (HashSet.insert newPos grid)
  'v' -> let newPos = Point x (y - 1) in State newPos (HashSet.insert newPos grid)
  c   -> error $ "unexpected input: " <> [c]

runGrid1 :: ByteString -> Grid
runGrid1 = grid . (ByteString.foldl' addToGrid initialState)

runGrid2 :: ByteString -> (Grid, Grid)
runGrid2 = (\ ~(s1, s2) -> (grid s1, grid s2)) . (altFoldl' addToGrid initialState)

totalVisited :: (Grid, Grid) -> Int
totalVisited (g1, g2) = length (HashSet.union g1 g2)

altFoldl' :: (a -> Char -> a) -> a -> ByteString -> (a, a)
altFoldl' f v (PS fp off len) =
      accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
        let
          !end  =  p  `plusPtr` (off + len)
          !end' = end `plusPtr` (- 1)
        in
          go (v, v) (p `plusPtr` off) end end'
    where
      -- tail recursive; traverses array left to right
      go z@(!a1, !a2) !p !q !q' | p == q  = return z
                                | p == q' =
                                    do xf <- peek p
                                       return (f a1 (w2c xf), a2)
                                | otherwise =
                                    do x <- peek p
                                       y <- peek (p `plusPtr` 1)
                                       go (f a1 (w2c x), f a2 (w2c y)) (p `plusPtr` 2) q q'

w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}
