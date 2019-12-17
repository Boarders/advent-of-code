{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text.IO               as IO
import qualified Data.Vector.Unboxed        as Unboxed
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, chunk)
import qualified Text.Megaparsec            as Megaparsec
import           Text.Megaparsec.Char       (char, space)
import           Text.Megaparsec.Char.Lexer (decimal, lexeme, signed)

import           Points

main :: IO ()
main =
  do
    textInput <- IO.readFile "input.txt"
    let points = parseInput textInput
    putStrLn "puzzle 1:"
    putStrLn $ replicate 30 ' ' <> (show . puzzle1 $ points)
    putStrLn "puzzle 2:"
    putStrLn $ replicate 30 ' ' <> (show . puzzle2 $ points)


puzzle1 :: Unboxed.Vector Point -> Int
puzzle1 points = totalEnergy (runSystem pointDir points 1000)


puzzle2 :: Unboxed.Vector Point -> Int
puzzle2 points =
  let
    xCoords, yCoords, zCoords :: Unboxed.Vector Int
    xCoords = Unboxed.map x points
    yCoords = Unboxed.map y points
    zCoords = Unboxed.map z points
  in
    lcm
      (findRepeat forceDirection xCoords)
      (lcm
        (findRepeat forceDirection yCoords)
        (findRepeat forceDirection zCoords)
      )




---------------
--- naughty! --
---------------
instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0


-----------------------
-- Main functionality--
-----------------------

-- |
-- Takes a function for updating the velocity between points and
-- an input vector and runs the system the given number of steps.
runSystem
  :: forall a . (Unboxed.Unbox a, Monoid a)
  => (a -> a -> a) -> Vector a -> Int -> System a
runSystem fn = step . initialiseSystem
  where
    step :: System a -> Int -> System a
    step system 0     = system
    step System{..} n =
        let
          zipper    = initialZipper position
          velocity' = Unboxed.zipWith (<>) velocity (stepFrom fn zipper)
          position' = Unboxed.zipWith (<>) position velocity'
          system'   = System position' velocity'
        in
          step system' (n - 1)

-- |
-- Takes a function for comparison and a vector of inputs and runs the system
-- until it finds a repeat.
findRepeat
  :: forall a . (Unboxed.Unbox a, Monoid a, Ord a) => (a -> a -> a) -> Vector a -> Int
findRepeat fn input = step (initialiseSystem input) mempty
  where
    step :: System a -> Set.Set (System a) -> Int
    step sys@System{..} seen =
      case Set.member sys seen of
        False ->
          let
            zipper    = initialZipper position
            velocity' = Unboxed.zipWith (<>) velocity (stepFrom fn zipper)
            position' = Unboxed.zipWith (<>) position velocity'
            system'   = System position' velocity'
            seen'     = Set.insert sys seen
          in
            step system' seen'
        True  -> length seen


-- |
-- This computes the new direction to move using the given function
stepFrom :: forall a . (Unboxed.Unbox a, Monoid a) => (a -> a -> a) -> Zipper a -> Vector a
stepFrom fn = elements . extend fromContext
  where
    fromContext :: Zipper a -> a
    fromContext z = Unboxed.foldr (\pt acc -> fn (extract z) pt <> acc) mempty (elements z)


-- |
-- Which direction a point should move
pointDir :: Point -> Point -> Point
pointDir (Point x0 y0 z0) (Point x1 y1 z1) =
  Point (forceDirection x0 x1) (forceDirection y0 y1) (forceDirection z0 z1)

-- |
-- Whether to move to the left or right
forceDirection :: Int -> Int -> Int
forceDirection n m =
  case compare n m of
    LT ->  1
    GT -> -1
    _  ->  0


--------------------
-- System utility --
--------------------
data System a = System
  { position :: !(Unboxed.Vector a)
  , velocity :: !(Unboxed.Vector a)
  }
  deriving (Eq, Ord)


initialiseSystem :: (Unboxed.Unbox a, Monoid a) => Unboxed.Vector a -> System a
initialiseSystem position =
  let
    velocity = Unboxed.replicate 4 mempty
  in
    System {..}

totalEnergy :: System Point -> Int
totalEnergy System{..} =
  Unboxed.sum $
  Unboxed.zipWith (*)
  (Unboxed.map absSum position)
  (Unboxed.map absSum velocity)


instance (Show a, Unboxed.Unbox a) => Show (System a) where
  show System {..} =
    unlines
      [ unlines . fmap (("pos="<>) . show) . Unboxed.toList $ position
      , unlines . fmap (("vel="<>) . show) . Unboxed.toList $ velocity
      ]

-------------
-- Parsing --
-------------
parseInput :: Text -> Unboxed.Vector Point
parseInput input =
  case Megaparsec.runParser parsePositions "input.txt" input of
    Left err    -> error $ Megaparsec.errorBundlePretty err
    Right point -> point

type Parser = Parsec Void Text


parsePoint :: Parser Point
parsePoint =
  between (char '<') (char '>') $
    do
      x <- lexeme space $ chunk "x=" *> signed space decimal <* char ','
      y <- lexeme space $ chunk "y=" *> signed space decimal <* char ','
      z <- lexeme space $ chunk "z=" *> signed space decimal
      pure (Point x y z)


parsePositions :: Parser (Unboxed.Vector Point)
parsePositions =
  do
    io       <- lexeme space parsePoint
    europa   <- lexeme space parsePoint
    ganymede <- lexeme space parsePoint
    callisto <- lexeme space parsePoint
    pure $ Unboxed.fromList [io, europa, ganymede, callisto]


----------------------------
-- zipper / comonad stuff --
----------------------------

data Zipper a = Zipper
  { elements :: Unboxed.Vector a
  , focus    :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- |
-- Initialise a zipper with a focus on the head.
initialZipper :: (Unboxed.Unbox a) => Unboxed.Vector a -> Zipper a
initialZipper v = Zipper v 0


-- |
-- Like extract for a comonad but with an Unbox constraint.
extract :: (Unboxed.Unbox a) => Zipper a -> a
extract (Zipper v i) = v Unboxed.! i

-- |
-- Like extend for a comonad but with an Unbox constraint.
extend :: (Unboxed.Unbox a, Unboxed.Unbox b) => (Zipper a -> b) -> Zipper a -> Zipper b
extend f (Zipper v i) =
  let n = Unboxed.length v in
    Zipper (Unboxed.generate n (f . Zipper v)) i
