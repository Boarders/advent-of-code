{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
module Main where

import Data.Text.IO as IO
import Data.Text hiding (foldr, minimum, foldl', zip)
import Text.Megaparsec hiding (sepEndBy1)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import Data.Functor (void)
import Data.Map as M hiding (foldl', toList)
import Data.Foldable


main :: IO ()
main =
  do
    wires <- parseInput
    print $ puzzle1 wires
    print $ puzzle2 wires


data Point = Point
  { _x :: {-# UNPACK #-} !Int
  , _y :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show, Ord)

-- Points are a torsor for directions.
act :: Dir -> Point -> Point
act dir (Point x y) =
  case dir of
    R n -> Point (x + n) y
    L n -> Point (x - n) y
    U n -> Point x (y + n)
    D n -> Point x (y - n)


instance Semigroup Point where
  (<>) (Point x0 y0) (Point x1 y1) = Point (x0 + x1) (y0 + y1)

instance Monoid Point where
  mempty = Point 0 0

manhattan :: Point -> Point -> Int
manhattan (Point x1 y1) (Point x2 y2)
  = abs (x2 - x1) + abs (y2 - y1)

norm :: Point -> Int
norm = manhattan mempty


data Journey = Journey
  { destination  :: {-# UNPACK #-} !Point
  , stepsTaken   :: {-# UNPACK #-} !Int
  , visited      :: !(Map Point Int)
  }
  deriving (Eq, Show)

emptyJourney :: Journey
emptyJourney = Journey mempty 0 mempty

journey  :: [Dir] -> Journey
journey =
    foldl'
      (\Journey{..} d
         -> Journey
              (act d destination)
              (stepsTaken + getLength d)
              (visited <> addSegment stepsTaken destination d)
      )
      emptyJourney
  where
    addSegment :: Int -> Point -> Dir -> Map Point Int
    addSegment steps (Point x0 y0) =
      \case
        R n ->
            fromList$ zip
                      [ (Point xi y0)
                      | xi <- [(x0 + 1) .. (x0 + n)]
                      ]
                      [steps + 1 .. steps + n]
        L n ->
            fromList $ zip
                       [ (Point xi y0)
                       | xi <- enumFromThenTo (x0 - 1) (x0 - 2) (x0 - n)
                       ]
                       [steps + 1 .. steps + n]
        U n ->
            fromList $ zip
                       [ (Point x0 yi)
                       | yi <- [(y0 + 1) .. (y0 + n)]
                       ]
                       [steps + 1 .. steps + n]
        D n ->
            fromList $ zip
                       [ (Point x0 yi)
                       | yi <- enumFromThenTo (y0 - 1) (y0 - 2) (y0 - n)
                       ]
                       [steps + 1 .. steps + n]


getIntersection :: ([Dir], [Dir]) -> Map Point Int
getIntersection (wire1, wire2) =
  M.intersectionWith (+)
  (visited . journey $ wire1) (visited . journey $ wire2)


puzzle1 :: ([Dir], [Dir]) -> Int
puzzle1 = minimum . fmap norm . M.keys . getIntersection

puzzle2 :: ([Dir], [Dir]) -> Int
puzzle2 = minimum . getIntersection

-- parse input

parseInput :: IO ([Dir], [Dir])
parseInput =
  do
    input <- IO.readFile "input3.txt"
    case runParser parseWires "input3.txt" input of
      Left err -> error $ errorBundlePretty err
      Right dirs -> pure dirs

type Parser = Parsec Void Text

parseWires :: Parser ([Dir], [Dir])
parseWires =
  do
    dirs1 <- parseDirs
    void (char '\n')
    dirs2 <- parseDirs
    pure (dirs1, dirs2)
  

parseDirs :: Parser [Dir]
parseDirs = sepBy parseDir (char ',')

parseDir :: Parser Dir
parseDir =
  choice
    [ R <$> (char 'R' *> decimal)
    , L <$> (char 'L' *> decimal)
    , U <$> (char 'U' *> decimal)
    , D <$> (char 'D' *> decimal)
    ]

data Dir where
  R :: Int -> Dir
  L :: Int -> Dir
  U :: Int -> Dir
  D :: Int -> Dir
  deriving Show

getLength :: Dir -> Int
getLength =
  \case
    R n -> n
    L n -> n
    U n -> n
    D n -> n
