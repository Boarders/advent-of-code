{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
module Main where

import Data.Text.IO as IO
import Data.Text hiding (foldr, minimum, foldl', zip)
import           Text.Megaparsec (Parsec, chunk, between)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (signed, decimal, lexeme)
import Data.Functor (void)
import Data.Void (Void)


main :: IO ()
main =
  do
    textInput <- IO.readFile "input.txt"
    let point = parseInput "<x=-9, y=10, z=-1>"
    print point
--    print $ puzzle1 wires
--    print $ puzzle2 wires


puzzle1 :: Moons
puzzle1 = error "to do"

puzzle2 :: Moons
puzzle2 = error "to do"

data Moons = Moon


data State = State
  { position :: {-# UNPACK #-} !Point
  , velocity :: {-# UNPACK #-} !Point
  }

initialState :: Point -> State
initialState 

data Point = Point
  { x :: {-# UNPACK #-} !Sum Int
  , y :: {-# UNPACK #-} !Sum Int
  , z :: {-# UNPACK #-} !Sum Int
  }
  deriving (Eq, Show)

instance Semigroup 



-- parse input

parseInput :: Text -> Point
parseInput input =
  do
    case Megaparsec.runParser parsePoint "input.txt" input of
      Left err    -> error $ Megaparsec.errorBundlePretty err
      Right point -> point

type Parser = Parsec Void Text


parsePoint :: Parser (Point)
parsePoint =
  between (char '<') (char '>') $
    do
      x <- lexeme space $ chunk "x=" *> signed space decimal <* char ','
      y <- lexeme space $ chunk "y=" *> signed space decimal <* char ','
      z <- lexeme space $ chunk "z=" *> signed space decimal
      pure (Point x y z)

parsePositions :: 
