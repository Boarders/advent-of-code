{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
module Day2 where

import Common
import Data.Bytes.Parser
import Data.Bytes.Parser.Ascii
import Data.Bytes.Parser.Latin
import Control.Applicative
import Data.List (foldl')

day2 :: IO ()
day2 = do
  ba <- readFileBA "input/day2.dat"
  let i = parseByteArray parse ba
  case i of
    Failure e -> putStrLn e
    Success (Slice _ _ res) ->
      let
        Pair sol1 sol2 = runSol res
      in
        putStrLn $ unlines (solutions 2 sol1 sol2)

data Present = Present
  { smol :: !Int
  , mid  :: !Int
  , big  :: !Int
  }
  deriving (Show)

data Pair a b = Pair !a !b

sort3 :: (Ord a) => (a,a,a) -> (a,a,a)
sort3 (a,b,c) =
  case ( a < b, b < c, a < c ) of
    ( True , True , _     ) -> (a,b,c)
    ( True , False, True  ) -> (a,c,b)
    ( True , False, False ) -> (c,a,b)
    ( False, True , True  ) -> (b,a,c)
    ( False, False, False ) -> (b,c,a)
    ( False, False , _    ) -> (c,b,a)
    _ -> error "Pattern matches ARE exhaustive actually"

present :: Int -> Int -> Int -> Present
present l w h =
  let (s, m, b) = sort3 (l, w, h) in
    Present s m b

parse ::  Parser String s [Present]
parse = some $ do
  i1 <- decStandardInt "int1"
  char "" 'x'
  i2 <- decStandardInt "int2"
  char "" 'x'
  i3 <- decStandardInt "int3"
  skipChar '\n'
  pure (present i1 i2 i3)

wrapping :: Present -> Int
wrapping (Present s m b) = 3 * s * m + 2 * (m * b + b * s)

allWrapping :: [Present] -> Int
allWrapping = foldl' (\acc p -> acc + wrapping p) 0

ribbon :: Present -> Int
ribbon (Present s m b) = 2 * s + 2 * m * ( s +  b)

allRibbon :: [Present] -> Int
allRibbon = foldl' (\acc p -> acc + ribbon p) 0

runSol :: [Present] -> Pair Int Int
runSol =  foldl' (\ ~(Pair f s) p -> Pair (f + wrapping p) (s + ribbon p)) (Pair 0 0)
