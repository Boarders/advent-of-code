{-# language BlockArguments #-}
module Main where

import qualified Criterion.Main as Criterion
import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import qualified Day4 as Day4
import qualified Day5 as Day5


day1, day2, day3, day4, day5 :: Criterion.Benchmark
day1 = Criterion.env Day1.parseInput $ \ ~d ->
  Criterion.bgroup "day1"
    [ Criterion.bench "sol1"   $ Criterion.nf Day1.s1 d
    , Criterion.bench "sol2"  $ Criterion.nf Day1.s2 d
    ]

day2 = Criterion.env Day2.parseInput $ \ ~d ->
  Criterion.bgroup "day2"
    [ Criterion.bench "sol1"    $ Criterion.nf Day2.s1 d
    , Criterion.bench "sol2"    $ Criterion.nf Day2.s2 d
    ]

day3 = Criterion.env Day3.parseInput $ \ ~d ->
  Criterion.bgroup "day3"
    [ Criterion.bench "sol1"    $ Criterion.nf Day3.s1 d
    , Criterion.bench "sol2"    $ Criterion.nf Day3.s2 d
    ]

day4 = Criterion.env Day4.parseInput $ \ ~d ->
  Criterion.bgroup "day4"
    [ Criterion.bench "sol1"    $ Criterion.nf Day4.s1 d
    , Criterion.bench "sol2"    $ Criterion.nf Day4.s2 d
    ]

day5 = Criterion.env Day5.parseInput $ \ ~d ->
  Criterion.bgroup "day5"
    [ Criterion.bench "sol1"    $ Criterion.nf Day5.s1 d
    , Criterion.bench "sol2"    $ Criterion.nf Day5.s2 d
    ]

main :: IO ()
main = do
  Criterion.defaultMain . pure $
    Criterion.bgroup "advent of code"
    [ day5
--      day1
--    , day2
--    , day3
--    , day4
--    , day5
    ]
