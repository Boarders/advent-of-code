{-# language BlockArguments #-}
module Main where

import qualified Criterion.Main as Criterion
import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import qualified Day4 as Day4
import qualified Day5 as Day5
import qualified Day6 as Day6
import qualified Day7 as Day7
import qualified Day8 as Day8
import qualified Day9 as Day9
import qualified Day10 as Day10




day1, day2, day3, day4, day5, day6, day7, day8, day9, day10 :: Criterion.Benchmark
day1 = Criterion.env Day1.parseInput $ \ ~d ->
  Criterion.bgroup "day1"
    [ Criterion.bench "sol1" $ Criterion.nf Day1.s1 d
    , Criterion.bench "sol2" $ Criterion.nf Day1.s2 d
    , Criterion.bench "tot"  $ Criterion.nfIO Day1.day1'
    ]

day2 = Criterion.env Day2.parseInput $ \ ~d ->
  Criterion.bgroup "day2"
    [ Criterion.bench "sol1" $ Criterion.nf Day2.s1 d
    , Criterion.bench "sol2" $ Criterion.nf Day2.s2 d
    , Criterion.bench "tot"  $ Criterion.nfIO Day2.day2'
    ]

day3 = Criterion.env Day3.parseInput $ \ ~d ->
  Criterion.bgroup "day3"
    [ Criterion.bench "sol1" $ Criterion.nf Day3.s1 d
    , Criterion.bench "sol2" $ Criterion.nf Day3.s2 d
    , Criterion.bench "tot"  $ Criterion.nfIO Day3.day3'
    ]

day4 = Criterion.env Day4.parseInput $ \ ~d ->
  Criterion.bgroup "day4"
    [ Criterion.bench "sol1"    $ Criterion.nf Day4.s1 d
    , Criterion.bench "sol2"    $ Criterion.nf Day4.s2 d
    , Criterion.bench "tot"  $ Criterion.nfIO Day4.day4'    
    ]

day5 = Criterion.env Day5.parseInput $ \ ~d ->
  Criterion.bgroup "day5"
    [ Criterion.bench "sol1"    $ Criterion.nf Day5.s1 d
    , Criterion.bench "sol2"    $ Criterion.nf Day5.s2 d
    , Criterion.bench "tot"  $ Criterion.nfIO Day5.day5'        
    ]


day6 = Criterion.env Day6.parseInput $ \ ~d ->
  Criterion.bgroup "day6"
    [ Criterion.bench "sol1"    $ Criterion.nf Day6.s1 d
    , Criterion.bench "sol2"    $ Criterion.nf Day6.s2 d
    , Criterion.bench "tot"    $ Criterion.nfIO Day6.day6'
    ]

day7 = Criterion.env Day7.parseInput $ \ ~d ->
  Criterion.bgroup "day7"
    [ Criterion.bench "sol1"    $ Criterion.nf Day7.s1 d
    , Criterion.bench "sol2"    $ Criterion.nf Day7.s2 d
    , Criterion.bench "tot"    $ Criterion.nfIO Day7.day7'
    ]

day8 = Criterion.env Day8.parseInput $ \ d ->
  Criterion.bgroup "day8"
    [
      Criterion.bench "sol1" $ Criterion.nf Day8.s1 d
    , Criterion.bench "sol2" $ Criterion.nf Day8.s2 d
    , Criterion.bench "tot"  $ Criterion.nfIO Day8.day8'
    ]

day9 = Criterion.env Day9.parseInput $ \ d ->
  Criterion.bgroup "day9"
    [
      Criterion.bench "sol1" $ Criterion.nf Day9.s1 d
    , Criterion.bench "sol2" $ Criterion.nf (Day9.s2 257342611) d
    , Criterion.bench "tot"  $ Criterion.nfIO Day9.day9'
    
    ]

day10 = Criterion.env Day10.parseInput $ \ d ->
  Criterion.bgroup "day10"
    [
      Criterion.bench "sol1" $ Criterion.nf Day10.s1 d
    , Criterion.bench "sol2" $ Criterion.nf Day10.s2 d
    , Criterion.bench "tot"  $ Criterion.nfIO Day10.day10'
    ]        

days :: [Criterion.Benchmark]
days = [day1, day2, day3, day4, day5, day6, day7,day8, day9]
main :: IO ()
main = do
  Criterion.defaultMain . pure $
    Criterion.bgroup "advent of code"
    [ day10 ]

