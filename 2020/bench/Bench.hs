{-# language BlockArguments #-}
module Main where

import qualified Criterion.Main                             as Criterion
  (bench, bgroup, defaultMain, env, nf)
import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3


main :: IO ()
main = do
  Criterion.defaultMain . pure $
    Criterion.bgroup "advent of code"
    [ Criterion.env Day1.parseInput $ \ ~d1 ->
         Criterion.bgroup "day1"
        [ Criterion.bench "sol1"   $ Criterion.nf Day1.s1 d1
        , Criterion.bench "sol2"  $ Criterion.nf Day1.s2 d1
        ]
    , Criterion.env Day2.parseInput $ \ ~d2 ->
        Criterion.bgroup "day2"
       [ Criterion.bench "sol1"    $ Criterion.nf Day2.s1 d2
       , Criterion.bench "sol2"    $ Criterion.nf Day2.s2 d2
       ]
    , Criterion.env Day3.parseInput $ \ ~d3 ->
        Criterion.bgroup "day3"
       [ Criterion.bench "sol1"    $ Criterion.nf Day3.s1 d3
       , Criterion.bench "sol2"    $ Criterion.nf Day3.s2 d3
       ]       
    ]
