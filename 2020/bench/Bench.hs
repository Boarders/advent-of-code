{-# language BlockArguments #-}
module Main where

import qualified Criterion.Main                             as C (bench, bgroup,
                                                                  defaultMain,
                                                                  env, nf)
import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3


main :: IO ()
main = do
  C.defaultMain . pure $
    C.bgroup "advent of code"
    [ C.env Day1.parseInput $ \ ~d1 ->
         C.bgroup "day1"
        [ C.bench "sol1"   $ C.nf Day1.s1 d1
        , C.bench "sol2"  $ C.nf Day1.s2 d1
        ]
    , C.env Day2.parseInput $ \ ~d2 ->
        C.bgroup "day2"
       [ C.bench "sol1"    $ C.nf Day2.s1 d2
       , C.bench "sol2"    $ C.nf Day2.s2 d2
       ]
    , C.env Day3.parseInput $ \ ~d3 ->
        C.bgroup "day3"
       [ C.bench "sol1"    $ C.nf Day3.s1 d3
       , C.bench "sol2"    $ C.nf Day3.s2 d3
       ]       
    ]
