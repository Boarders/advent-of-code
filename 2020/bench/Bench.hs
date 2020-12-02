{-# language BlockArguments #-}
module Main where

import qualified Criterion.Main                             as C (bench, bgroup,
                                                                  defaultMain,
                                                                  env, nf, nfIO)
import qualified Data.ByteString                            as ByteString
import Day1
import qualified Day2 as Day2


getDay1 :: IO [Int]
getDay1 = do
  bs <- ByteString.readFile "input/day1.dat"
  let is = parseInput bs
  pure is





main :: IO ()
main = do
  C.defaultMain . pure $
    C.bgroup "advent of code"
    [ C.env getDay1 $ \ ~d1 ->
         C.bgroup "day1"
        [ C.bench "sol1"   $ C.nf s1 d1
        --, C.bench "naive"    $ C.nf sum_2020 day1        
        --, C.bench "naive2"   $ C.nf sum3_2020 day1
        , C.bench "sol2"  $ C.nf s2 d1
        ]
    , C.env Day2.d2 $ \ ~d2 ->
        C.bgroup "day2"
       [ C.bench "sol1"    $ C.nf (length . filter Day2.s1 <$>) d2
       , C.bench "sol2"    $ C.nf (length . filter Day2.s2 <$>) d2
       ]
    ]
