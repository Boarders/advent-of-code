module Main where

import qualified Criterion.Main                             as C (bench, bgroup,
                                                                  defaultMain,
                                                                  env, nf, nfIO)
import qualified Data.ByteString                            as ByteString
import Day1


getInput :: IO [Int]
getInput = do
  bs <- ByteString.readFile "input/day1.dat"
  let is = parseInput bs
  pure is

main :: IO ()
main = do
  C.defaultMain . pure $
    C.env getInput $ \ ~is ->
      C.bgroup "day1"
        [ C.bench "naive"    $ C.nf sum_2020 is
        , C.bench "intSet"   $ C.nf s1 is
        , C.bench "naive2"   $ C.nf sum3_2020 is
        , C.bench "intSet2"  $ C.nf s2 is
        , C.bench "solution" $ C.nfIO day1
        ]
