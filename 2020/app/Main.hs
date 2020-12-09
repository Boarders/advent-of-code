module Main where

--import Day8

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6

import Day7
import Day8
import Day9

import Control.Concurrent.ParallelIO.Global
import Control.Monad
import Common (solutions)

{-
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
--}

main :: IO ()
main = do

--An attempt was made
  rs <- parallel
          [ day1'
          , day2'
          , day3'
          , day4'
          , day5'
          , day6'
          , day7'
          , day8'
          , day9'
          ]
  zipWithM_ (\n -> uncurry (solutions n)) [1..] rs

--}
