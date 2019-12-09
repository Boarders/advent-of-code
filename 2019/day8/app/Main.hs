{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Data.Char       as Char
import qualified Data.List.Extra as List
import           Data.Text       (Text)
import qualified Data.Text       as Text
import qualified Data.Text.IO    as IO

main :: IO ()
main =
  do
    inputText <- IO.readFile "input.txt"
    let layers = parseInput 25 6 inputText
    putStrLn "puzzle1 :"
    putStrLn $ (replicate 25 ' ') <> (show . puzzle1 $ layers)
    putStrLn ""
    putStrLn "puzzle2 :"
    putStrLn ""
    putStrLn $ puzzle2 25 layers


puzzle1 :: [[Int]] -> Int
puzzle1 =  uncurry (*)
         . foldr (\x ~(ones, twos) ->
                    case x of
                      1 -> (ones + 1, twos    )
                      2 -> (ones    , twos + 1)
                      _ -> (ones    , twos    )
                 ) (0,0)
         . List.minimumOn (length . filter (== 0))


puzzle2 :: Int -> [[Int]] -> String
puzzle2 width = displayPixels width . getPixels


parseInput :: Int -> Int -> Text -> [[Int]]
parseInput width height inputText =
    let
      textDigits = Text.filter Char.isDigit inputText
      layers = Text.chunksOf (width * height) textDigits
      toInts = fmap Char.digitToInt . Text.unpack
    in
      fmap toInts layers


determinePixel :: [Int] -> Bool
determinePixel = (== 0) . head . filter (/= 2)


getPixels :: [[Int]] -> [Bool]
getPixels = fmap determinePixel . List.transpose


displayPixels :: Int -> [Bool] -> String
displayPixels width = unlines . fmap ((replicate 25 ' ') <>) . fmap renderRow . rows
  where
    rows = List.chunksOf width
    renderRow = foldr (\x acc ->
                         case x of
                           False -> '☐' : acc
                           True  -> ' ' : acc
                      ) mempty
