{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
module Day5 where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Common (solutions)

day5 :: IO ()
day5 = do
  bs <- ByteString.readFile "input/day5.dat"
  let
    sol1 = length . filter nice  $ ByteString.lines bs
    sol2 = length . filter nice2 $ ByteString.lines bs
  putStrLn $ (solutions 2 sol1 sol2)


data State1 = State1
  { vowels        :: !Int
  , lastLetter    :: !Char
  , hasDouble     :: !Bool
  , hasDisallowed :: !Bool
  }
  deriving (Show)
  

disallowed :: HashSet String
disallowed =
  HashSet.fromList ["ab", "cd", "pq", "xy"]

isVowel :: Char -> Bool
isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'


accumulator1 :: State1 -> Char -> State1
accumulator1 (State1 v ll db dis) c =
  case (isVowel c, ll == c, ([ll , c]) `HashSet.member` disallowed) of
    (True , True , True ) ->
      State1 (v + 1) c True True
    (True , False, True ) ->
      State1 (v + 1) c db True    
    (True , True , False) ->
      State1 (v + 1) c True dis    
    (True , False, False) ->
      State1 (v + 1) c db dis   
    (False, True , True ) ->
      State1 v c True True    
    (False, False, True ) ->
      State1 v c db True    
    (False, True , False) ->
      State1 v c True dis 
    (False, False, False) ->
      State1 v c db dis   

finalState :: State1 -> Bool
finalState (State1 v _ b1 b2) =
   (v >= 3) && b1 && (not b2)

nice :: ByteString -> Bool
nice bs =
  let
    Just (c, bs') = ByteString.uncons bs
    initial =
      if isVowel c
        then State1 1 c False False
        else State1 0 c False False
  in
    finalState (ByteString.foldl' accumulator1 initial bs')

data State2 = State2
  { pairsSeen    :: HashSet (Char, Char)
  , lastLetters  :: (Char, Char)
  , spacedRepeat :: Bool
  , pairRepeat   :: Bool
  }
  deriving Show


accumulator2 :: State2 -> Char -> State2
accumulator2 (State2 pairs lP@(l1, l2) spRepeat prRepeat) c =
  let
    ll' = (l2, c)
    pairs' = HashSet.insert lP pairs
  in
  case (c == l1, ll' `HashSet.member` pairs) of
    (True, True) ->
      State2 pairs' ll' True True
    (True, False) ->
      State2 pairs' ll' True prRepeat
    (False, True) ->
      State2 pairs' ll' spRepeat True
    (False, False) ->
      State2 pairs' ll' spRepeat prRepeat

finalState2 :: State2 -> Bool
finalState2 (State2 _ _ b1 b2) =
    b1 && b2


nice2 :: ByteString -> Bool
nice2 bs =
  let
    Just (c1, bs')  = ByteString.uncons bs
    Just (c2, bs'') = ByteString.uncons bs'
    pair    = (c1, c2)
    initial = State2 mempty pair False False
  in
    finalState2 (ByteString.foldl' accumulator2 initial bs'')
