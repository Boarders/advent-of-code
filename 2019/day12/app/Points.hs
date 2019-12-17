{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Points where

import           Data.Foldable
import           Data.Vector.Unboxed.Deriving

data Point = Point
  { x :: {-# UNPACK #-} !Int
  , y :: {-# UNPACK #-} !Int
  , z :: {-# UNPACK #-} !Int
  }
  deriving (Eq)

instance Show Point where
  show (Point x y z) =
    let
      xStr   = show x
      yStr   = show y
      zStr   = show z
    in
    fold [  "<x=" ,  pad 3 xStr
         , ", y=" ,  pad 3 yStr
         , ", z=" ,  pad 3 zStr
         , ">"]

pad :: Int -> String -> String
pad padLen xs =
  let
    len = length xs
  in
    replicate (padLen - len) ' ' <> xs



absSum :: Point -> Int
absSum Point{..} = abs x + abs y + abs z


instance Semigroup Point where
  (<>) (Point x1 y1 z1) (Point x2 y2 z2)
    = Point (x1 + x2) (y1 + y2) (z1 + z2)

instance Monoid Point where
  mempty = Point 0 0 0

-- Template haskell for unboxed instances
derivingUnbox "Point"
    [t| Point -> (Int, Int, Int)           |]
    [| \ (Point p0 p1 p2) -> (p0, p1, p2)  |]
    [| \ (x, y, z)        -> Point x y z   |]

