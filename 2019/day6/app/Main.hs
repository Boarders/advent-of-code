{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Tree             (Tree (..))
import qualified Data.Tree             as Tree

main :: IO ()
main =
  do
    orbits <- parseInput
    let
        printT
          = putStrLn
          . Tree.drawTree
          . fmap ByteString.unpack
          . mapToTree
          . inputToMap
    putStrLn "Example orbit:"
    putStrLn ""
    printT example
    putStrLn $ "Total orbits in example:          "        <> (show . puzzle1 $ example)
    putStrLn $ "Orbital distance from SAN to YOU: " <> (show . puzzle2 $  example)
    putStrLn ""
    putStrLn "Puzzle solutions:"
    print $ puzzle1 orbits
    print $ puzzle2 orbits


-- Name of root in all orbits
com :: ByteString
com = "COM"


-- puzzle solutions
puzzle1 :: [(ByteString, ByteString)] -> Int
puzzle1 = sum . depth . mapToTree . inputToMap

puzzle2 :: [(ByteString, ByteString)] -> Int
puzzle2 orbits = findDistance (toChildParentMap orbits) "SAN" "YOU"

-- Find the sum of the distance to the nearest common ancestor
findDistance :: Map ByteString ByteString -> ByteString -> ByteString -> Int
findDistance relations startNode = findNearestAncestor 0
  where
    ancestors :: Map ByteString Int -> Int ->  ByteString -> Map ByteString Int
    ancestors ancestorMap i node =
      let
        parent = node `Map.lookup` relations
      in
        case parent of
          Nothing         -> ancestorMap
          Just nodeParent -> ancestors (Map.insert nodeParent i ancestorMap) (i + 1) nodeParent

    startNodeAncestors :: Map ByteString Int
    startNodeAncestors = ancestors mempty 0 startNode

    findNearestAncestor :: Int  -> ByteString  -> Int
    findNearestAncestor j  node =
      let
        parent = relations Map.! node
      in
        case parent `Map.lookup` startNodeAncestors of
          Nothing -> findNearestAncestor (j + 1) parent
          Just i  -> i + j


-- Label each node with its depth
depth :: forall a . Tree a -> Tree Int
depth = go 0
  where
    go :: Int -> Tree a -> Tree Int
    go i (Node _ forest) = Node i (go (i + 1) <$> forest)

-- Conversion functions

type Parent = ByteString
type Children = [ByteString]

inputToMap :: [(ByteString, ByteString)] -> Map Parent Children
inputToMap = foldr go mempty
  where
  go :: (ByteString, ByteString) -> Map Parent Children -> Map Parent Children
  go (k, v) = Map.insertWith (<>) k (pure v)


toChildParentMap :: [(ByteString, ByteString)] -> Map ByteString ByteString
toChildParentMap = Map.fromList . fmap (\(x,y) -> (y,x))


mapToTree :: Map Parent Children -> Tree ByteString
mapToTree relations = go com
  where
    go :: Parent  -> Tree ByteString
    go startNode =
      case Map.lookup startNode relations of
        Nothing -> Node startNode []
        Just cs -> Node startNode ((\c -> go c) <$> cs)


 -- Parse Input
parseInput :: IO ([(ByteString, ByteString)])
parseInput =
    ByteString.readFile "input.txt" >>= pure . processInput


processInput :: ByteString -> [(ByteString, ByteString)]
processInput
  = fmap (\(x:y:_) -> (x, y)) . fmap (ByteString.split ')') . ByteString.lines

-- Running example
example :: [(ByteString, ByteString)]
example =
  processInput . ByteString.unlines $
    ["COM)B"
    ,"B)C"
    ,"C)D"
    ,"D)E"
    ,"E)F"
    ,"B)G"
    ,"G)H"
    ,"D)I"
    ,"E)J"
    ,"J)K"
    ,"K)L"
    ,"K)YOU"
    ,"I)SAN"
    ]
