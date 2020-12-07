{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}
module Day7 where

import Common
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)

import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)
import Data.List (uncons, foldl')
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.Set as Set
import Data.Set (Set)

-- For dot file output
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO                 as TL
import           System.FilePath.Posix             ((<.>))


day7 :: IO ()
day7 = do
  input <- parseInput
  let
    sol1  = s1 input
    sol2  = s2 input
  putStrLn $ (solutions 2 sol1 sol2)



parseInput :: IO Graph
parseInput = do
  bs <- ByteString.readFile "input/day7.dat"
  pure (parseBags bs)

type Edge = (Int, Label)
type Label = ByteString
type Node = (Label, Set Edge)
type Graph = HashMap Label (Set Edge)

toGraph :: [Node] -> Graph
toGraph = foldl' addNode mempty
  where
    addNode :: Graph -> Node -> Graph
    addNode g ~(n, e) = Map.insert n e g

parseBags :: ByteString -> Graph
parseBags = toGraph . fmap parseBag . ByteString.lines

parseBag :: ByteString -> Node
parseBag bs =
  case head items == "no" of
    True ->  (topColour, mempty)
    False -> (topColour, convertList (ByteString.unwords items))
  where
    topColour = ByteString.unwords top
    bswds = ByteString.words bs
    (top, rest) = splitAt 2 bswds
    (_, items)  = splitAt 2 rest

convertList :: ByteString -> Set Edge
convertList = Set.fromList . fmap convertItem . ByteString.split ','
  where
    convertItem :: ByteString -> (Int, ByteString)
    convertItem =
        bimap bsToInt (ByteString.unwords)
      . (fromJust . uncons)
      . take 3
      . ByteString.words

transposeGraph :: Graph -> Graph
transposeGraph =
    Map.foldlWithKey' addReverseEdges mempty
  where
    addReverseEdges :: Graph -> Label -> Set Edge -> Graph
    addReverseEdges g src =
      Set.foldl'
        (\g' ~(n, tgt) -> Map.insertWith (<>) tgt (Set.singleton (n, src)) g')
        (Map.insertWith (<>) src mempty g)

getAdjacent :: Label -> Graph -> Set ByteString
getAdjacent n g = Set.map snd (g Map.! n)

findDescendents :: Label -> Graph -> Set ByteString
findDescendents n g =
    let descs = getAdjacent n g in
      foldl' (\acc d -> findDescendents d g <> acc) descs descs

countDescendents :: Label -> Graph -> Int
countDescendents n g =
    let descs = g Map.! n in
      foldl' (\acc (n', d) -> n' * (countDescendents d g) + acc) 1 descs


s1, s2 :: Graph -> Int
s1 = Set.size . findDescendents "shiny gold" . transposeGraph
s2 = pred . countDescendents "shiny gold"


---------------------
-- dot file output --
---------------------

testInput :: ByteString
testInput =
  ByteString.unlines
  [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
  , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  , "bright white bags contain 1 shiny gold bag."
  , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  , "faded blue bags contain no other bags."
  , "dotted black bags contain no other bags."
  ]

makeDotFile :: (Graph, FilePath) -> IO ()
makeDotFile (graph, filename) = do
    let
      (nodes, edges) = (getNodes graph, getEdges graph)
      dotGraph =  G.graphElemsToDot networkGraphParameters nodes edges :: G.DotGraph String
      dotText  =  G.printDotGraph dotGraph :: TL.Text
    TL.writeFile (filename <.> "dot") dotText

type DotNode  = String
type DotEdge  = (String, String, Int)
type DotGraph = ([DotNode], [DotEdge])

networkGraphParameters
 :: G.GraphvizParams
       String          -- vertex type
       ()              -- vertex label type
       Int              -- edge label type
       ()              -- cluster type
       ()              -- cluster label type
networkGraphParameters = G.defaultParams {
    G.fmtNode = \case
        (str, _)  -> colorAttribute  green <> nodeSh <> [G.textLabel (TL.pack str)]
    ,
    G.fmtEdge = \case
        (_, _, n)  -> colorAttribute blue <> [G.textLabel (TL.pack (show n))]
    }
  where
    colorAttribute color = [ G.Color $ G.toColorList [color] ]
    blue   = G.RGB 30 144 255
    green = G.RGB 0 100 0
    nodeSh = [G.Shape G.Ellipse] <> [G.Area 0.5]

getNodes :: Graph -> [(String, ())]
getNodes = fmap (\x -> (x, ())) . fmap ByteString.unpack . Map.keys

getEdges :: Graph -> [(String, String, Int)]
getEdges = Map.foldMapWithKey toEdge
  where
    toEdge :: ByteString -> Set Edge -> [(String, String, Int)]
    toEdge srcNode
      =
      let
        srcNodeStr = ByteString.unpack srcNode
      in
          Set.toList . Set.fromList
        . fmap (\(n, tgtNode) -> (srcNodeStr, ByteString.unpack tgtNode, n))
        . Set.toList
