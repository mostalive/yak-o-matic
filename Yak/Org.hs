{- 
Parse org-mode files and generate a dot graph  from the tree
of TODOs inside this file
-}
module Yak.Org where

import Data.GraphViz(graphElemsToDot,
                     defaultParams,
                     GraphvizParams(..),
                     NodeCluster(..),
                     DotGraph,
                     nodeInformation)
import Data.Map(toAscList)
import Data.GraphViz.Types(GraphID(..))
import Data.GraphViz.Attributes.Complete(Attribute(Label), Label(..))
import Data.Text.Lazy(Text, pack)

import YakGraph

-- |Generate a graph from a list of strings.
--
-- >>> show$ toAscList$ nodeInformation False $ toDotGraph ["*** TODO something todo"]
-- "[(\"something todo\",(fromList [Just (Str \"TODO\")],[]))]"
--
-- >>> show$ toAscList$ nodeInformation False $ toDotGraph ["** DONE something else"]
-- "[(\"something else\",(fromList [Just (Str \"DONE\")],[]))]"
toDotGraph :: [String] -> DotGraph String
toDotGraph s = graphElemsToDot clusteredParams (map nodesFromTODOs s) []
  where
    clusteredParams = defaultParams {
      clusterBy = clusterByTODOKeyword,
      isDotCluster = const $ True,
      clusterID = identifyCluster
      }

identifyCluster s = Str $ pack s

clusterByTODOKeyword ("TODO something todo",nl) = C "TODO" $ N ("something todo","")
clusterByTODOKeyword ("DONE something else",nl) = C "DONE" $ N ("something else","")

nodesFromTODOs "*** TODO something todo" = ("TODO something todo","")
nodesFromTODOs "** DONE something else" = ("DONE something else","")
