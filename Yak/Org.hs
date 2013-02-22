{- 
Parse org-mode files and generate a dot graph  from the tree
of TODOs inside this file
-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text.Lazy

import YakGraph

-- |Generate a graph from a list of strings.
--
-- >>> show$ toAscList$ nodeInformation False $ toDotGraph ["*** TODO something todo"]
-- "[(\"something todo\",(fromList [Just (Str \"TODO\")],[]))]"
toDotGraph :: [String] -> DotGraph String
toDotGraph _ = graphElemsToDot clusteredParams [("something todo","")] []
  where
    clusteredParams = defaultParams {
      clusterBy = \ n ->  C "TODO" $ N n,
      isDotCluster = const $ True,
      clusterID = const $ Str "TODO"
      }
