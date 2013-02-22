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
                     nodeInformation,
                     edgeInformation)
import Data.Map(toAscList)
import Data.GraphViz.Types(GraphID(..))
import Data.GraphViz.Attributes.Complete(Attribute(Label), Label(..))
import Data.Text.Lazy(Text, pack)
import Data.Char(isSpace)
import Text.Regex.Posix

import YakGraph

-- |Generate a graph from a list of strings.
--
-- >>> show$ toAscList$ nodeInformation False $ toDotGraph ["*** TODO something todo"]
-- "[(\"something todo\",(fromList [Just (Str \"TODO\")],[]))]"
--
-- >>> show$ toAscList$ nodeInformation False $ toDotGraph ["** DONE something else"]
-- "[(\"something else\",(fromList [Just (Str \"DONE\")],[]))]"
--
-- >>> show$ toAscList$ nodeInformation False $ toDotGraph ["** DONE something other"]
-- "[(\"something other\",(fromList [Just (Str \"DONE\")],[]))]"
--
-- >>> show$  edgeInformation False $ toDotGraph ["** TODO something other", "*** DONE something else"]
-- "[DotEdge {fromNode = \"something other\", toNode = \"something else\", edgeAttributes = []}]"
toDotGraph :: [String] -> DotGraph String
toDotGraph s = graphElemsToDot clusteredParams (map nodesFromTODOs s) (edges s)
  where
    clusteredParams = defaultParams {
      clusterBy = clusterByTODOKeyword,
      isDotCluster = const $ True,
      clusterID = identifyCluster
      }

edges ["** TODO something other", "*** DONE something else"] = [("something other","something else","")]
edges _ = []
  
identifyCluster s = Str $ pack s

clusterByTODOKeyword (n,nl) = C typ $ N (tail lbl,"")
  where
    (typ,lbl) = span (not.isSpace) n
    
nodesFromTODOs :: String -> (String,String)
nodesFromTODOs todo = (todoType ++ " " ++ todoText,"")
  where
    [_:todoType:todoText:_] = match (makeRegex "\\*+ (.*) (.*)" :: Regex) todo
