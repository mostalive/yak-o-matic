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
import Data.Maybe(fromJust)

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
--
-- >>> show$  edgeInformation False $ toDotGraph ["** TODO parent", "*** DONE child"]
-- "[DotEdge {fromNode = \"parent\", toNode = \"child\", edgeAttributes = []}]"
--
-- >>> show$  edgeInformation False $ toDotGraph ["** TODO parent", "*** DONE child", "*** TODO other child"]
-- "[DotEdge {fromNode = \"parent\", toNode = \"other child\", edgeAttributes = []},DotEdge {fromNode = \"parent\", toNode = \"child\", edgeAttributes = []}]"
--
-- >>> show$  edgeInformation False $ toDotGraph ["** TODO parent", "*** DONE child", "**** TODO grandchild"]
-- "[DotEdge {fromNode = \"child\", toNode = \"grandchild\", edgeAttributes = []},DotEdge {fromNode = \"parent\", toNode = \"child\", edgeAttributes = []}]"
--
-- >>> show$  edgeInformation False $ toDotGraph ["** TODO parent", "*** DONE child", "** TODO other parent"]
-- "[DotEdge {fromNode = \"parent\", toNode = \"child\", edgeAttributes = []}]"
--
-- >>> show$  edgeInformation False $ toDotGraph ["** TODO parent", "** TODO other parent"]
-- "[]"
toDotGraph :: [String] -> DotGraph String
toDotGraph s = graphElemsToDot clusteredParams (map nodesFromTODOs s) (edges s)
  where
    clusteredParams = defaultParams {
      clusterBy = clusterByTODOKeyword,
      isDotCluster = const $ True,
      clusterID = identifyCluster
      }

todoRegex :: Regex
todoRegex = makeRegex "(\\*+) +([^ ]*) +(.*)" 

edges :: [String] -> [(String, String, String)]
edges s = edges' s [] []
    
levelAndLabel n = (length stars, Just txt)
  where
    [_:stars:typ:txt:_] = match todoRegex n

edges' []     _       acc    = acc
edges' (e:es) []      acc    = edges' es [(levelAndLabel e)] acc
edges' (e:es) (c:cs)  acc 
  | levelOf lle > levelOf c  = edges' es (lle:c:cs)          ((labelOf c, labelOf lle, "") :acc)
  | levelOf lle == levelOf c 
    && null cs               = edges' es (lle:cs)            acc
  | levelOf lle == levelOf c 
    && (not$ null cs)        = edges' es (lle:cs)            ((labelOf (head cs), labelOf lle, ""):acc)
  | levelOf lle < levelOf c  = edges' es (lle:cs)            acc
  where
    lle = levelAndLabel e
    labelOf = fromJust . snd
    levelOf = fst
    
identifyCluster s = Str $ pack s

clusterByTODOKeyword (n,nl) = C typ $ N (tail lbl,"")
  where
    (typ,lbl) = span (not.isSpace) n
    
nodesFromTODOs :: String -> (String,String)
nodesFromTODOs todo = (todoType ++ " " ++ todoText,"")
  where
    [_:_:todoType:todoText:_] = match todoRegex todo
