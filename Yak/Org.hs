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
--
-- >>> show$ toAscList$ nodeInformation False $ toDotGraph ["* a header", "** TODO a todo"]
-- "[(\"a todo\",(fromList [Just (Str \"TODO\")],[]))]"
toDotGraph :: [String] -> DotGraph String
toDotGraph s = graphElemsToDot clusteredParams (map nodesFromTODOs todos) (edgesFromTree todos [] [])
  where
    todos = filter (match todoRegex) s
    clusteredParams = defaultParams {
      clusterBy = clusterByTODOKeyword,
      isDotCluster = const $ True,
      clusterID = identifyCluster
      }

todoRegex :: Regex
todoRegex = makeRegex "(\\*+) +([A-Z]+) +(.*)" 

levelAndLabel n = (length stars, Just txt)
  where
    [_:stars:typ:txt:_] = match todoRegex n

edgesFromTree []           _             acc   = acc
edgesFromTree (todo:todos) []            acc   = edgesFromTree todos [levelAndLabel todo]      acc
edgesFromTree (todo:todos) (context:cs)  acc 
  | currentTodo `isDeeperThan` context         = edgesFromTree todos (currentTodo:context:cs)  ((labelOf context, labelOf currentTodo, "") :acc)
  | context `isDeeperThan` currentTodo 
    || atTopLevel                              = edgesFromTree todos (currentTodo:cs)          acc
  | atSameLevel                                = edgesFromTree todos (currentTodo:cs)          ((labelOf (head cs), labelOf currentTodo, ""):acc)
  where
    currentTodo = levelAndLabel todo
    labelOf = fromJust . snd
    levelOf = fst
    isDeeperThan a b = levelOf a > levelOf b
    atTopLevel  = null cs
    atSameLevel = levelOf currentTodo == levelOf context && (not$ null cs)
    
identifyCluster s = Str $ pack s

clusterByTODOKeyword (n,nl) = C typ $ N (tail lbl,"")
  where
    (typ,lbl) = span (not.isSpace) n
    
nodesFromTODOs :: String -> (String,String)
nodesFromTODOs todo = (todoType ++ " " ++ todoText,"")
  where
    [_:_:todoType:todoText:_] = match todoRegex todo
