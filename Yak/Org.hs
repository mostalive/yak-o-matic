{- 
Parse org-mode files and generate a dot graph  from the tree
of TODOs inside this file
-}
module Yak.Org(toDotGraph) where

import Data.GraphViz(graphElemsToDot,
                     defaultParams,
                     GraphvizParams(..),
                     NodeCluster(..),
                     DotGraph,
                     -- for testing 
                     nodeInformation,
                     edgeInformation)
import Data.GraphViz.Types(GraphID(..))
import Data.Char(isSpace)
import Text.Regex.Posix
import Data.Maybe(fromJust)

-- for testing
import Data.Map(toAscList)
import Data.Text.Lazy(pack)

-- |Generate a graph from a list of headers in org-mode format.
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
-- Linking is done from parent to child with the header's level as a measure of parent-child relationship.
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
-- Of course, when we change the context of todos, moving up in the headers' level, children relationship should be taken care of correctly.
-- >>> show$  edgeInformation False $ toDotGraph ["** TODO parent", "*** DONE child", "** TODO other parent"]
-- "[DotEdge {fromNode = \"parent\", toNode = \"child\", edgeAttributes = []}]"
--
-- >>> show$  edgeInformation False $ toDotGraph ["** TODO parent", "*** DONE child", "** TODO other parent", "** TODO third parent"]
-- "[DotEdge {fromNode = \"parent\", toNode = \"child\", edgeAttributes = []}]"
--
-- >>> show$  edgeInformation False $ toDotGraph ["** TODO parent", "*** DONE child", "* TODO other parent","** TODO other child"]
-- "[DotEdge {fromNode = \"other parent\", toNode = \"other child\", edgeAttributes = []},DotEdge {fromNode = \"parent\", toNode = \"child\", edgeAttributes = []}]"
--
-- We don't care about non-todo headers: A todo-header is any header with a level and an upper-case keyword at start. Note that we do not distinguish
-- true org keyword from plain words in capital.
-- >>> show$ toAscList$ nodeInformation False $ toDotGraph ["* a header", "** TODO a todo"]
-- "[(\"a todo\",(fromList [Just (Str \"TODO\")],[]))]"
toDotGraph :: [String] -> DotGraph String
toDotGraph s = graphElemsToDot clusteredParams (map nodesFromTODOs todos) (edgesFromTree todos [] [])
  where
    todos = filter (match todoRegex) s
    clusteredParams = defaultParams {
      clusterBy = clusterByTODOKeyword,
      isDotCluster = const $ True,
      clusterID = Str . pack
      }
    clusterByTODOKeyword (n,_) = C typ $ N (tail lbl,"")
      where
        (typ,lbl) = span (not.isSpace) n

todoRegex :: Regex
todoRegex = makeRegex "(\\*+) +([A-Z]+) +(.*)" 

levelAndLabel :: String -> (Int, Maybe String)
levelAndLabel n = (length stars, Just txt) 
  where  [_:stars:_:txt:_] = match todoRegex n

edgesFromTree :: [String] -> [(Int,Maybe String)] -> [(String,String,String)] -> [(String,String,String)]
edgesFromTree []           _             acc = acc
edgesFromTree (todo:todos) []            acc = edgesFromTree todos [levelAndLabel todo]      acc
edgesFromTree (todo:todos) (context:cs)  acc
  | currentTodo `isDeeperThan` context       = edgesFromTree todos (currentTodo:context:cs)      ((labelOf context, labelOf currentTodo, "") :acc)
  | context `isDeeperThan` currentTodo       = edgesFromTree todos (popContextTo currentTodo cs) acc
  | atTopLevel                               = edgesFromTree todos (currentTodo:cs)              acc
  | atSameLevel                              = edgesFromTree todos (currentTodo:cs)              ((labelOf (head cs), labelOf currentTodo, ""):acc)
  where
    currentTodo = levelAndLabel todo
    labelOf = fromJust . snd
    levelOf = fst
    isDeeperThan a b = levelOf a > levelOf b
    atTopLevel  = null cs
    atSameLevel = levelOf currentTodo == levelOf context && (not$ null cs)
    popContextTo todo [] = [todo]
    popContextTo todo (context:cs) | levelOf context == levelOf todo = todo:cs
                                   | otherwise                       = popContextTo todo cs
                                                  
nodesFromTODOs :: String -> (String,String)
nodesFromTODOs todo = (todoType ++ " " ++ todoText,"")
  where
    [_:_:todoType:todoText:_] = match todoRegex todo
