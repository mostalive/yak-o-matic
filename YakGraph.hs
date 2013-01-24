module YakGraph(listOfClusterNames, listOfNodesPerCluster, countOfNodesPerCluster) where
import Data.GraphViz.Types
import Data.GraphViz.Types.Graph
import Data.GraphViz.Commands.IO
import qualified Data.Map as M
import Data.List(groupBy)
import Data.Maybe
import Data.Text.Lazy(unpack)
import Data.Sequence(Seq)
import Data.Foldable(toList)

fromGraphId (Str s) = unpack s

listOfClusterNames :: DotGraph String -> [String]
listOfClusterNames = map fromGraphId.map fromJust.M.keys. snd. graphStructureInformation

keyByCluster ((n,(p,_)):ns) = (p,n) : keyByCluster ns
keyByCluster []  =  []

extractClusterName :: Seq (Maybe GraphID) -> String
extractClusterName seq = case toList seq of
                          []                  -> "inbox"
                          (Just (Str name)):_ -> unpack name


listOfPairsToMapOfKeysWithListOfValues ::  [(Seq(Maybe GraphID),String)] -> M.Map String [String]
listOfPairsToMapOfKeysWithListOfValues pairs = groupStuff pairs M.empty
  where
    groupStuff [] m = m
    groupStuff ((c,n):rest) m =  M.alter (addToList n) (extractClusterName c) (groupStuff rest m)
    addToList  n Nothing = Just [n]
    addToList n (Just ns) = Just $ n:ns

countOfNodesPerCluster :: DotGraph String -> [(String, Int)]
countOfNodesPerCluster = map (\ (c,l) -> (c,length l)) . M.toList  .listOfNodesPerCluster


listOfNodesPerCluster :: DotGraph String -> M.Map String [String]
listOfNodesPerCluster =
 listOfPairsToMapOfKeysWithListOfValues . concatMap keyByCluster .groupBy sameCluster. M.toAscList . nodeInformation False
 where
  sameCluster (n,(p,_)) (n',(p',_)) = p == p' 

