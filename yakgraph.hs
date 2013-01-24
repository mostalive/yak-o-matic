import Data.GraphViz.Types
import Data.GraphViz.Types.Graph
import Data.GraphViz.Commands.IO
import Data.Map(keys,toAscList,empty,alter,Map(..))
import Data.List(groupBy)
import Data.Maybe
import Data.Text.Lazy(unpack)
import Data.Sequence(Seq)
import Data.Foldable(toList)

iograph = (readDotFile "planning.dot" :: IO (DotGraph String)) 

smarthost g = successorsOf g "MailSmartHost"

fromGraphId (Str s) = unpack s

listOfClusterNames = map fromGraphId.map fromJust.keys. snd. graphStructureInformation

keyByCluster ((n,(p,_)):ns) = (p,n) : keyByCluster ns
keyByCluster []  =  []

extractClusterName :: Seq (Maybe GraphID) -> String
extractClusterName seq = case toList seq of
                          []                  -> "inbox"
                          (Just (Str name)):_ -> unpack name


listOfPairsToMapOfKeysWithListOfValues ::  [(Seq(Maybe GraphID),String)] -> Map String [String]
listOfPairsToMapOfKeysWithListOfValues pairs = groupStuff pairs empty
  where
    groupStuff :: [(Seq(Maybe GraphID),String)] -> Map String [String] -> Map String [String] 
    groupStuff [] m = m
    groupStuff ((c,n):rest) m =  alter (addToList n) (extractClusterName c) (groupStuff rest m)
    addToList  n Nothing = Just [n]
    addToList n (Just ns) = Just $ n:ns

listOfNodesPerCluster =
 listOfPairsToMapOfKeysWithListOfValues . concatMap keyByCluster .groupBy sameCluster. toAscList . nodeInformation False
 where
  sameCluster (n,(p,_)) (n',(p',_)) = p == p' 

main = do 
    g <- iograph
    -- putStr $ show $ smarthost g
    putStrLn $ show.listOfClusterNames $ g
    putStrLn $ show.listOfNodesPerCluster $ g
