import Data.GraphViz.Types.Graph
import Data.GraphViz.Commands.IO
import YakGraph

iograph = (readDotFile "planning.dot" :: IO (DotGraph String)) 

main = do 
    g <- iograph
    putStrLn $ show.listOfClusterNames $ g
    putStrLn $ show.listOfNodesPerCluster $ g
    putStrLn $ show.countOfNodesPerCluster $ g
