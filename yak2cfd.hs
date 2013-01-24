import System.Environment
import Data.GraphViz.Types.Graph
import Data.GraphViz.Commands.IO
import YakGraph

iograph :: String -> IO (DotGraph String)
iograph  = readDotFile

main = do
    [repoGit,pathToFile] <- getArgs 
    g <- iograph pathToFile
    putStrLn $ show.listOfClusterNames $ g
    putStrLn $ show.listOfNodesPerCluster $ g
    putStrLn $ show.countOfNodesPerCluster $ g
