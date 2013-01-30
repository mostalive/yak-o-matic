import System.Environment
import Data.GraphViz.Types.Graph
import Data.GraphViz.Commands.IO
import Data.GraphViz.Parsing
import Data.Text.Lazy(pack)
import Data.Maybe(fromJust)

import YakGraph
import YakGit

iograph :: String -> DotGraph String
iograph  = parseIt'.pack

-- |Output cfddata for a given file in a given repo
--
-- >>> outputCfdData "test-repo" "planning.dot" >>= return . head
-- "foo"
outputCfdData :: FilePath        -- ^Path to git repo
                 -> FilePath     -- ^File (expect graphviz format) to extract data from
                 -> IO [String]  -- ^CFD Data extracted from graph file's content
outputCfdData gitrepo filename = do
  commits <- gitCommitsForFile gitrepo filename
  contents <- mapM (gitContentOfFileAtCommit gitrepo filename . gitHash) commits 
  return $ map (show . countOfNodesPerCluster . iograph . fromJust) contents

  
main = do
    [repoGit,pathToFile] <- getArgs
    cfdData <- outputCfdData repoGit pathToFile
    mapM_ putStrLn cfdData
