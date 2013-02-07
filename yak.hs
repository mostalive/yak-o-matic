import System.Environment
import Data.Maybe(fromJust)
import Control.Monad

import YakGraph
import YakGit

data YakOptions = YakOptions {
  debugParsing         :: Bool,            -- ^Trace Graphviz parser input & output
  gitRepository        :: Maybe FilePath,  -- ^Path to git repo                               
  relativeYakFilePath  :: Maybe FilePath   -- ^File (expect graphviz format) to extract data from
  } deriving (Eq, Show, Read)
             
emptyOptions :: YakOptions
emptyOptions = YakOptions False Nothing Nothing

-- |Parse command-line arguments building options for yakgraph execution
--
-- >>> makeOptions []
-- YakOptions {debugParsing = False, gitRepository = Nothing, relativeYakFilePath = Nothing}
--
-- >>> makeOptions ["foo" ,"bar"]
-- YakOptions {debugParsing = False, gitRepository = Just "foo", relativeYakFilePath = Just "bar"}
--
-- >>> makeOptions ["foo" ,"-v", "bar"]
-- YakOptions {debugParsing = True, gitRepository = Just "foo", relativeYakFilePath = Just "bar"}
makeOptions :: [String] -> YakOptions 
makeOptions options = makeOptions' options emptyOptions
  where
    makeOptions' ("-v":opts) y                                = makeOptions' opts $ y { debugParsing = True }
    makeOptions' (opt:opts)  y@(YakOptions _ Nothing Nothing) = makeOptions' opts $ y { gitRepository = Just opt }
    makeOptions' (opt:opts)  y@(YakOptions _ _       Nothing) = makeOptions' opts $ y { relativeYakFilePath = Just opt }
    makeOptions' (_:opts)    y@(YakOptions _ _       _)       = makeOptions' opts $ y 
    makeOptions' []          y                                = y 

type Cfd = [(String, Int)]

data YakStep = YakStep {
  commitId :: GitHash,        -- ^Identifier for this step
  cfd      :: Cfd             -- ^Count of number of tasks per cluster
  } deriving (Eq, Show, Read)

-- |Output cfddata for a given file in a given repo
--
-- >>> outputCfdData (YakOptions False (Just "test-repo") (Just "planning.dot")) >>= return . head
-- YakStep {commitId = "f826a39", cfd = [("done",13),("inbox",9),("inprogress",2),("processacceleration",4),("technicaldebt",6)]}
--
-- >>> outputCfdData (YakOptions True (Just "test-repo") Nothing)
-- *** Exception: Invalid command-line arguments
outputCfdData :: YakOptions    
                 -> IO [YakStep]  -- ^CFD Data extracted from graph file's content
outputCfdData (YakOptions debug (Just gitrepo) (Just filename)) =
  gitCommitsForFile gitrepo filename >>=
  mapM (buildCfdForCommit debug gitrepo filename)
outputCfdData _ = error "Invalid command-line arguments"

buildCfdForCommit :: Bool ->  FilePath -> FilePath -> GitCommit -> IO YakStep
buildCfdForCommit debug gitrepo filename commit =                      
  (gitContentOfFileAtCommit gitrepo filename . gitHash) commit >>=
  maybeDebug debug >>=
  return . YakStep (gitHash commit) . countOfNodesPerCluster . parseGraph . fromJust
  
maybeDebug :: (Show a) => Bool -> a -> IO a
maybeDebug True s  = putStrLn (show s) >> return s
maybeDebug False s = return s

main :: IO ()
main = do
    args <- getArgs
    let yakOptions = makeOptions args
    cfdData <- outputCfdData yakOptions
    mapM_ (putStrLn.show) cfdData
