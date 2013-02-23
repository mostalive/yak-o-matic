
module YakCli(makeOptions, outputCfdData, 
              YakOptions(..),emptyOptions) where
import Data.Maybe(fromJust)
import System.FilePath(takeExtension)

import Types
import YakGraph
import YakGit
import Yak.Org

data YakOptions = YakOptions {
  debugParsing         :: Bool,            -- ^Trace Graphviz parser input & output
  gitRepository        :: Maybe FilePath,  -- ^Path to git repo                               
  relativeYakFilePath  :: Maybe FilePath   -- ^File (expect graphviz format) to extract data from
  } deriving (Eq, Show, Read)
             
defaultYakFile :: String
defaultYakFile = "planning.dot"

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
-- >>> makeOptions ["foo", "-v", "bar"]
-- YakOptions {debugParsing = True, gitRepository = Just "foo", relativeYakFilePath = Just "bar"}
makeOptions :: [String] -> YakOptions 
makeOptions options = makeOptions' options emptyOptions
  where
    makeOptions' ("-v":opts) y                                = makeOptions' opts $ y { debugParsing = True }
    makeOptions' (opt:opts)  y@(YakOptions _ Nothing Nothing) = makeOptions' opts $ y { gitRepository = Just opt }
    makeOptions' (opt:opts)  y@(YakOptions _ _       Nothing) = makeOptions' opts $ y { relativeYakFilePath = Just opt }
    makeOptions' (_:opts)    y@(YakOptions _ _       _)       = makeOptions' opts $ y 
    makeOptions' []          y                                = y 

-- |Output cfddata for a given file in a given repo
-- Path to planning file is optional and defaults to `planning.dot`:
--
-- >>> outputCfdData (YakOptions False (Just "test-repo") (Just "planning.dot")) >>= return . head
-- YakStep {commitId = "f826a39", commitDate = 2013-01-30 18:36:07 UTC, cfd = [("done",13),("inbox",9),("inprogress",2),("processacceleration",4),("technicaldebt",6)]}
-- >>> outputCfdData (YakOptions False (Just "test-repo") Nothing) >>= return . head
-- YakStep {commitId = "f826a39", commitDate = 2013-01-30 18:36:07 UTC, cfd = [("done",13),("inbox",9),("inprogress",2),("processacceleration",4),("technicaldebt",6)]}
--
-- >>> outputCfdData (YakOptions False Nothing Nothing)
-- [YakStep {commitId = "47e3075", commitDate = 2013-01-29 18:34:58 UTC, cfd = [("done",12),("inbox",8),("inprogress",1),("processacceleration",4),("technicaldebt",8)]}]
outputCfdData :: YakOptions    
                 -> IO Yak  -- ^CFD Data extracted from graph file's content
outputCfdData (YakOptions debug (Just gitrepo) (Just filename)) =
  gitCommitsForFile gitrepo filename >>=
  mapM (buildCfdForCommit debug gitrepo filename)
outputCfdData y@(YakOptions _ (Just _) Nothing) = outputCfdData $ y { relativeYakFilePath = Just defaultYakFile }
outputCfdData y@(YakOptions _ Nothing  Nothing) = outputCfdData $ y { gitRepository = Just ".", relativeYakFilePath = Just defaultYakFile }
outputCfdData _                                 = error "Invalid command-line arguments"

buildCfdForCommit :: Bool ->  FilePath -> FilePath -> GitCommit -> IO YakStep
buildCfdForCommit debug gitrepo filename commit =                      
  (gitContentOfFileAtCommit gitrepo filename . gitHash) commit >>=
  maybeDebug debug >>=
  return . YakStep (gitHash commit) (gitDate commit) . countOfNodesPerCluster . toGraph (takeExtension filename)
  
toGraph ".org" = toDotGraph.lines.fromJust
toGraph _      = parseGraph.fromJust

maybeDebug :: (Show a) => Bool -> a -> IO a
maybeDebug True s  = putStrLn (show s) >> return s
maybeDebug False s = return s

