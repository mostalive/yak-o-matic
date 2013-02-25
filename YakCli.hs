
module YakCli(makeOptions, outputCfdData, 
              YakOptions(..),emptyOptions,toGraph) where
import Data.Maybe(fromJust,fromMaybe)
import System.FilePath(takeExtension)
import Data.GraphViz(DotGraph)
import System.Console.GetOpt

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
emptyOptions = YakOptions False (Just ".") (Just defaultYakFile)

options :: [OptDescr (YakOptions -> YakOptions)]
options = 
  [ Option ['g'] ["git-repo"] 
    (ReqArg (\ repopath yakoptions -> yakoptions { gitRepository = Just repopath }) "DIR")  "git repository containing yak file"
  , Option ['f'] ["yak-file"] 
    (ReqArg (\ yakfile yakoptions -> yakoptions { relativeYakFilePath = Just yakfile }) "FILE") "path of yak file relative to git repository"
  , Option ['v'] ["verbose"] 
    (NoArg (\ yakoptions -> yakoptions { debugParsing = True })) "verbose parsing of yak file"
  ]
    
-- |Parse command-line arguments building options for yakgraph execution
--
-- >>> makeOptions []
-- YakOptions {debugParsing = False, gitRepository = Just ".", relativeYakFilePath = Just "planning.dot"}
--
-- >>> makeOptions ["--git-repo=foo","-fbar"]
-- YakOptions {debugParsing = False, gitRepository = Just "foo", relativeYakFilePath = Just "bar"}
--
-- >>> makeOptions ["-gfoo", "-v", "-fbar"]
-- YakOptions {debugParsing = True, gitRepository = Just "foo", relativeYakFilePath = Just "bar"}
makeOptions :: [String] -> YakOptions 
makeOptions arguments = makeOptions' arguments 
  where
    (opts,_,[])            = getOpt Permute options arguments
    makeOptions' arguments = foldl (flip id) emptyOptions opts

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
  
toGraph :: String -> Maybe String -> DotGraph String
toGraph ".org" = toDotGraph.lines.fromJust
toGraph _      = parseGraph.fromJust

maybeDebug :: (Show a) => Bool -> a -> IO a
maybeDebug True s  = putStrLn (show s) >> return s
maybeDebug False s = return s

