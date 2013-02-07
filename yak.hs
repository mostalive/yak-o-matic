import System.Environment
import Data.GraphViz.Types.Graph
import Data.GraphViz.Parsing
import Data.Text.Lazy(pack)
import Data.Maybe(fromJust)

import YakGraph
import YakGit

iograph :: String -> DotGraph String
iograph  = parseIt'.pack

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
  
-- |Output cfddata for a given file in a given repo
--
-- >>> outputCfdData (YakOptions False (Just "test-repo") (Just "planning.dot")) >>= return . head
-- "[(\"done\",13),(\"inbox\",9),(\"inprogress\",2),(\"processacceleration\",4),(\"technicaldebt\",6)]"
--
-- >>> outputCfdData (YakOptions True (Just "test-repo") Nothing)
-- *** Exception: Invalid command-line arguments
outputCfdData :: YakOptions    
                 -> IO [String]  -- ^CFD Data extracted from graph file's content
outputCfdData (YakOptions debug (Just gitrepo) (Just filename)) =
  gitCommitsForFile gitrepo filename >>=
  mapM (gitContentOfFileAtCommit gitrepo filename . gitHash) >>=
  maybeDebug debug >>=
  return . map (show . countOfNodesPerCluster . iograph . fromJust)
outputCfdData _ = error "Invalid command-line arguments"

  
maybeDebug :: Bool -> [Maybe String] -> IO ([Maybe String])
maybeDebug True s  = putStrLn (show s) >> return s
maybeDebug False s = return s

main :: IO ()
main = do
    args <- getArgs
    let yakOptions = makeOptions args
    cfdData <- outputCfdData yakOptions
    mapM_ putStrLn cfdData
