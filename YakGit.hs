module YakGit where

import Text.ParserCombinators.ReadP((+++))
import qualified Text.ParserCombinators.ReadP as P
import Data.Char(isSpace,isDigit,isHexDigit)
import Lib.Git.Type
import Data.Maybe(fromJust, isJust)

parse' s p = [x | (x,t) <- (P.readP_to_S p s) ]

-- | A single git commit.
data GitCommit = GitCommit { 
  gitHash       :: String,  -- ^Git unique hash identifier for the commit
  gitLogMessage :: String   -- ^Log message
  } deriving (Eq, Show)

listCommitsFromLogOutput :: String -> [GitCommit] 
listCommitsFromLogOutput = map fromJust . filter isJust . map (parseOneLineLog) . lines
  where
    parseOneLineLog s | [x] <- parse' s parseLog = Just x
                      | otherwise                = Nothing

    parseLog         = do 
      h <- P.munch (isHexDigit)
      P.skipSpaces
      m <- P.munch (const True)
      return (GitCommit h m)

-- | List all commits for a given file.
--
-- We only test for the hashes       
-- >>> gitCommitsForFile "test-repo" "planning.dot" >>= return . map gitHash
-- ["f826a39","8aff6d5","2d251b4"]
gitCommitsForFile :: FilePath           -- ^The path to the git repository containing file 
                     -> FilePath        -- ^The path to the file we want to look at, relative to the git repo
                     -> IO [GitCommit]  -- ^A list of @GitCommit@ objects
gitCommitsForFile gitrepo filename = 
  let config = makeConfig gitrepo Nothing
      log    = gitExec "log" ["--oneline", filename] []
  in runGit config log >>= 
     return . either (return []) (listCommitsFromLogOutput)
