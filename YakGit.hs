{-# LANGUAGE PatternGuards #-}
module YakGit(gitCommitsForFile,
              gitContentOfFileAtCommit) where

import qualified Text.ParserCombinators.ReadP as P
import Lib.Git.Type
import Data.Maybe(fromJust, isJust)
import System.Locale(defaultTimeLocale)
import Data.Time(readTime)

import Types

parse' :: String -> P.ReadP a -> [a]
parse' s p = [x | (x,_) <- (P.readP_to_S p s) ]

listCommitsFromLogOutput :: String -> [GitCommit] 
listCommitsFromLogOutput = map fromJust . filter isJust . map (parseOneLineLog) . lines
  where
    parseOneLineLog s | [x] <- parse' s parseLog = Just x
                      | otherwise                = Nothing

    parseLog         = do 
      [h,d,m] <- P.sepBy (P.munch (/= ',')) (P.char ',')
      return (GitCommit h (readTime defaultTimeLocale iso8601DateFormat d) m)

-- | List all commits for a given file.
--
-- We only test for the hashes       
--
-- >>> gitCommitsForFile "test-repo" "planning.dot" >>= return . map gitHash
-- ["f826a39","8aff6d5","2d251b4"]
--
-- >>> gitCommitsForFile "test-repo" "planning.dot" >>= return . map gitDate
-- [2013-01-30 18:36:07 UTC,2013-01-30 18:35:04 UTC,2013-01-30 18:34:13 UTC]
--
-- >>> gitCommitsForFile "test-repo" "non-existing-file" >>= return . map gitHash    
-- []
--
-- >>> gitCommitsForFile "no-repo" "planning.dot" >>= return . map gitHash    
-- *** Exception: git: createProcess: invalid argument (Invalid argument)
gitCommitsForFile :: FilePath           -- ^The path to the git repository containing file 
                     -> FilePath        -- ^The path to the file we want to look at, relative to the git repo
                     -> IO [GitCommit]  -- ^A list of @GitCommit@ objects
gitCommitsForFile gitrepo filename = 
  let config = makeConfig gitrepo Nothing
      logOneLine = gitExec "log" ["--format=format:%h,%ad,%s", "--date=iso", filename] []
  in runGit config logOneLine >>= 
     return . either (return []) (listCommitsFromLogOutput)


-- |Retrieve the content of a file at specified commit
--
-- >>> gitContentOfFileAtCommit "test-repo" "planning.dot" "8aff6d5" >>= return . head. drop 21 . lines . fromJust
-- "    RvmInChef"
--
-- >>> gitContentOfFileAtCommit "test-repo" "no-File" "8aff6d5"
-- Nothing
--
-- >>> gitContentOfFileAtCommit "test-repo" "planning.dot" "1234566"
-- Nothing
--
-- >>> gitContentOfFileAtCommit "no-repo" "planning.dot" "8aff6d5"
-- *** Exception: git: createProcess: invalid argument (Invalid argument)
gitContentOfFileAtCommit :: FilePath             -- ^The path to the git repository containing file 
                            -> FilePath          -- ^The path to the file we want to look at, relative to the git repo
                            -> GitHash           -- ^The commit at which file content is requested
                            -> IO (Maybe String) -- ^Content of file if found, 
gitContentOfFileAtCommit gitrepo filename h = 
  let config  = makeConfig gitrepo Nothing
      content = gitExec "show" [h ++ ":" ++ filename] []
  in runGit config content >>=
     return . either (const Nothing) Just
