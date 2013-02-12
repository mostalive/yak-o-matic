{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Data(Data,Typeable)
import Data.Time(UTCTime)

-- | Cumulative flow diagram representation
type Cfd = [(String, Int)]

-- | A git hash value (SHA-1)
type GitHash = String

-- |A single step of the yak
data YakStep = YakStep {
  commitId   :: GitHash,  -- ^Identifier for this step
  commitDate :: UTCTime,  -- ^Date of commit
  cfd        :: Cfd       -- ^Count of number of tasks per cluster
  } deriving (Eq, Show, Read, Typeable, Data)

type Yak = [YakStep]

-- | A single git commit.
data GitCommit = GitCommit { 
  gitHash       :: GitHash,  -- ^Git unique hash identifier for the commit
  gitDate       :: UTCTime,  -- ^Commit date
  gitLogMessage :: String   -- ^Log message
  } deriving (Eq, Show)


iso8601DateFormat :: String
iso8601DateFormat = "%Y-%m-%d %T %z"


