{-# LANGUAGE TupleSections, NamedFieldPuns, FlexibleInstances #-}
module YakWeb where

-- needed for tests but hidden
import System.Locale(defaultTimeLocale)
import Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import Data.Time(UTCTime, readTime)
import Data.ByteString.Lazy.UTF8(toString)
import Data.Text(pack)
import Data.List(union)
import Control.Arrow((***))
import Data.Fixed(Pico)

import qualified Data.Aeson.Generic as G
import Data.Aeson(encode,ToJSON(..),(.=),object)
import Data.ByteString.Lazy(ByteString)
import Types
  
-- |Encode a Yak as a JSon value.
--
-- >>> toString $ toJsonString [YakStep {commitId = "a", commitDate = (readTime defaultTimeLocale iso8601DateFormat "2013-01-30 18:36:07+0100"), cfd = [("a",1),("b",2)]}]
-- "[{\"commitDate\":\"2013-01-30T17:36:07Z\",\"commitId\":\"a\",\"cfd\":[[\"a\",1],[\"b\",2]]}]"
toJsonString :: Yak -> ByteString
toJsonString = G.encode

-- | Convert a TimeSeries to JSon
-- 
-- >>> let t1 = (readTime defaultTimeLocale iso8601DateFormat "2013-01-30 18:36:07+0100") :: UTCTime 
-- >>> let t2 = (readTime defaultTimeLocale iso8601DateFormat "2013-01-30 18:37:07+0100") :: UTCTime 
-- 
-- >>> toString $ toJsonTimeSeries $ toTimeSeries [YakStep {commitId = "a", commitDate = t1, cfd = [("a",1),("b",2)]}, YakStep {commitId = "a", commitDate = t2, cfd = [("a",1),("b",3),("c",1)]}] 
-- "{\"a\":[[1359567367000,1],[1359567427000,1]],\"c\":[[1359567367000,0],[1359567427000,1]],\"b\":[[1359567367000,2],[1359567427000,3]]}"
toJsonTimeSeries :: [(String, TimeSeries)] -> ByteString
toJsonTimeSeries = encode . toJSON . map convertUTCTimeToMs

convertUTCTimeToMs :: (String, TimeSeries) -> (String, [(Integer,Int)])
convertUTCTimeToMs (s,t) = (s, reverse$ map (round.(1000*).utcTimeToPOSIXSeconds *** id) t)

type TimeSeries = [(UTCTime,Int)]

instance ToJSON [(String,[(Integer,Int)])] where
  toJSON = object . map (\ x -> (pack $fst x) .= snd x)
     
-- |Format a Yak as a map from phase name to (time, value) series.
-- 
toTimeSeries :: Yak -> [(String, TimeSeries)]
toTimeSeries yak = let phases = map (,[]) $ collectPhases yak
                   in  foldl (flip appendTimeAndValue) phases yak
                       
-- | Append a step's values to the given phases
-- 
appendTimeAndValue :: YakStep -> [(String, TimeSeries)] -> [(String, TimeSeries)]
appendTimeAndValue (YakStep { commitDate, cfd }) phases = map (appendWithDefault cfd) phases 
  where
    appendWithDefault :: Cfd -> (String, TimeSeries) -> (String, TimeSeries)
    appendWithDefault cfds p = case lookup (fst p) cfds of
      Just v  -> update (commitDate, v) p
      Nothing -> update (commitDate, 0) p
    update :: (UTCTime, Int) -> (String, TimeSeries) -> (String, TimeSeries)
    update x p = (fst p, x : snd p)
    
-- | Collect all known phases names for this Yak.
--
-- >>> let t1 = (readTime defaultTimeLocale iso8601DateFormat "2013-01-30 18:36:07+0100") :: UTCTime 
-- >>> let t2 = (readTime defaultTimeLocale iso8601DateFormat "2013-01-30 18:37:07+0100") :: UTCTime 
-- 
-- >>> collectPhases [YakStep {commitId = "a", commitDate = t1, cfd = [("a",1),("b",2)]}, YakStep {commitId = "a", commitDate = t2, cfd = [("a",1),("b",3),("c",1)]}]
-- ["a","b","c"]
collectPhases :: Yak -> [String]
collectPhases = foldl (\ p y ->  p `union` (map fst (cfd y))) []
