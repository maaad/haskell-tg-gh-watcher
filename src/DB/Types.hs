module DB.Types where

import           Data.Int       (Int64)
import           Data.Text      (Text)
import           Data.Time      (UTCTime)

data Repository = Repository
  { repoId             :: Int64
  , repoFullName       :: Text
  , repoWatcherId      :: Int64
  , repoStargazers     :: Int64
  , repoWatchers       :: Int64
  , repoOpenIssues     :: Int64
  , repoPulls          :: Int64
  , repoForks          :: Int64
  , repoCreatedAt      :: UTCTime
  , repoUpdatedAt      :: UTCTime
  } deriving (Show)

data LogEntry = LogEntry
  { logId        :: Int64
  , logLevel     :: Text
  , logMessage   :: Text
  , logPayload   :: Maybe Text
  , logCreatedAt :: UTCTime
  } deriving (Show)

