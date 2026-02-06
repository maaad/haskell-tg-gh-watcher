{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DB.Database
  ( initDatabase
  , addRepo
  , updateRepo
  , repoExistsByFullName
  , getUserRepos
  , getUserReposFullnames
  , iterateRepos
  , backupDatabase
  ) where

import           Control.Monad              (forM_)
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import           Database.SQLite.Simple
import           System.Directory           (copyFile)

import           DB.Types

instance FromRow Repository where
  fromRow =
    Repository <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

initDatabase :: Connection -> IO ()
initDatabase conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS repositories (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \ full_name TEXT NOT NULL UNIQUE,\
    \ watcher_id INTEGER NOT NULL,\
    \ stargazers_count INTEGER NOT NULL,\
    \ watchers_count INTEGER NOT NULL,\
    \ open_issues_count INTEGER NOT NULL,\
    \ pulls_count INTEGER NOT NULL,\
    \ forks_count INTEGER NOT NULL,\
    \ created_at TEXT NOT NULL,\
    \ updated_at TEXT NOT NULL\
    \)"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS logs (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \ level TEXT NOT NULL,\
    \ message TEXT NOT NULL,\
    \ payload TEXT NULL,\
    \ created_at TEXT NOT NULL\
    \)"

addRepo :: Connection -> Repository -> IO ()
addRepo conn repo = do
  execute conn
    "INSERT INTO repositories (full_name, watcher_id, stargazers_count, watchers_count, open_issues_count, pulls_count, forks_count, created_at, updated_at)\
    \ VALUES (?,?,?,?,?,?,?,?,?)"
    ( repoFullName repo
    , repoWatcherId repo
    , repoStargazers repo
    , repoWatchers repo
    , repoOpenIssues repo
    , repoPulls repo
    , repoForks repo
    , repoCreatedAt repo
    , repoUpdatedAt repo
    )

updateRepo :: Connection -> Repository -> IO ()
updateRepo conn repo = do
  execute conn
    "UPDATE repositories SET stargazers_count = ?, watchers_count = ?, open_issues_count = ?, pulls_count = ?, forks_count = ?, updated_at = ?\
    \ WHERE id = ?"
    ( repoStargazers repo
    , repoWatchers repo
    , repoOpenIssues repo
    , repoPulls repo
    , repoForks repo
    , repoUpdatedAt repo
    , repoId repo
    )

repoExistsByFullName :: Connection -> Text -> IO Bool
repoExistsByFullName conn fullName = do
  rows <- query conn "SELECT 1 FROM repositories WHERE full_name = ? LIMIT 1" (Only fullName) :: IO [Only Int]
  pure (not (null rows))

getUserRepos :: Connection -> Int64 -> IO [Repository]
getUserRepos conn uid =
  query conn "SELECT id, full_name, watcher_id, stargazers_count, watchers_count, open_issues_count, pulls_count, forks_count, created_at, updated_at\
             \ FROM repositories WHERE watcher_id = ?" (Only uid)

getUserReposFullnames :: Connection -> Int64 -> IO [Text]
getUserReposFullnames conn uid = do
  rows <- query conn "SELECT full_name FROM repositories WHERE watcher_id = ?" (Only uid) :: IO [Only Text]
  pure (map fromOnly rows)

iterateRepos :: Connection -> (Repository -> IO ()) -> IO ()
iterateRepos conn f = do
  repos <- query_ conn "SELECT id, full_name, watcher_id, stargazers_count, watchers_count, open_issues_count, pulls_count, forks_count, created_at, updated_at FROM repositories"
  forM_ repos f

backupDatabase :: FilePath -> FilePath -> IO ()
backupDatabase src dst = copyFile src dst

