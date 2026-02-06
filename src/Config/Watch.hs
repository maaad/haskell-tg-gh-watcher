module Config.Watch
  ( perRepositoryDelaySeconds
  , watchdogSleepSeconds
  , databaseFile
  , databaseBackupFile
  ) where

perRepositoryDelaySeconds :: Int
perRepositoryDelaySeconds =
  1

watchdogSleepSeconds :: Int
watchdogSleepSeconds =
  60 * 60

databaseFile :: FilePath
databaseFile =
  "git-watcher-bot.db"

databaseBackupFile :: FilePath
databaseBackupFile =
  "git-watcher-bot.db.bak"

