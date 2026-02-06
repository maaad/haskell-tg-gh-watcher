{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Watchdog
  ( runWatchdog
  ) where

import qualified Data.ByteString.Char8    as BS
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.STM   (readTVarIO)
import           Control.Monad            (void, when)

import           App.Env                  (AppEnv (..))
import           Bot.Telegram.Types       (Env (..))
import           Bot.Telegram.Messages    (sendTextMessage)
import           Logging                  (logInfo)
import           Config.AppM              (AppM, liftIOApp, runAppM)
import           Config.Watch             (databaseFile, databaseBackupFile,
                                           watchdogSleepSeconds)
import           DB.Database              (backupDatabase, iterateRepos)
import           Domain.RepositoryCheck   (CheckOrigin (..),
                                           RepositoryCheckContext (..),
                                           checkAndNotifyRepository)

runWatchdog :: Env -> AppM ()
runWatchdog env = do
  let appEnv  = envAppEnv env
      conn    = appDbConn appEnv
      logger  = appLogger appEnv
      flagVar = appWatchdogFlag appEnv
      ctx     =
        RepositoryCheckContext
          { rccConnection = conn
          , rccLogger     = logger
          , rccOrigin     = OriginWatchdog
          }
      sendFn uid text = liftIOApp (sendTextMessage env uid text)
      loop = do
        keepRunning <- liftIOApp (readTVarIO flagVar)
        when keepRunning $ do
          liftIOApp
            ( iterateRepos conn $ \repo ->
                void (runAppM appEnv (checkAndNotifyRepository ctx sendFn repo))
            )
          liftIOApp (backupDatabase databaseFile databaseBackupFile)
          liftIOApp
            ( logInfo
                logger
                (BS.pack "Watchdog created database backup and is sleeping until next hour")
            )
          liftIOApp (threadDelay (watchdogSleepSeconds * 1000000))
          loop
  loop

