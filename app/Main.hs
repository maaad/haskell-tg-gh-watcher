module Main where

import           Control.Concurrent.Async    (async, cancel, wait)
import           Control.Concurrent.STM
import           Control.Monad               (void)
import qualified Data.ByteString.Char8       as BS
import           Data.Int                    (Int64)
import           Data.List                   (isPrefixOf)
import qualified Data.Text                   as T
import           System.Environment          (setEnv)
import           Database.SQLite.Simple      (open)
import           System.Posix.Signals
import           App.Env                     (AppEnv (..))
import           Bot.Telegram                (runBotAppM)
import           Config.Watch                (databaseFile)
import           Config.AppM                 (runAppM)
import           Control.Error               (getErrorMessage)
import           Domain.Types                (AdminUserId (..))
import           DB.Database                 (initDatabase)
import           Logging                     (withLogger, logError)

loadEnvConfig :: IO (String, Int64)
loadEnvConfig = do
  contents <- readFile ".env"
  let ls = lines contents
      lookupKey key =
        case [drop (length key + 1) l | l <- ls, (key ++ "=") `isPrefixOf` l] of
          (v:_) -> Just v
          []    -> Nothing
  botToken <-
    maybe (fail "BOT_TOKEN is not set in .env") pure (lookupKey "BOT_TOKEN")
  adminStr <-
    maybe (fail "ADMIN_USER_ID is not set in .env") pure (lookupKey "ADMIN_USER_ID")
  -- Optional GitHub token for higher rate limits and private repositories.
  case lookupKey "GITHUB_TOKEN" of
    Just ghTok -> setEnv "GITHUB_TOKEN" ghTok
    Nothing    -> pure ()
  pure (botToken, read adminStr)

main :: IO ()
main = do
  (botToken, adminUserId) <- loadEnvConfig

  withLogger $ \logger -> do
    dbConn <- open databaseFile
    initDatabase dbConn
    watchdogFlag <- newTVarIO True

    let env = AppEnv
          { appDbConn       = dbConn
          , appAdminUserId  = AdminUserId adminUserId
          , appWatchdogFlag = watchdogFlag
          , appLogger       = logger
          }

    -- Run bot and watchdog inside AppM
    let runBot = do
          runResult <- runAppM env (runBotAppM botToken)
          case runResult of
            Left appErr ->
              logError
                logger
                ( BS.pack
                    ( "Bot failed: "
                        <> T.unpack (getErrorMessage appErr)
                    )
                )
            Right _ ->
              pure ()

    botAsync      <- async runBot

    -- Simple signal handling: on SIGINT/SIGTERM stop watchdog and bot
    let stopAction = do
          atomically $ writeTVar watchdogFlag False
          cancel botAsync

    void $ installHandler keyboardSignal (Catch stopAction) Nothing
    void $ installHandler softwareTermination (Catch stopAction) Nothing

    -- Wait for bot to finish (or be cancelled)
    void $ wait botAsync

