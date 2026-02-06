module App.Env
  ( AppEnv (..)
  , isAdmin
  ) where

import           Control.Concurrent.STM (TVar)
import           Database.SQLite.Simple (Connection)

import           Domain.Types           (AdminUserId (..), ChatId (..))
import           Logging                (Logger)

data AppEnv = AppEnv
  { appDbConn       :: Connection
  , appAdminUserId  :: AdminUserId
  , appWatchdogFlag :: TVar Bool
  , appLogger       :: Logger
  }

isAdmin :: AppEnv -> ChatId -> Bool
isAdmin appEnv (ChatId chatId) =
  chatId == unAdminUserId (appAdminUserId appEnv)

