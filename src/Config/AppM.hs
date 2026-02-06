{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config.AppM
  ( AppM (..)
  , runAppM
  , liftIOApp
  ) where

import           App.Env                (AppEnv)
import           Control.Error          (AppError)
import           Control.Monad.Except   (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)

newtype AppM a = AppM
  { unAppM :: ReaderT AppEnv (ExceptT AppError IO) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    , MonadError AppError
    )

runAppM :: AppEnv -> AppM a -> IO (Either AppError a)
runAppM env (AppM m) =
  runExceptT (runReaderT m env)

liftIOApp :: IO a -> AppM a
liftIOApp = liftIO

