{-# LANGUAGE OverloadedStrings #-}

module Logging
  ( Logger
  , withLogger
  , logInfo
  , logWarn
  , logError
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.ByteString.Char8    as BS
import           System.Log.FastLogger

newtype Logger = Logger { unLogger :: TimedFastLogger }

withLogger :: (Logger -> IO a) -> IO a
withLogger action = do
  timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
  let settings = LogStdout defaultBufSize
  withTimedFastLogger timeCache settings $ \tfl ->
    action (Logger tfl)

logWithLevel :: MonadIO m => Logger -> BS.ByteString -> BS.ByteString -> m ()
logWithLevel (Logger tfl) level msg = liftIO $
  tfl (\timeStr -> toLogStr timeStr <> " [" <> toLogStr level <> "] " <> toLogStr msg <> "\n")

logInfo :: MonadIO m => Logger -> BS.ByteString -> m ()
logInfo logger = logWithLevel logger "INFO"

logWarn :: MonadIO m => Logger -> BS.ByteString -> m ()
logWarn logger = logWithLevel logger "WARN"

logError :: MonadIO m => Logger -> BS.ByteString -> m ()
logError logger = logWithLevel logger "ERROR"

