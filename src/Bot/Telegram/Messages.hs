{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Messages
  ( sendTextMessage
  , notifyAdmin
  ) where

import           Data.Int               (Int64)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.ByteString.Char8  as BS

import           App.Env                (AppEnv (..))
import           Bot.Telegram.Types     (Env (..))
import           Domain.Types           (AdminUserId (..))
import           Logging                (logError, logInfo)

import qualified Telegram.Bot.API       as TG

sendTextMessage :: Env -> Int64 -> Text -> IO ()
sendTextMessage env uid text = do
  let appEnv     = envAppEnv env
      logger     = appLogger appEnv
      tok        = envToken env
      chatId     = TG.ChatId (fromIntegral uid)
      req    = TG.defSendMessage (TG.SomeChatId chatId) text
  result <- TG.defaultRunBot tok (TG.sendMessage req)
  case result of
    Left err ->
      logError logger (BS.pack ("sendMessage failed: " <> show err))
    Right _ ->
      logInfo logger (BS.pack ("[SEND] to " <> show uid <> ": " <> T.unpack text))

notifyAdmin :: Env -> Text -> IO ()
notifyAdmin env msg =
  let appEnv = envAppEnv env
  in sendTextMessage env (unAdminUserId (appAdminUserId appEnv)) msg

