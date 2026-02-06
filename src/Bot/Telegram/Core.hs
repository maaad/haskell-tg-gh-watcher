{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Core
  ( runBotAppM
  , botLoopM
  ) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Monad            (void)
import qualified Data.ByteString.Char8    as BS
import           Data.Int                 (Int64)
import qualified Data.Text                as T

import           App.Env                  (AppEnv (..), isAdmin)
import           Bot.Telegram.Types       (Env (..))
import           Bot.Telegram.Commands    (dispatchCommand)
import           Bot.Telegram.Watchdog    (runWatchdog)
import           Config.AppM              (AppM, liftIOApp, runAppM)
import           Control.Monad.Reader     (ask)
import           Domain.Types             (ChatId (..))
import           Logging                  (logWarn)

import qualified Telegram.Bot.API         as TG

runBotAppM :: String -> AppM ()
runBotAppM token = do
  appEnv <- ask
  let tok =
        TG.Token
          (T.pack (trim token))
      env =
        Env
          { envAppEnv = appEnv
          , envToken  = tok
          }
  -- Запускаем watchdog в фоне.
  void
    ( liftIOApp
        ( async
            ( do
                _ <- runAppM appEnv (runWatchdog env)
                pure ()
            )
        )
    )
  botLoopM env Nothing

botLoopM :: Env -> Maybe TG.UpdateId -> AppM ()
botLoopM env mOffset = do
  let logger = appLogger (envAppEnv env)
      tok    = envToken env
      baseReq =
        TG.defGetUpdates
          { TG.getUpdatesOffset        = mOffset
          , TG.getUpdatesTimeout       = Nothing
          , TG.getUpdatesAllowedUpdates = Just [TG.UpdateMessage]
          }
  result <-
    liftIOApp
      (TG.defaultRunBot tok (TG.getUpdates baseReq))
  case result of
    Left err -> do
      liftIOApp
        (logWarn logger (BS.pack ("getUpdates failed: " <> show err)))
      liftIOApp (threadDelaySeconds 10)
      botLoopM env mOffset
    Right resp -> do
      let updates = TG.responseResult resp
      mapM_ (handleUpdateM env) updates
      let newOffset =
            case updates of
              [] -> mOffset
              _  ->
                let TG.UpdateId lastRaw = TG.updateUpdateId (last updates)
                in Just (TG.UpdateId (lastRaw + 1))
      botLoopM env newOffset

handleUpdateM :: Env -> TG.Update -> AppM ()
handleUpdateM env upd =
  case TG.updateMessage upd of
    Just msg -> handleMessageM env msg
    Nothing  -> pure ()

handleMessageM :: Env -> TG.Message -> AppM ()
handleMessageM env msg =
  case TG.messageFrom msg of
    Nothing       -> pure ()
    Just fromUser -> do
      let TG.UserId uidRaw = TG.userId fromUser
          uid :: Int64
          uid = fromIntegral uidRaw
          isBotSender  = TG.userIsBot fromUser
          chat          = TG.messageChat msg
          isPrivateChat =
            case TG.chatType chat of
              TG.ChatTypePrivate -> True
              _                  -> False
          chatId       = ChatId uid
      if isBotSender || not isPrivateChat || not (isAdmin (envAppEnv env) chatId)
        then pure ()
        else
          dispatchCommand env msg

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds n =
  threadDelay (n * 1000000)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== '\n')

