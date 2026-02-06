{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Commands
  ( dispatchCommand
  ) where

import           Control.Monad            (void)
import qualified Data.Text                as T

import           API.GitHub               (fetchRepository)
import           App.Env                  (AppEnv (..))
import           Bot.Telegram.Types       (Env (..))
import           Bot.Telegram.Messages    (sendTextMessage)
import           Config.AppM              (AppM, liftIOApp, runAppM)
import           Domain.Types             (AdminUserId (..))
import           DB.Database
import qualified DB.Types                 as DB
import           Domain.RepositoryCheck   (CheckOrigin (..),
                                           RepositoryCheckContext (..),
                                           checkAndNotifyRepository)
import           Domain.RepositoryName    (RepositoryFullName (..),
                                           parseRepositoryFullName)

import qualified Telegram.Bot.API         as TG

dispatchCommand :: Env -> TG.Message -> AppM ()
dispatchCommand env msg =
  case TG.messageText msg of
    Just txt
      | "/start" `T.isPrefixOf` txt        -> onStartCommand env msg
      | "/my_repos" `T.isPrefixOf` txt     -> onMyReposCommand env msg
      | "/watch_repo" `T.isPrefixOf` txt   -> onWatchRepoCommand env msg
      | "/unwatch_repo" `T.isPrefixOf` txt -> onUnwatchRepoCommand env msg
      | "/check" `T.isPrefixOf` txt        -> onCheckCommand env
      | otherwise                          -> onNonCommandMessage env msg
    _ -> pure ()

onStartCommand :: Env -> TG.Message -> AppM ()
onStartCommand env msg = do
  let adminId = unAdminUserId (appAdminUserId (envAppEnv env))
  case TG.messageFrom msg of
    Nothing -> pure ()
    Just _ ->
      liftIOApp
        ( sendTextMessage
            env
            adminId
            "Welcome! Send a repository full name to add to the watch list. Example: torvalds/linux or https://github.com/torvalds/linux"
        )

onWatchRepoCommand :: Env -> TG.Message -> AppM ()
onWatchRepoCommand env msg = do
  let appEnv  = envAppEnv env
      adminId = unAdminUserId (appAdminUserId appEnv)
  case TG.messageFrom msg of
    Nothing -> pure ()
    Just _ ->
      liftIOApp
        ( sendTextMessage
            env
            adminId
            "Send a repository full name to add to the watch list. Example: torvalds/linux or https://github.com/torvalds/linux"
        )

onMyReposCommand :: Env -> TG.Message -> AppM ()
onMyReposCommand env msg = do
  let appEnv  = envAppEnv env
      conn    = appDbConn appEnv
      adminId = unAdminUserId (appAdminUserId appEnv)
  case TG.messageFrom msg of
    Nothing -> pure ()
    Just _ -> do
      fullnames <- liftIOApp (getUserReposFullnames conn adminId)
      if null fullnames
        then liftIOApp (sendTextMessage env adminId "Watch list is empty.")
        else do
          let header =
                "Watching "
                  <> T.pack (show (length fullnames))
                  <> " repositories:\n"
              body = T.concat [ "- " <> name <> "\n" | name <- fullnames ]
          liftIOApp (sendTextMessage env adminId (header <> body))

onUnwatchRepoCommand :: Env -> TG.Message -> AppM ()
onUnwatchRepoCommand env msg = do
  let appEnv  = envAppEnv env
      conn    = appDbConn appEnv
      adminId = unAdminUserId (appAdminUserId appEnv)
  case TG.messageFrom msg of
    Nothing -> pure ()
    Just _ -> do
      repos <- liftIOApp (getUserRepos conn adminId)
      if null repos
        then liftIOApp (sendTextMessage env adminId "Watch list is empty.")
        else pure () -- Simplified: inline keyboard handling omitted.

onCheckCommand :: Env -> AppM ()
onCheckCommand env = do
  let appEnv    = envAppEnv env
      conn      = appDbConn appEnv
      logger    = appLogger appEnv
      ctx       =
        RepositoryCheckContext
          { rccConnection = conn
          , rccLogger     = logger
          , rccOrigin     = OriginManualCheck
          }
      sendFn uid text = liftIOApp (sendTextMessage env uid text)
  liftIOApp (sendTextMessage env (unAdminUserId (appAdminUserId appEnv)) "Manual /check started...")
  liftIOApp
    ( iterateRepos conn $ \repo ->
        void (runAppM appEnv (checkAndNotifyRepository ctx sendFn repo))
    )
  liftIOApp (sendTextMessage env (unAdminUserId (appAdminUserId appEnv)) "Manual /check finished.")

onNonCommandMessage :: Env -> TG.Message -> AppM ()
onNonCommandMessage env msg = do
  let appEnv  = envAppEnv env
      conn    = appDbConn appEnv
      adminId = unAdminUserId (appAdminUserId appEnv)
  case TG.messageFrom msg of
    Nothing -> pure ()
    Just _ ->
      case TG.messageText msg of
        Nothing -> pure ()
        Just txt -> do
          let mFullName = parseRepositoryFullName txt
          case mFullName of
            Nothing -> pure ()
            Just (RepositoryFullName fullName) -> do
              exists <- liftIOApp (repoExistsByFullName conn fullName)
              if exists
                then
                  liftIOApp
                    ( sendTextMessage
                        env
                        adminId
                        ( "Repository "
                            <> fullName
                            <> " is already in the watch list."
                        )
                    )
                else do
                  repo <- fetchRepository adminId fullName
                  liftIOApp (addRepo conn repo)
                  liftIOApp
                    ( sendTextMessage
                        env
                        adminId
                        ( "Repository "
                            <> DB.repoFullName repo
                            <> " added to watch list."
                        )
                    )
