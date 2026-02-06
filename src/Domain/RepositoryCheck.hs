{-# LANGUAGE OverloadedStrings #-}

module Domain.RepositoryCheck
  ( RepositoryCheckContext (..)
  , CheckOrigin (..)
  , checkAndNotifyRepository
  , computeChanges
  ) where

import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (when)
import qualified Data.ByteString.Char8          as BS
import           Data.Int                       (Int64)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (getCurrentTime)
import           Database.SQLite.Simple         (Connection)

import           API.GitHub                     (fetchRepository)
import           Config.AppM                    (AppM, liftIOApp)
import           Config.Watch                   (perRepositoryDelaySeconds)
import           Control.Error                  (MonadError (catchError),
                                                 getErrorContext,
                                                 getErrorMessage)
import           DB.Database                    (updateRepo)
import qualified DB.Types                       as DB
import           Logging                        (Logger, logError)

data CheckOrigin
  = OriginManualCheck
  | OriginWatchdog

data RepositoryCheckContext = RepositoryCheckContext
  { rccConnection :: Connection
  , rccLogger     :: Logger
  , rccOrigin     :: CheckOrigin
  }

checkAndNotifyRepository
  :: RepositoryCheckContext
  -> (Int64 -> Text -> AppM ())
  -> DB.Repository
  -> AppM ()
checkAndNotifyRepository ctx sendText repo =
  handleGitHubErrors ctx repo $
    do
      let conn   = rccConnection ctx
          origin = rccOrigin ctx
      remoteRepo <-
        fetchRepository
          (DB.repoWatcherId repo)
          (DB.repoFullName repo)
      let changes = computeChanges repo remoteRepo
      when (not (null changes)) $
        sendText
          (DB.repoWatcherId repo)
          ( renderPrefix origin
              <> DB.repoFullName repo
              <> ":\n"
              <> T.intercalate "\n" changes
          )
      now <- liftIOApp getCurrentTime
      let updated =
            remoteRepo
              { DB.repoId        = DB.repoId repo
              , DB.repoUpdatedAt = now
              }
      liftIOApp (updateRepo conn updated)
      liftIOApp (threadDelay (perRepositoryDelaySeconds * 1000000))

handleGitHubErrors
  :: RepositoryCheckContext
  -> DB.Repository
  -> AppM ()
  -> AppM ()
handleGitHubErrors ctx repo action =
  action
    `catchError` \appErr -> do
      let logger  = rccLogger ctx
          origin  = rccOrigin ctx
          context = getErrorContext appErr
      case context of
        "GitHub.RateLimit" ->
          case origin of
            OriginManualCheck ->
              sendRateLimitMessage repo
            OriginWatchdog ->
              pure ()
        "GitHub.NotFound" ->
          pure ()
        _ ->
          liftIOApp
            ( logError
                logger
                (BS.pack (T.unpack (getErrorMessage appErr)))
            )
  where
    sendRateLimitMessage :: DB.Repository -> AppM ()
    sendRateLimitMessage _ =
      pure ()

type RepoMetric = (Text, DB.Repository -> Int64)

repoMetrics :: [RepoMetric]
repoMetrics =
  [ ("Stars", DB.repoStargazers)
  , ("Watchers", DB.repoWatchers)
  , ("Issues", DB.repoOpenIssues)
  , ("Pull requests", DB.repoPulls)
  , ("Forks", DB.repoForks)
  ]

computeChanges :: DB.Repository -> DB.Repository -> [Text]
computeChanges oldRepo newRepo =
  [ formatMetricDelta name (getter oldRepo) (getter newRepo)
  | (name, getter) <- repoMetrics
  , getter newRepo /= getter oldRepo
  ]

renderPrefix :: CheckOrigin -> Text
renderPrefix origin =
  case origin of
    OriginManualCheck -> "[check] Changes in "
    OriginWatchdog    -> "New changes in "

formatMetricDelta :: Text -> Int64 -> Int64 -> Text
formatMetricDelta name oldValue newValue =
  let delta     = newValue - oldValue
      deltaText =
        if delta > 0
          then "+" <> T.pack (show delta)
          else T.pack (show delta)
  in "- " <> name <> ": " <> T.pack (show oldValue) <> " → " <> T.pack (show newValue) <> " (" <> deltaText <> ")"

