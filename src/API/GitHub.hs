{-# LANGUAGE OverloadedStrings #-}

module API.GitHub
  ( fetchRepository
  ) where

import           Control.Exception         (SomeException, try)
import           Data.Aeson
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Int                  (Int64)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (getCurrentTime)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types        (RequestHeaders)
import           Network.HTTP.Types.Status (statusCode)
import           System.Environment        (lookupEnv)

import           Config.AppM               (AppM, liftIOApp)
import           Control.Error             (MonadError (throwError),
                                            mkAppError)
import           DB.Types

throwGitHubError :: Text -> Text -> [(Text, Text)] -> AppM a
throwGitHubError context message details =
  throwError (mkAppError message context (Map.fromList details))

data GitHubRepoPayload = GitHubRepoPayload
  { grFullName        :: Text
  , grStargazersCount :: Int64
  , grWatchersCount   :: Int64
  , grOpenIssuesCount :: Int64
  , grForksCount      :: Int64
  } deriving (Show)

instance FromJSON GitHubRepoPayload where
  parseJSON = withObject "GitHubRepoPayload" $ \o ->
    GitHubRepoPayload
      <$> o .: "full_name"
      <*> o .: "stargazers_count"
      <*> o .: "watchers_count"
      <*> o .: "open_issues_count"
      <*> o .: "forks_count"

fetchRepository :: Int64 -> Text -> AppM Repository
fetchRepository watcherId fullName = do
  manager <- liftIOApp (newManager tlsManagerSettings)
  mToken  <- liftIOApp (lookupEnv "GITHUB_TOKEN")
  initialRequest <-
    liftIOApp
      (parseRequest ("https://api.github.com/repos/" <> T.unpack fullName))
  let baseHeaders =
        [ ("User-Agent", "git-watcher-bot-hs")
        , ("Accept"    , "application/vnd.github+json")
        ]
      authHeader =
        case mToken of
          Just tok | not (null tok) ->
            [("Authorization", BS.append "Bearer " (BS.pack tok))]
          _ -> []
      request =
        initialRequest
          { requestHeaders = baseHeaders <> authHeader
          }
  responseResult <-
    liftIOApp
      ( try (httpLbs request manager)
          :: IO (Either SomeException (Response LBS.ByteString))
      )
  case responseResult of
    Left ex ->
      throwGitHubError
        "API.GitHub.fetchRepository"
        "GitHub request failed"
        [ ("exception", T.pack (show ex)), ("repository", fullName) ]
    Right response -> do
      let st     = responseStatus response
          body   = responseBody response
          bodyBS = LBS.toStrict body
          repoDetail = Map.singleton "repository" fullName
      case statusCode st of
        404 ->
          throwGitHubError "GitHub.NotFound" "GitHub repository not found" (Map.toList repoDetail)
        403 | "rate limit" `BS.isInfixOf` bodyBS ->
          throwGitHubError "GitHub.RateLimit" "GitHub API rate limit exceeded" (Map.toList repoDetail)
        _ ->
          case eitherDecode body of
            Left err ->
              throwGitHubError
                "API.GitHub.fetchRepository"
                "Failed to decode GitHub repository payload"
                [ ("error", T.pack err), ("repository", fullName) ]
            Right payload -> do
              now <- liftIOApp getCurrentTime
              pullsCount <-
                fetchOpenPullsCount
                  manager
                  (requestHeaders request)
                  fullName
              let issuesOnly = max 0 (grOpenIssuesCount payload - pullsCount)
              pure
                Repository
                  { repoId         = 0
                  , repoFullName   = grFullName payload
                  , repoWatcherId  = watcherId
                  , repoStargazers = grStargazersCount payload
                  , repoWatchers   = grWatchersCount payload
                  , repoOpenIssues = issuesOnly
                  , repoPulls      = pullsCount
                  , repoForks      = grForksCount payload
                  , repoCreatedAt  = now
                  , repoUpdatedAt  = now
                  }

fetchOpenPullsCount :: Manager -> RequestHeaders -> Text -> AppM Int64
fetchOpenPullsCount manager baseHeaders fullName = do
  req0 <-
    liftIOApp
      ( parseRequest
          ( "https://api.github.com/repos/"
              <> T.unpack fullName
              <> "/pulls?state=open&per_page=100"
          )
      )
  let req =
        req0
          { requestHeaders = baseHeaders
          }
  responseResult <-
    liftIOApp
      ( try (httpLbs req manager)
          :: IO (Either SomeException (Response LBS.ByteString))
      )
  case responseResult of
    Left _ ->
      pure 0
    Right response ->
      case eitherDecode (responseBody response) of
        Left _   -> pure 0
        Right arr ->
          let pulls :: [Value]
              pulls = arr
          in pure (fromIntegral (length pulls))

