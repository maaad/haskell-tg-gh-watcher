module Control.Error
  ( AppError (..)
  , mkAppError
  , getErrorMessage
  , getErrorContext
  , getErrorDetails
  , MonadError (..)
  , withExceptT
  ) where

import           Control.Monad.Except (MonadError (..), withExceptT)
import           Data.Map             (Map)
import           Data.Text            (Text)

data AppError = AppError
  { appErrorMessage :: !Text
  , appErrorContext :: !Text
  , appErrorDetails :: !(Map Text Text)
  }
  deriving (Show)

mkAppError :: Text -> Text -> Map Text Text -> AppError
mkAppError message context details =
  AppError
    { appErrorMessage = message
    , appErrorContext = context
    , appErrorDetails = details
    }

getErrorMessage :: AppError -> Text
getErrorMessage = appErrorMessage

getErrorContext :: AppError -> Text
getErrorContext = appErrorContext

getErrorDetails :: AppError -> Map Text Text
getErrorDetails = appErrorDetails

