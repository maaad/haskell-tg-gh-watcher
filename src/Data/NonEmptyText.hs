module Data.NonEmptyText
  ( NonEmptyText (..)
  , isNonEmptyText
  , mkNonEmptyText
  ) where

import           Data.String (IsString (..))
import           Data.Text   (Text)
import qualified Data.Text   as T

newtype NonEmptyText = NonEmptyText
  { unNonEmptyText :: Text
  }
  deriving (Eq, Ord, Show)

instance IsString NonEmptyText where
  fromString s = NonEmptyText (T.pack s)

isNonEmptyText :: Text -> Bool
isNonEmptyText t = not (T.null t)

mkNonEmptyText :: Text -> Maybe NonEmptyText
mkNonEmptyText t
  | isNonEmptyText t = Just (NonEmptyText t)
  | otherwise        = Nothing

