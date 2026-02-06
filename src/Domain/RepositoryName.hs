module Domain.RepositoryName
  ( RepositoryFullName (..)
  , parseRepositoryFullName
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

newtype RepositoryFullName = RepositoryFullName
  { unRepositoryFullName :: Text
  }
  deriving (Eq, Show)

parseRepositoryFullName :: Text -> Maybe RepositoryFullName
parseRepositoryFullName txt
  | T.pack "github.com/" `T.isInfixOf` txt =
      let parts = T.splitOn (T.pack "github.com/") txt
          raw   =
            case reverse parts of
              (p:_) -> p
              []    -> txt
          firstToken =
            case T.words raw of
              (w:_) -> w
              []    -> raw
          cleaned = T.strip firstToken
      in if T.pack "/" `T.isInfixOf` cleaned
           then Just (RepositoryFullName cleaned)
           else Nothing
  | T.pack "/" `T.isInfixOf` txt =
      Just (RepositoryFullName (T.strip txt))
  | otherwise =
      Nothing

