module Domain.Types
  ( AdminUserId (..)
  , ChatId (..)
  ) where

import           Data.Int (Int64)

newtype AdminUserId = AdminUserId
  { unAdminUserId :: Int64
  }

newtype ChatId = ChatId
  { unChatId :: Int64
  }

