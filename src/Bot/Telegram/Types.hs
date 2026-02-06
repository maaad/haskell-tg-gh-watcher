{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Types
  ( Env (..)
  ) where

import           App.Env          (AppEnv)
import qualified Telegram.Bot.API as TG

data Env = Env
  { envAppEnv :: AppEnv
  , envToken  :: TG.Token
  }

