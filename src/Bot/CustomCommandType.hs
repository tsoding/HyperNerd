{-# LANGUAGE OverloadedStrings #-}

-- | Moved out of CustomCommand to break dependency cycle:
--   Help depends on this type, but custom commands needs
--   to refresh Help and therefore also this type.
module Bot.CustomCommandType
  ( CustomCommand(..)
  ) where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Entity
import Property

data CustomCommand = CustomCommand
  { customCommandName :: T.Text
  , customCommandMessage :: T.Text
  , customCommandTimes :: Int
  }

instance IsEntity CustomCommand where
  nameOfEntity _ = "CustomCommand"
  toProperties customCommand =
    M.fromList
      [ ("name", PropertyText $ customCommandName customCommand)
      , ("message", PropertyText $ customCommandMessage customCommand)
      , ("times", PropertyInt $ customCommandTimes customCommand)
      ]
  fromProperties properties =
    CustomCommand <$> extractProperty "name" properties <*>
    extractProperty "message" properties <*>
    pure (fromMaybe 0 $ extractProperty "times" properties)
