{-# LANGUAGE OverloadedStrings #-}

module Bot.BotUserInfo where

import qualified Data.Text as T
import Entity
import qualified Data.Map as M
import Effect
import Property
import Data.Maybe
import Data.Functor

newtype BotUserInfo = BotUserInfo { buiNickname :: T.Text }

renameBui :: T.Text -> BotUserInfo -> BotUserInfo
renameBui nickname bui = bui { buiNickname = nickname }

instance IsEntity BotUserInfo where
  toProperties bui = M.fromList [("nickname", PropertyText $ buiNickname bui)]
  fromProperties properties =
    BotUserInfo <$> extractProperty "nickname" properties

botUserInfo :: Effect (Maybe (Entity BotUserInfo))
botUserInfo = listToMaybe <$> selectEntities "BotUserInfo" All

updateBotUserInfo :: T.Text -> Effect ()
updateBotUserInfo nickname = do
  bui <- botUserInfo
  case bui of
    Nothing -> void $ createEntity "BotUserInfo" $ BotUserInfo nickname
    Just bui' -> void $ updateEntityById $ fmap (renameBui nickname) $ bui'
