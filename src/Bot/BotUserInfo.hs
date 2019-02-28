{-# LANGUAGE OverloadedStrings #-}

module Bot.BotUserInfo where

import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Effect
import Entity
import Property

-- TODO: BotUserInfo does not work in multi-channel setting
newtype BotUserInfo = BotUserInfo
  { buiNickname :: T.Text
  }

renameBui :: T.Text -> BotUserInfo -> BotUserInfo
renameBui nickname bui = bui {buiNickname = nickname}

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
    Just bui'
      | buiNickname (entityPayload bui') /= nickname ->
        void $ updateEntityById $ fmap (renameBui nickname) bui'
    _ -> return ()
