{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Alias
  ( redirectAlias
  , addAliasCommand
  , removeAliasCommand
  ) where

import Bot.Replies
import Command
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Effect
import Entity
import Property
import Reaction
import Text.InterpolatedString.QM
import Transport
import Data.Proxy

data Alias = Alias
  { aliasName :: T.Text
  , aliasRedirect :: T.Text
  }

instance IsEntity Alias where
  nameOfEntity _ = "Alias"
  toProperties alias =
    M.fromList
      [ ("name", PropertyText $ aliasName alias)
      , ("redirect", PropertyText $ aliasRedirect alias)
      ]
  fromProperties properties =
    Alias <$> extractProperty "name" properties <*>
    extractProperty "redirect" properties

getAliasByName :: T.Text -> Effect (Maybe Alias)
getAliasByName name =
  fmap entityPayload . listToMaybe <$>
  selectEntities
    Proxy
    (Take 1 $ Filter (PropertyEquals "name" (PropertyText name)) All)

redirectAlias :: Command a -> Effect (Command a)
redirectAlias command = do
  alias <- getAliasByName $ commandName command
  return $ maybe command (renameCommand command . aliasRedirect) alias

addAliasCommand :: Reaction Message (T.Text, T.Text)
addAliasCommand =
  Reaction $ \Message { messageSender = sender
                      , messageContent = (name, redirect)
                      } ->
    if name == redirect
      then replyToSender sender "Alias cannot redirect to itself"
      else do
        alias <- getAliasByName name
        case alias of
          Just _ -> replyToSender sender [qms|Alias '{name}' already exists|]
          Nothing -> do
            void $
              createEntity
                Proxy
                Alias {aliasName = name, aliasRedirect = redirect}
            replyToSender sender [qms|Alias '{name}' has been created|]

removeAliasCommand :: Reaction Message T.Text
removeAliasCommand =
  Reaction $ \Message {messageSender = sender, messageContent = name} -> do
    alias <- getAliasByName name
    case alias of
      Just _ -> do
        void $
          deleteEntities
            (Proxy :: Proxy Alias)
            (Filter (PropertyEquals "name" (PropertyText name)) All)
        replyToSender sender [qms|Alias '{name}' has been removed|]
      Nothing -> replyToSender sender [qms|Alias '{name}' does not exists"|]
