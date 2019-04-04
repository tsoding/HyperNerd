{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Periodic
  ( startPeriodicCommands
  , addPeriodicCommand
  , removePeriodicCommand
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

mrbotka :: Sender
mrbotka =
  Sender
    { senderName = "mrbotka"
    , senderDisplayName = "MrBotka"
    , senderChannel = TwitchChannel "#tsoding"
    , senderRoles = [TwitchSub, TwitchMod, TwitchBroadcaster, TwitchBotOwner]
    , senderId = ""
    }

-- TODO(#485): Periodic commands have no channel to send them to
newtype PeriodicCommand = PeriodicCommand
  { periodicCommand :: Command T.Text
  }

instance IsEntity PeriodicCommand where
  nameOfEntity _ = "PeriodicCommand"
  toProperties pc =
    M.fromList
      [ ("name", PropertyText $ commandName command)
      , ("args", PropertyText $ commandArgs command)
      ]
    where
      command = periodicCommand pc
  fromProperties properties =
    PeriodicCommand <$>
    (Command <$> extractProperty "name" properties <*>
     extractProperty "args" properties)

getPeriodicCommandByName :: T.Text -> Effect (Maybe (Entity PeriodicCommand))
getPeriodicCommandByName name =
  fmap listToMaybe $
  selectEntities "PeriodicCommand" $
  Take 1 $ Filter (PropertyEquals "name" (PropertyText name)) All

startPeriodicCommands ::
     Channel -> (Message (Command T.Text) -> Effect ()) -> Effect ()
startPeriodicCommands channel dispatchCommand = do
  maybePc <-
    fmap listToMaybe $ selectEntities "PeriodicCommand" $ Take 1 $ Shuffle All
  maybe
    (return ())
    (dispatchCommand .
     Message (mrbotka {senderChannel = channel}) False .
     periodicCommand . entityPayload)
    maybePc
  timeout (10 * 60 * 1000) $ startPeriodicCommands channel dispatchCommand

addPeriodicCommand :: Reaction Message (Command T.Text)
addPeriodicCommand =
  Reaction $ \Message { messageSender = sender
                      , messageContent = command@Command {commandName = name}
                      } -> do
    maybePc <- getPeriodicCommandByName name
    case maybePc of
      Just _ ->
        replyToSender sender [qms|'{name}' is aleady called periodically|]
      Nothing -> do
        void $ createEntity $ PeriodicCommand command
        replyToSender
          sender
          [qms|'{name}' has been scheduled to call periodically|]

removePeriodicCommand :: Reaction Message T.Text
removePeriodicCommand =
  Reaction $ \Message {messageSender = sender, messageContent = name} -> do
    maybePc <- getPeriodicCommandByName name
    case maybePc of
      Just _ -> do
        void $
          deleteEntities "PeriodicCommand" $
          Filter (PropertyEquals "name" $ PropertyText name) All
        replyToSender sender [qms|'{name}' has been unscheduled|]
      Nothing ->
        replyToSender sender [qms|'{name}' was not scheduled to begin with|]
