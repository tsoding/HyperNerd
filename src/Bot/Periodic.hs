{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Periodic
  ( startPeriodicCommands
  , addPeriodicCommand
  , removePeriodicCommand
  , enablePeriodicTimerCommand
  , disablePeriodicTimerCommand
  , statusPeriodicTimerCommand
  ) where

import Bot.Replies
import Command
import Control.Monad
import Data.Bool.Extra
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
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

newtype PeriodicTimer = PeriodicTimer
  { periodicTimerEnabled :: Bool
  }

periodicTimerEntity :: Effect (Entity PeriodicTimer)
periodicTimerEntity = do
  entity <- listToMaybe <$> selectEntities Proxy All
  maybe (createEntity Proxy $ PeriodicTimer True) return entity

instance IsEntity PeriodicTimer where
  nameOfEntity _ = "PeriodicTimer"
  toProperties pt =
    M.fromList [("enabled", PropertyInt $ boolAsInt $ periodicTimerEnabled pt)]
  fromProperties properties =
    PeriodicTimer <$> (intAsBool <$> extractProperty "enabled" properties)

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
  selectEntities Proxy $
  Take 1 $ Filter (PropertyEquals "name" (PropertyText name)) All

startPeriodicCommands ::
     Channel -> (Message (Command T.Text) -> Effect ()) -> Effect ()
startPeriodicCommands channel dispatchCommand = do
  maybePc <- fmap listToMaybe $ selectEntities Proxy $ Take 1 $ Shuffle All
  periodicTimer <- entityPayload <$> periodicTimerEntity
  when (periodicTimerEnabled periodicTimer) $
    maybe
      (return ())
      (dispatchCommand .
       Message (mrbotka {senderChannel = channel}) False .
       periodicCommand . entityPayload)
      maybePc
  timeout
    (10 * 60 * 1000)
    (Just channel)
    (startPeriodicCommands channel dispatchCommand)

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
        void $ createEntity Proxy $ PeriodicCommand command
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
          deleteEntities (Proxy :: Proxy PeriodicCommand) $
          Filter (PropertyEquals "name" $ PropertyText name) All
        replyToSender sender [qms|'{name}' has been unscheduled|]
      Nothing ->
        replyToSender sender [qms|'{name}' was not scheduled to begin with|]

enablePeriodicTimer :: PeriodicTimer -> PeriodicTimer
enablePeriodicTimer pt = pt {periodicTimerEnabled = True}

disablePeriodicTimer :: PeriodicTimer -> PeriodicTimer
disablePeriodicTimer pt = pt {periodicTimerEnabled = False}

enablePeriodicTimerCommand :: Reaction Message a
enablePeriodicTimerCommand =
  liftR (const periodicTimerEntity) $
  cmapR (fmap enablePeriodicTimer) $
  liftR updateEntityById $
  cmapR (const "Periodic timer has been enabled") $ Reaction replyMessage

disablePeriodicTimerCommand :: Reaction Message a
disablePeriodicTimerCommand =
  liftR (const periodicTimerEntity) $
  cmapR (fmap disablePeriodicTimer) $
  liftR updateEntityById $
  cmapR (const "Periodic timer has been disabled") $ Reaction replyMessage

statusPeriodicTimerCommand :: Reaction Message a
statusPeriodicTimerCommand =
  liftR (const periodicTimerEntity) $
  cmapR (T.pack . show . periodicTimerEnabled . entityPayload) $
  Reaction replyMessage
