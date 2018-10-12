{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Periodic ( startPeriodicCommands
                    , addPeriodicCommand
                    , removePeriodicCommand
                    ) where

import           Bot.Replies
import           Command
import           Control.Monad
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Entity
import           Events
import           Property
import           Text.InterpolatedString.QM

-- TODO(#238): Periodic command sender should be the bot itself
god :: Sender
god = Sender { senderName = "god"
             , senderChannel = "dimension10"
             , senderSubscriber = True
             , senderMod = True
             , senderBroadcaster = True
             }

newtype PeriodicCommand = PeriodicCommand { periodicCommand :: Command T.Text }

instance IsEntity PeriodicCommand where
    toProperties pc =
        M.fromList [ ("name", PropertyText $ commandName command)
                   , ("args", PropertyText $ commandArgs command)
                   ]
        where command = periodicCommand pc

    fromProperties properties =
        PeriodicCommand <$> (Command <$> extractProperty "name" properties
                                     <*> extractProperty "args" properties)

getPeriodicCommandByName :: T.Text -> Effect (Maybe (Entity PeriodicCommand))
getPeriodicCommandByName name =
  fmap listToMaybe $
  selectEntities "PeriodicCommand" $
  Take 1 $
  Filter (PropertyEquals "name" (PropertyText name)) All

startPeriodicCommands :: (Message (Command T.Text) -> Effect ()) -> Effect ()
startPeriodicCommands dispatchCommand = do
  maybePc <- fmap listToMaybe $
             selectEntities "PeriodicCommand" $
             Take 1 $
             Shuffle All
  maybe (return ())
        (dispatchCommand . Message god . periodicCommand . entityPayload)
        maybePc
  timeout (10 * 60 * 1000) $ startPeriodicCommands dispatchCommand

addPeriodicCommand :: CommandHandler (Command T.Text)
addPeriodicCommand Message { messageSender = sender
                           , messageContent = command@Command { commandName = name }
                           } = do
  maybePc <- getPeriodicCommandByName name
  case maybePc of
    Just _ -> replyToSender sender [qms|'{name}' is aleady
                                        called periodically|]
    Nothing -> do void $ createEntity "PeriodicCommand" $ PeriodicCommand command
                  replyToSender sender [qms|'{name}' has been scheduled
                                                 to call periodically|]


removePeriodicCommand :: CommandHandler T.Text
removePeriodicCommand Message { messageSender = sender
                              , messageContent = name
                              }= do
  maybePc <- getPeriodicCommandByName name
  case maybePc of
    Just _ -> do void $
                   deleteEntities "PeriodicCommand" $
                   Filter (PropertyEquals "name" $
                           PropertyText name) All
                 replyToSender sender [qms|'{name}' has been unscheduled|]
    Nothing -> replyToSender sender [qms|'{name}' was not scheduled to
                                         begin with|]
