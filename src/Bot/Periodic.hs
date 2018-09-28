{-# LANGUAGE OverloadedStrings #-}
module Bot.Periodic ( startPeriodicCommands
                    , addPeriodicCommand
                    , removePeriodicCommand
                    ) where

import           Bot.Replies
import           Command
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Entity
import           Events
import           Property
import           Text.Printf

-- TODO(#238): Periodic command sender should be the bot itself
god :: Sender
god = Sender { senderName = "god"
             , senderChannel = "dimension10"
             , senderSubscriber = True
             , senderMod = True
             }

newtype PeriodicCommand = PeriodicCommand { periodicCommand :: Command T.Text }

instance IsEntity PeriodicCommand where
    toProperties pc =
        M.fromList [ ("name", PropertyText $ commandName command)
                   , ("args", PropertyText $ commandArgs command)
                   ]
        where command = periodicCommand pc

    fromProperties properties = do
        PeriodicCommand <$> (Command <$> extractProperty "name" properties
                                     <*> extractProperty "args" properties)

getPeriodicCommandByName :: T.Text -> Effect (Maybe (Entity PeriodicCommand))
getPeriodicCommandByName name =
  fmap listToMaybe $
  selectEntities "PeriodicCommand" $
  Take 1 $
  Filter (PropertyEquals "name" (PropertyText name)) All

startPeriodicCommands :: (Sender -> Command T.Text -> Effect ()) -> Effect ()
startPeriodicCommands dispatchCommand = do
  maybePc <- fmap listToMaybe $
             selectEntities "PeriodicCommand" $
             Take 1 $
             Shuffle All
  maybe (return ())
        (dispatchCommand god . periodicCommand . entityPayload)
        maybePc
  timeout (10 * 60 * 1000) $ startPeriodicCommands dispatchCommand

addPeriodicCommand :: CommandHandler (Command T.Text)
addPeriodicCommand sender command =
    do maybePc <- getPeriodicCommandByName name
       case maybePc of
         Just _ -> replyToSender sender $
                   T.pack $
                   printf "'%s' is aleady called periodically" name
         Nothing -> do _ <- createEntity "PeriodicCommand" (PeriodicCommand command)
                       replyToSender sender $
                         T.pack $
                         printf "'%s' has been scheduled to call periodically" name
    where name = commandName command

removePeriodicCommand :: CommandHandler T.Text
removePeriodicCommand sender name = do
  maybePc <- getPeriodicCommandByName name
  case maybePc of
    Just _ -> do _ <- deleteEntities "PeriodicCommand" $
                      Filter (PropertyEquals "name" (PropertyText name)) All
                 replyToSender sender $
                   T.pack $
                   printf "'%s' has been unscheduled" name
    Nothing -> replyToSender sender $
               T.pack $
               printf "'%s' was not scheduled to begin with" name
