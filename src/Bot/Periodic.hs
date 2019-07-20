{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Periodic
  ( startPeriodicCommands
  , addPeriodicCommand
  , removePeriodicCommand
  , enablePeriodicTimerCommand
  , disablePeriodicTimerCommand
  , statusPeriodicTimerCommand
  , addPeriodicTimerCommand
  , removePeriodicTimerCommand
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
import Data.Foldable

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
data PeriodicCommand = PeriodicCommand
  { periodicCommand :: Command T.Text
  , periodicTimer :: Int
  }

-- TODO: There is no way to modify timers period
data PeriodicTimer = PeriodicTimer
  { periodicTimerEnabled :: Bool
  , periodicTimerPeriod :: Int
  }

instance IsEntity PeriodicTimer where
  nameOfEntity _ = "PeriodicTimer"
  toProperties pt =
    M.fromList
      [ ("enabled", PropertyInt $ boolAsInt $ periodicTimerEnabled pt)
      , ("period", PropertyInt $ periodicTimerPeriod pt)
      ]
  fromProperties properties =
    PeriodicTimer <$> (intAsBool <$> extractProperty "enabled" properties) <*>
    extractProperty "period" properties

instance IsEntity PeriodicCommand where
  nameOfEntity _ = "PeriodicCommand"
  toProperties PeriodicCommand { periodicCommand = Command name args
                               , periodicTimer = timer
                               } =
    M.fromList
      [ ("name", PropertyText name)
      , ("args", PropertyText args)
      , ("timer", PropertyInt timer)
      ]
  fromProperties properties =
    PeriodicCommand <$>
    (Command <$> extractProperty "name" properties <*>
     extractProperty "args" properties) <*>
    extractProperty "timer" properties

getPeriodicCommandByName :: T.Text -> Effect (Maybe (Entity PeriodicCommand))
getPeriodicCommandByName name =
  fmap listToMaybe $
  selectEntities Proxy $
  Take 1 $ Filter (PropertyEquals "name" (PropertyText name)) All

startPeriodicTimer ::
     (Message (Command T.Text) -> Effect ()) -> Channel -> Int -> Effect ()
startPeriodicTimer dispatchCommand channel eid =
  periodicEffect' (Just channel) $ do
    pt' <- getEntityById Proxy eid
    maybe
      (return Nothing)
      (\Entity {entityPayload = pt} -> do
         pc' <-
           fmap listToMaybe $
           selectEntities Proxy $
           Take 1 $
           Shuffle $ Filter (PropertyEquals "timer" $ PropertyInt eid) All
         when (periodicTimerEnabled pt) $
           maybe
             (return ())
             (dispatchCommand .
              Message (mrbotka {senderChannel = channel}) False .
              periodicCommand . entityPayload)
             pc'
         return $ Just $ fromIntegral $ periodicTimerPeriod pt)
      pt'

startPeriodicCommands ::
     Channel -> (Message (Command T.Text) -> Effect ()) -> Effect ()
startPeriodicCommands channel dispatchCommand = do
  eids <- (entityId <$>) <$> selectEntities (Proxy :: Proxy PeriodicTimer) All
  for_ eids (startPeriodicTimer dispatchCommand channel)

-- TODO: !addperiodic does not check if the provided timer exist
addPeriodicCommand :: Reaction Message (Int, Command T.Text)
addPeriodicCommand =
  Reaction $ \Message { messageSender = sender
                      , messageContent = (timerId, command@Command {commandName = name})
                      } -> do
    maybePc <- getPeriodicCommandByName name
    case maybePc of
      Just _ ->
        replyToSender sender [qms|'{name}' is aleady called periodically|]
      Nothing -> do
        void $ createEntity Proxy $ PeriodicCommand command timerId
        replyToSender
          sender
          [qms|'{name}' has been scheduled to call periodically|]

-- TODO: !delperiodic does not allow to specify the timer
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
  liftR (const $ selectEntities Proxy All) $
  cmapR ((enablePeriodicTimer <$>) <$>) $
  liftR (mapM_ updateEntityById) $
  cmapR (const "Periodic timers have been enabled") $ Reaction replyMessage

disablePeriodicTimerCommand :: Reaction Message a
disablePeriodicTimerCommand =
  liftR (const $ selectEntities Proxy All) $
  cmapR ((disablePeriodicTimer <$>) <$>) $
  liftR (mapM_ updateEntityById) $
  cmapR (const "Periodic timer has been disabled") $ Reaction replyMessage

statusPeriodicTimerCommand :: Reaction Message a
statusPeriodicTimerCommand =
  liftR (const $ selectEntities (Proxy :: Proxy PeriodicTimer) All) $
  cmapR (T.pack . show . map (periodicTimerEnabled . entityPayload)) $
  Reaction replyMessage

addPeriodicTimerCommand ::
     (Message (Command T.Text) -> Effect ()) -> Reaction Message Int
addPeriodicTimerCommand dispatchCommand =
  cmapR (PeriodicTimer False) $
  liftR (createEntity Proxy) $
  dupLiftR
    (\msg -> do
       let eid = entityId $ messageContent msg
       startPeriodicTimer
         dispatchCommand
         (senderChannel $ messageSender msg)
         (entityId $ messageContent msg)
       return eid) $
  cmapR (("Created Periodic Timer with id " <>) . T.pack . show) $
  Reaction replyMessage

removePeriodicTimerCommand :: Reaction Message Int
removePeriodicTimerCommand =
  deleteEntityByIdCommand (Proxy :: Proxy PeriodicTimer)

deleteEntityByIdCommand :: IsEntity e => Proxy e -> Reaction Message Int
deleteEntityByIdCommand proxy =
  liftR (getEntityById proxy) $
  replyOnNothing [qms|No {nameOfEntity proxy} with such id|] $
  liftR (deleteEntityById proxy . entityId) $
  cmapR (const "{nameOfEntity proxy} has been removed") $ Reaction replyMessage
