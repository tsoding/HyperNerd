{-# LANGUAGE OverloadedStrings #-}

module Bot.Log where

import Bot.Replies
import Command
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Events
import Property
import Numeric.Natural

data LogRecord = LogRecord
  { lrUser :: T.Text
  , lrChannel :: T.Text
  , lrMsg :: T.Text
  , lrTimestamp :: UTCTime
  }

timestampPV :: T.Text
timestampPV = "timestamp"

type Seconds = Natural

instance IsEntity LogRecord where
  toProperties lr =
    M.fromList
      [ ("user", PropertyText $ lrUser lr)
      , ("channel", PropertyText $ lrChannel lr)
      , ("msg", PropertyText $ lrMsg lr)
      , (timestampPV, PropertyUTCTime $ lrTimestamp lr)
      ]
  fromProperties properties =
    LogRecord <$> extractProperty "user" properties <*>
    extractProperty "channel" properties <*>
    extractProperty "msg" properties <*>
    extractProperty timestampPV properties

recordUserMsg :: Sender -> T.Text -> Effect ()
recordUserMsg sender msg = do
  timestamp <- now
  _ <-
    createEntity
      "LogRecord"
      LogRecord
        { lrUser = senderName sender
        , lrChannel = senderChannel sender
        , lrMsg = msg
        , lrTimestamp = timestamp
        }
  return ()

getRecentLogs :: Seconds -> Effect [LogRecord]
getRecentLogs offset = do
  currentTime <- now
  let diff = intToNominalDiffTime $ -1 * offset
  let startDate = addUTCTime diff currentTime
  -- TODO there should be comparing selector when implemented
  allLogs <- selectEntities "LogRecord" $ SortBy timestampPV Desc All
  let result = filter (\l -> lrTimestamp l > startDate) $ map entityPayload allLogs
  return result
  where
    intToNominalDiffTime = fromInteger . toInteger
  
randomLogRecordCommand :: CommandHandler T.Text
randomLogRecordCommand Message { messageSender = sender
                               , messageContent = rawName
                               } = do
  let name = T.toLower $ T.strip rawName
  user <-
    if T.null name
      then return $ senderName sender
      else return name
  entity <-
    listToMaybe <$>
    selectEntities
      "LogRecord"
      (Take 1 $ Shuffle $ Filter (PropertyEquals "user" $ PropertyText user) All)
  maybe
    (return ())
    (fromEntityProperties >=> replyToUser user . lrMsg . entityPayload)
    entity
