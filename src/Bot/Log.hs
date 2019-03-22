{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Log
  ( LogRecord(..)
  , Seconds
  , randomLogRecordCommand
  , recordUserMsg
  , getRecentLogs
  , randomLogRecord
  ) where

import Bot.Replies
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Numeric.Natural
import Property
import Reaction
import Safe
import Text.InterpolatedString.QM
import Transport

data LogRecord = LogRecord
  { lrUser :: T.Text
  , lrChannel :: Channel
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
      , ("channel", PropertyText $ T.pack $ show $ lrChannel lr)
      , ("msg", PropertyText $ lrMsg lr)
      , (timestampPV, PropertyUTCTime $ lrTimestamp lr)
      ]
  fromProperties properties =
    LogRecord <$> extractProperty "user" properties <*>
    (fromMaybe (TwitchChannel "#tsoding") . readMay . T.unpack <$>
     extractProperty "channel" properties) <*>
    extractProperty "msg" properties <*>
    extractProperty timestampPV properties

randomUserQuoteSelector :: Message T.Text -> Selector
randomUserQuoteSelector msg =
  Take 1 $
  Shuffle $
  Filter
    (ConditionAnd
       [ PropertyEquals "user" $ PropertyText $ messageContent msg
       , PropertyEquals "channel" $ PropertyText $ T.pack $ show $ senderChannel $ messageSender msg
       ])
    All

recordUserMsg :: Message T.Text -> Effect ()
recordUserMsg Message {messageSender = sender, messageContent = msg} = do
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
  let diff = secondsAsBackwardsDiff offset
  let startDate = addUTCTime diff currentTime
  -- TODO(#358): use "PropertyGreater" when it's ready
  -- limiting fetched logs by 100 untill then
  allLogs <- selectEntities "LogRecord" $ Take 100 $ SortBy timestampPV Desc All
  let result =
        filter (\l -> lrTimestamp l > startDate) $ map entityPayload allLogs
  return result

secondsAsBackwardsDiff :: Seconds -> NominalDiffTime
secondsAsBackwardsDiff = negate . fromInteger . toInteger

randomLogRecord :: Reaction Message a
randomLogRecord =
  liftR (const $ selectEntities "LogRecord" $ Take 1 $ Shuffle All) $
  cmapR listToMaybe $
  ignoreNothing $ cmapR (lrMsg . entityPayload) $ Reaction replyMessage

randomLogRecordCommand :: Reaction Message T.Text
randomLogRecordCommand =
  cmapR (T.toLower . T.strip) $
  transCmapR extractUser $
  transLiftR (selectEntities "LogRecord" . randomUserQuoteSelector) $
  cmapR listToMaybe $
  ignoreNothing $ cmapR (lrAsMsg . entityPayload) $ Reaction sayMessage
  where
    extractUser :: Message T.Text -> T.Text
    extractUser msg
      | T.null $ messageContent msg = senderName $ messageSender msg
      | otherwise = messageContent msg
    lrAsMsg :: LogRecord -> T.Text
    lrAsMsg lr = [qms|<{lrUser lr}> {lrMsg lr}|]
