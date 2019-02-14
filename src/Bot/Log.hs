{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Log where

import Bot.Replies
import Control.Comonad
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Transport
import Numeric.Natural
import Property
import Reaction
import Text.InterpolatedString.QM

data LogRecord = LogRecord
  { lrUser :: T.Text
  , lrChannel :: T.Text
  , lrMsg :: T.Text
  , lrTimestamp :: UTCTime
  }

lrAsMsg :: LogRecord -> T.Text
lrAsMsg lr = [qms|<{lrUser lr}> {lrMsg lr}|]

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

randomUserQuoteSelector :: T.Text -> Selector
randomUserQuoteSelector user =
  Take 1 $ Shuffle $ Filter (PropertyEquals "user" $ PropertyText user) All

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
  transR duplicate $
  cmapR extractUser $
  liftR (selectEntities "LogRecord" . randomUserQuoteSelector) $
  cmapR listToMaybe $
  ignoreNothing $ cmapR (lrAsMsg . entityPayload) $ liftR say ignore
  where
    extractUser :: Message T.Text -> T.Text
    extractUser msg =
      fromMaybe (senderName $ messageSender msg) $
      find (not . T.null) $ Just $ messageContent msg
