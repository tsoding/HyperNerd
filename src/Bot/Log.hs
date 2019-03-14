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
import Numeric.Natural
import Property
import Reaction
import Text.InterpolatedString.QM
import Transport
import Safe
import Control.Monad.Catch

data LogRecordV1 = LogRecordV1
  { lr1User :: T.Text
  , lr1Channel :: T.Text
  , lr1Msg :: T.Text
  , lr1Timestamp :: UTCTime
  }

instance IsEntity LogRecordV1 where
  toProperties lr =
    M.fromList
      [ ("user", PropertyText $ lr1User lr)
      , ("channel", PropertyText $ lr1Channel lr)
      , ("msg", PropertyText $ lr1Msg lr)
      , (timestampPV, PropertyUTCTime $ lr1Timestamp lr)
      ]
  fromProperties properties =
    LogRecordV1 <$> extractProperty "user" properties <*>
    extractProperty "channel" properties <*>
    extractProperty "msg" properties <*>
    extractProperty timestampPV properties


data LogRecord = LogRecord
  { lrUser :: T.Text
  , lrChannel :: Channel
  , lrMsg :: T.Text
  , lrTimestamp :: UTCTime
  }

instance IsEntity LogRecord where
  toProperties lr =
    M.fromList
      [ ("version", PropertyInt 2)
      , ("user", PropertyText $ lrUser lr)
      , ("channel", PropertyText $ T.pack $ show $ lrChannel lr)
      , ("msg", PropertyText $ lrMsg lr)
      , (timestampPV, PropertyUTCTime $ lrTimestamp lr)
      ]
  fromProperties properties =
    case extractProperty "version" properties of
      Nothing -> migrateV1toV2 <$> fromProperties properties
      Just x
        | x == (2 :: Int) ->
          LogRecord <$> extractProperty "user" properties <*>
          -- TODO(#507): an error should be thrown on unparsable channel
          (fromMaybe (TwitchChannel "#tsoding") . readMay . T.unpack <$>
           extractProperty "channel" properties) <*>
          extractProperty "msg" properties <*>
          extractProperty timestampPV properties
      _ -> throwM $ UnexpectedPropertyType "LogRecord Version 2"

migrateV1toV2 :: LogRecordV1 -> LogRecord
migrateV1toV2 lr1 =
  LogRecord
    { lrUser = lr1User lr1
    , lrChannel = newChannel
    , lrMsg = lr1Msg lr1
    , lrTimestamp = lr1Timestamp lr1
    }
  where
    newChannel =
      case T.uncons $ lr1Channel lr1 of
        Just ('#', _) -> TwitchChannel $ lr1Channel lr1
        -- TODO: an error should be thrown on unparsable Discord channel id
        _ -> DiscordChannel $ fromMaybe 0 $ readMay $ T.unpack $ lr1Channel lr1


lrAsMsg :: LogRecord -> T.Text
lrAsMsg lr = [qms|<{lrUser lr}> {lrMsg lr}|]

timestampPV :: T.Text
timestampPV = "timestamp"

type Seconds = Natural

randomUserQuoteSelector :: T.Text -> Selector
randomUserQuoteSelector user =
  Take 1 $ Shuffle $ Filter (PropertyEquals "user" $ PropertyText user) All

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

-- TODO: !rq should query random message only for the channel it was called in
randomLogRecordCommand :: Reaction Message T.Text
randomLogRecordCommand =
  cmapR (T.toLower . T.strip) $
  transR duplicate $
  cmapR extractUser $
  liftR (selectEntities "LogRecord" . randomUserQuoteSelector) $
  cmapR listToMaybe $
  ignoreNothing $ cmapR (lrAsMsg . entityPayload) $ Reaction sayMessage
  where
    extractUser :: Message T.Text -> T.Text
    extractUser msg =
      fromMaybe (senderName $ messageSender msg) $
      find (not . T.null) $ Just $ messageContent msg
