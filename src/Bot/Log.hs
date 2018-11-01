{-# LANGUAGE OverloadedStrings #-}
module Bot.Log where

import           Bot.Replies
import           Command
import           Control.Monad
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Events
import           Property

data LogRecord = LogRecord { lrUser :: T.Text
                           , lrChannel :: T.Text
                           , lrMsg :: T.Text
                           , lrTimestamp :: UTCTime
                           }

instance IsEntity LogRecord where
    toProperties lr =
        M.fromList [ ("user", PropertyText $ lrUser lr)
                   , ("channel", PropertyText $ lrChannel lr)
                   , ("msg", PropertyText $ lrMsg lr)
                   , ("timestamp", PropertyUTCTime $ lrTimestamp lr)
                   ]
    fromProperties properties =
        LogRecord <$> extractProperty "user" properties
                  <*> extractProperty "channel" properties
                  <*> extractProperty "msg" properties
                  <*> extractProperty "timestamp" properties

recordUserMsg :: Sender -> T.Text -> Effect ()
recordUserMsg sender msg =
    do timestamp <- now
       _         <- createEntity "LogRecord" LogRecord { lrUser = senderName sender
                                                       , lrChannel = senderChannel sender
                                                       , lrMsg = msg
                                                       , lrTimestamp = timestamp
                                                       }
       return ()

intToNormalDiffTime :: Int -> NominalDiffTime
intToNormalDiffTime = fromInteger . toInteger

assertIsNegative :: Int -> String -> Int
assertIsNegative n msg
  | n < 0 = n
  | otherwise = error $ show n ++ " " ++ msg
  
getLogs :: Int -> Effect [LogRecord]
getLogs offsetMillis = do
  currentTime <- now
  let diff = intToNormalDiffTime (assertIsNegative offsetMillis "offset should be negative" `div` 1000)
  let startDate = addUTCTime diff currentTime 
  allLogs <- selectEntities "LogRecord" $ SortBy "timstamp" Desc All
  let result = filter (\l -> lrTimestamp l > startDate) $ map entityPayload allLogs
  return result
  
randomLogRecordCommand :: CommandHandler T.Text
randomLogRecordCommand Message { messageSender = sender
                               , messageContent = rawName
                               } =
    do let name = T.toLower $ T.strip rawName
       user   <- if T.null name
                 then return $ senderName sender
                 else return name
       entity <- listToMaybe
                   <$> selectEntities "LogRecord" (Take 1
                                                     $ Shuffle
                                                     $ Filter (PropertyEquals "user" $ PropertyText user)
                                                       All)
       maybe (return ())
             (fromEntityProperties >=> replyToUser user . lrMsg . entityPayload)
             entity
