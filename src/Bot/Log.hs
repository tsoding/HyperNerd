{-# LANGUAGE OverloadedStrings #-}
module Bot.Log where

import           Bot.Replies
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

randomLogRecordCommand :: Sender -> T.Text -> Effect ()
randomLogRecordCommand sender rawName =
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
