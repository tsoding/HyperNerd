{-# LANGUAGE OverloadedStrings #-}
module Bot.Log where

import           Bot.Replies
import           Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Events
import           Text.Printf

data LogRecord = LogRecord { lrSender :: Sender
                           , lrMsg :: T.Text
                           , lrTimestamp :: UTCTime
                           }

instance IsEntity LogRecord where
    toProperties lr =
        M.fromList [ ("user", PropertyText $ senderName $ lrSender lr)
                   , ("channel", PropertyText $ senderChannel $ lrSender lr)
                   , ("msg", PropertyText $ lrMsg lr)
                   , ("timestamp", PropertyUTCTime $ lrTimestamp lr)
                   ]
    fromEntity entity =
        do user      <- extractProperty "user" entity
           channel   <- extractProperty "channel" entity
           msg       <- extractProperty "msg" entity
           timestamp <- extractProperty "timestamp" entity
           return LogRecord { lrSender = Sender { senderName = user
                                                , senderChannel = channel
                                                -- TODO(#148): senderSubscriber look out of place in IsEntity LogRecord
                                                , senderSubscriber = False
                                                }
                            , lrMsg = msg
                            , lrTimestamp = timestamp
                            }

recordUserMsg :: Sender -> T.Text -> Effect ()
recordUserMsg sender msg =
    do timestamp <- now
       _         <- createEntity "LogRecord"
                      $ toProperties LogRecord { lrSender = sender
                                               , lrMsg = msg
                                               , lrTimestamp = timestamp
                                               }
       return ()

randomLogRecordCommand :: Sender -> T.Text -> Effect ()
randomLogRecordCommand sender rawName =
    do name   <- return $ T.toLower $ T.strip rawName
       user   <- if T.null name
                 then return $ senderName sender
                 else return name
       logMsg $ T.pack $ printf "The requested user is %s" user
       entity <- getRandomEntity "LogRecord" (PropertyEquals "user" (PropertyText user))
       maybe (return ())
             (fromEntity >=> replyToUser user . lrMsg)
             entity
