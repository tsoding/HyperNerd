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
import           Text.Printf

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
        do user      <- extractProperty "user" properties
           channel   <- extractProperty "channel" properties
           msg       <- extractProperty "msg" properties
           timestamp <- extractProperty "timestamp" properties
           return LogRecord { lrUser = user
                            , lrChannel = channel
                            , lrMsg = msg
                            , lrTimestamp = timestamp
                            }

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
    do name   <- return $ T.toLower $ T.strip rawName
       user   <- if T.null name
                 then return $ senderName sender
                 else return name
       logMsg $ T.pack $ printf "The requested user is %s" user
       entity <- listToMaybe
                   <$> selectEntities "LogRecord" (Take 1
                                                     $ Shuffle
                                                     $ Filter (PropertyEquals "user" $ PropertyText user)
                                                       All)
       maybe (return ())
             (fromEntityProperties >=> replyToUser user . lrMsg . entityPayload)
             entity
