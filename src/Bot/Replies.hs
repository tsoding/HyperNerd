{-# LANGUAGE QuasiQuotes #-}
module Bot.Replies where

import qualified Data.Text as T
import           Effect
import           Events
import           Text.InterpolatedString.QM

replyToUser :: T.Text -> T.Text -> Effect ()
replyToUser user text = say [qms|@{user} {text}|]

replyToSender :: Sender -> T.Text -> Effect ()
replyToSender sender = replyToUser (senderName sender)

banUser :: T.Text -> Effect ()
banUser user = say [qms|/ban {user}|]

timeoutUser :: Int -> T.Text -> Effect ()
timeoutUser t user = say [qms|/timeout {user} {t}|]

timeoutSender :: Int -> Sender -> Effect ()
timeoutSender t sender = timeoutUser t (senderName sender)

whisperToUser :: T.Text -> T.Text -> Effect ()
whisperToUser user message = say [qms|/w {user} {message}|]

whisperToSender :: Sender -> T.Text -> Effect ()
whisperToSender = whisperToUser . senderName
