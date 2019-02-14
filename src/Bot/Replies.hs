{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Replies where

import qualified Data.Text as T
import Effect
import Transport
import Reaction
import Text.InterpolatedString.QM

replyToUser :: T.Text -> T.Text -> Effect ()
replyToUser user text = say [qms|@{user} {text}|]

replyToSender :: Sender -> T.Text -> Effect ()
replyToSender sender = replyToUser (senderName sender)

replyMessage :: Message T.Text -> Effect ()
replyMessage Message {messageSender = sender, messageContent = text} =
  replyToSender sender text

banUser :: T.Text -> Effect ()
banUser user = twitchCommand "ban" [user]

timeoutUser :: Int -> T.Text -> Effect ()
timeoutUser t user = twitchCommand "timeout" [user, T.pack $ show t]

timeoutSender :: Int -> Sender -> Effect ()
timeoutSender t = timeoutUser t . senderName

timeoutMessage :: Int -> Message a -> Effect ()
timeoutMessage t = timeoutSender t . messageSender

whisperToUser :: T.Text -> T.Text -> Effect ()
whisperToUser user message = twitchCommand "w" [user, message]

whisperToSender :: Sender -> T.Text -> Effect ()
whisperToSender = whisperToUser . senderName

replyOnNothing :: T.Text -> Reaction Message a -> Reaction Message (Maybe a)
replyOnNothing reply =
  maybeReaction $ cmapR (const reply) $ Reaction replyMessage

replyLeft :: Reaction Message a -> Reaction Message (Either String a)
replyLeft = eitherReaction $ cmapR T.pack $ Reaction replyMessage
