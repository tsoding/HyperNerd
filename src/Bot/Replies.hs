{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Replies where

import qualified Data.Text as T
import Effect
import Reaction
import Text.InterpolatedString.QM
import Transport

sayMessage :: Message T.Text -> Effect ()
sayMessage msg = say (senderChannel $ messageSender msg) (messageContent msg)

replyToSender :: Sender -> T.Text -> Effect ()
replyToSender sender text = do
  let channel = senderChannel sender
  case channel of
    DiscordChannel _ ->
      say channel [qms|<@{senderId sender}> {text}|]
    _ -> say channel [qms|@{senderName sender} {text}|]

replyMessage :: Message T.Text -> Effect ()
replyMessage Message {messageSender = sender, messageContent = text} =
  replyToSender sender text

banUser :: Channel -> T.Text -> Effect ()
banUser channel user = twitchCommand channel "ban" [user]

timeoutUser :: Channel -> Int -> T.Text -> Effect ()
timeoutUser channel t user =
  twitchCommand channel "timeout" [user, T.pack $ show t]

timeoutSender :: Int -> Sender -> Effect ()
timeoutSender t sender =
  timeoutUser (senderChannel sender) t (senderName sender)

timeoutMessage :: Int -> Message a -> Effect ()
timeoutMessage t = timeoutSender t . messageSender

whisperToUser :: Channel -> T.Text -> T.Text -> Effect ()
whisperToUser channel user message = twitchCommand channel "w" [user, message]

whisperToSender :: Sender -> T.Text -> Effect ()
whisperToSender sender =
  whisperToUser (senderChannel sender) $ (senderName sender)

replyOnNothing :: T.Text -> Reaction Message a -> Reaction Message (Maybe a)
replyOnNothing reply =
  maybeReaction $ cmapR (const reply) $ Reaction replyMessage

replyLeft :: Reaction Message a -> Reaction Message (Either String a)
replyLeft = eitherReaction $ cmapR T.pack $ Reaction replyMessage
