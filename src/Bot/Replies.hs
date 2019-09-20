{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Replies where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Text as T
import Effect
import HyperNerd.Comonad
import Network.HTTP.Simple (getResponseBody, parseRequest)
import Reaction
import Regexp
import Text.InterpolatedString.QM
import Transport

sayMessage :: Reaction Message T.Text
sayMessage =
  Reaction $ \msg ->
    say (senderChannel $ messageSender msg) (messageContent msg)

mentionSender :: Sender -> T.Text
mentionSender Sender { senderChannel = sndrChannel
                     , senderName = sndrName
                     , senderId = sndrId
                     } =
  case sndrChannel of
    DiscordChannel _ -> [qms|<@{sndrId}>|]
    _ -> [qms|@{sndrName}|]

replyToSender :: Sender -> T.Text -> Effect ()
replyToSender sender text = do
  let channel = senderChannel sender
  case channel of
    DiscordChannel _ -> say channel [qms|{mentionSender sender} {text}|]
    _ -> say channel [qms|{mentionSender sender} {text}|]

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
  whisperToUser (senderChannel sender) $ senderName sender

replyOnNothing :: T.Text -> Reaction Message a -> Reaction Message (Maybe a)
replyOnNothing reply =
  maybeReaction $ cmapR (const reply) $ Reaction replyMessage

replyLeft :: Reaction Message a -> Reaction Message (Either String a)
replyLeft = eitherReaction $ cmapR T.pack $ Reaction replyMessage

onlyForRoles :: T.Text -> [Role] -> Reaction Message a -> Reaction Message a
onlyForRoles reply roles reaction =
  transR duplicate $
  ifR
    (any (`elem` roles) . senderRoles . messageSender)
    (cmapR extract reaction)
    (cmapR (const reply) $ Reaction replyMessage)

onlyForMods :: Reaction Message a -> Reaction Message a
onlyForMods = onlyForRoles "Only for mods" authorityRoles

nonEmptyRoles :: T.Text -> Reaction Message a -> Reaction Message a
nonEmptyRoles reply reaction =
  transR duplicate $
  ifR
    (null . senderRoles . messageSender)
    (cmapR (const reply) $ Reaction replyMessage)
    (cmapR extract reaction)

onlyForTwitch :: Reaction Message a -> Reaction Message a
onlyForTwitch reaction =
  Reaction $ \msg ->
    case senderChannel $ messageSender msg of
      TwitchChannel _ -> runReaction reaction msg
      _ -> replyMessage ("Works only in Twitch channels" <$ msg)

subcommand :: [(T.Text, Reaction Message T.Text)] -> Reaction Message T.Text
subcommand subcommandList =
  cmapR (regexParseArgs "([a-zA-Z0-9]*) *(.*)") $
  replyLeft $
  Reaction $ \msg ->
    case messageContent msg of
      [name, args] ->
        case M.lookup name subcommandTable of
          Just reaction -> runReaction reaction (args <$ msg)
          Nothing ->
            replyToSender (messageSender msg) [qms|No such subcommand {name}|]
      _ -> logMsg [qms|[ERROR] Could not pattern match {messageContent msg}|]
  where
    subcommandTable = M.fromList subcommandList

jsonHttpRequestReaction ::
     FromJSON a => Reaction Message a -> Reaction Message String
jsonHttpRequestReaction =
  cmapR parseRequest .
  eitherReaction (Reaction (logMsg . T.pack . show . messageContent)) .
  liftR httpRequest .
  cmapR (eitherDecode . getResponseBody) .
  eitherReaction (Reaction (logMsg . T.pack . messageContent))

byteStringHttpRequestReaction ::
     Reaction Message BS.ByteString -> Reaction Message String
byteStringHttpRequestReaction =
  cmapR parseRequest .
  eitherReaction (Reaction (logMsg . T.pack . show . messageContent)) .
  liftR httpRequest . cmapR getResponseBody
