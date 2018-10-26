{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
module Bot.BttvFfz ( ffzCommand
                   , bttvCommand
                   , updateFfzEmotesCommand
                   , updateBttvEmotesCommand
                   ) where

import           Bot.Replies
import           Command
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Effect
import           Entity
import           Events
import           Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import           Property
import           Reaction
import           Safe
import           Text.Printf

newtype FfzEmote = FfzEmote { ffzName :: T.Text }

instance IsEntity FfzEmote where
    toProperties entity =
        M.fromList [ ("name", PropertyText $ ffzName entity) ]
    fromProperties properties =
        FfzEmote <$> extractProperty "name" properties

newtype BttvEmote = BttvEmote { bttvName :: T.Text }

instance IsEntity BttvEmote where
    toProperties entity =
        M.fromList [ ("name", PropertyText $ bttvName entity) ]
    fromProperties properties =
        BttvEmote <$> extractProperty "name" properties

newtype BttvRes = BttvRes { bttvResEmotes :: [BttvEmote] }

instance FromJSON BttvRes where
    parseJSON (Object v) = BttvRes <$> v .: "emotes"
    parseJSON invalid = typeMismatch "BttvRes" invalid

instance FromJSON BttvEmote where
    parseJSON (Object v) = BttvEmote <$> v .: "code"
    parseJSON invalid = typeMismatch "BttvEmote" invalid

newtype FfzRes = FfzRes { ffzResEmotes :: [FfzEmote] }

instance FromJSON FfzEmote where
    parseJSON (Object v) = FfzEmote <$> v .: "name"
    parseJSON invalid = typeMismatch "FfzEmote" invalid

instance FromJSON FfzRes where
    parseJSON (Object v) = FfzRes <$>
        do setId <- v .: "room" >>= (.: "set") :: Parser Int
           v .: "sets"
             >>= (.: (T.pack $ show setId))
             >>= (.: "emoticons")
    parseJSON invalid = typeMismatch "FfzRes" invalid

urlAsRequest :: CommandHandler (Either SomeException Request)
             -> CommandHandler String
urlAsRequest next = next . fmap parseRequest

jsonHttpRequest :: FromJSON a => CommandHandler (Either String a)
                -> CommandHandler Request
jsonHttpRequest next message@Message { messageContent = request } = do
  response <- eitherDecode . getResponseBody <$> httpRequest request
  next $ fmap (const response) message

dropEither :: Show e => CommandHandler a
           -> CommandHandler (Either e a)
dropEither _ Message { messageContent = Left e } =
    errorEff $ T.pack $ show e
dropEither next message@Message { messageContent = Right a } =
    next $ fmap (const a) message

ffzUrlString :: CommandHandler String
             -> CommandHandler ()
ffzUrlString next message@Message { messageSender = sender } =
    next $ fmap (const url) message
    where url = maybe "tsoding"
                      (printf "https://api.frankerfacez.com/v1/room/%s" . URI.encode)
                      (tailMay $ T.unpack $ senderChannel sender)

bttvUrlString :: CommandHandler String
              -> CommandHandler ()
bttvUrlString next message@Message { messageSender = sender } =
    next $ fmap (const url) message
    where url = maybe "tsoding"
                      (printf "https://api.betterttv.net/2/channels/%s" . URI.encode)
                      (tailMay $ T.unpack $ senderChannel sender)

replaceAllEntitiesCH :: IsEntity a => T.Text -> CommandHandler [a]
replaceAllEntitiesCH name Message { messageContent = entities } = do
  void $ deleteEntities name All
  traverse_ (createEntity name) entities

ffzCommand :: Reaction (Message ())
ffzCommand =
    liftEM (selectEntities "FfzEmote" All) $
    cmapF  (T.concat .
            intersperse " " .
            map (ffzName . entityPayload)) $
    Reaction replyMessage

bttvCommand :: Reaction (Message ())
bttvCommand =
    liftEM (selectEntities "BttvEmote" All) $
    cmapF  (T.concat .
            intersperse " " .
            map (bttvName . entityPayload)) $
    Reaction replyMessage

updateFfzEmotesCommand :: CommandHandler ()
updateFfzEmotesCommand =
    ffzUrlString    $
    urlAsRequest    $ dropEither $
    jsonHttpRequest $ dropEither $
    contramapCH ffzResEmotes     $
    replaceAllEntitiesCH "FfzEmote"

updateBttvEmotesCommand :: CommandHandler ()
updateBttvEmotesCommand =
    bttvUrlString   $
    urlAsRequest    $ dropEither $
    jsonHttpRequest $ dropEither $
    contramapCH bttvResEmotes    $
    replaceAllEntitiesCH "BttvEmote"
