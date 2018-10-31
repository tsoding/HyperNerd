{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot.BttvFfz
  ( ffzCommand
  , bttvCommand
  , updateFfzEmotesCommand
  , updateBttvEmotesCommand
  ) where

import Bot.Replies
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Effect
import Entity
import Events
import Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import Property
import Reaction
import Safe
import Text.Printf

newtype FfzEmote = FfzEmote
  { ffzName :: T.Text
  }

instance IsEntity FfzEmote where
  toProperties entity = M.fromList [("name", PropertyText $ ffzName entity)]
  fromProperties properties = FfzEmote <$> extractProperty "name" properties

newtype BttvEmote = BttvEmote
  { bttvName :: T.Text
  }

instance IsEntity BttvEmote where
  toProperties entity = M.fromList [("name", PropertyText $ bttvName entity)]
  fromProperties properties = BttvEmote <$> extractProperty "name" properties

newtype BttvRes = BttvRes
  { bttvResEmotes :: [BttvEmote]
  }

instance FromJSON BttvRes where
  parseJSON (Object v) = BttvRes <$> v .: "emotes"
  parseJSON invalid = typeMismatch "BttvRes" invalid

instance FromJSON BttvEmote where
  parseJSON (Object v) = BttvEmote <$> v .: "code"
  parseJSON invalid = typeMismatch "BttvEmote" invalid

newtype FfzRes = FfzRes
  { ffzResEmotes :: [FfzEmote]
  }

instance FromJSON FfzEmote where
  parseJSON (Object v) = FfzEmote <$> v .: "name"
  parseJSON invalid = typeMismatch "FfzEmote" invalid

instance FromJSON FfzRes where
  parseJSON (Object v) =
    FfzRes <$> do
      setId <- v .: "room" >>= (.: "set") :: Parser Int
      v .: "sets" >>= (.: (T.pack $ show setId)) >>= (.: "emoticons")
  parseJSON invalid = typeMismatch "FfzRes" invalid

ffzUrl :: Message a -> Message String
ffzUrl message@Message {messageSender = sender} = fmap (const url) message
  where
    url =
      maybe
        "tsoding"
        (printf "https://api.frankerfacez.com/v1/room/%s" . URI.encode)
        (tailMay $ T.unpack $ senderChannel sender)

bttvUrl :: Message a -> Message String
bttvUrl message@Message {messageSender = sender} = fmap (const url) message
  where
    url =
      maybe
        "tsoding"
        (printf "https://api.betterttv.net/2/channels/%s" . URI.encode)
        (tailMay $ T.unpack $ senderChannel sender)

ffzCommand :: Reaction (Message ())
ffzCommand =
  liftEM (selectEntities "FfzEmote" All) $
  cmapF (T.concat . intersperse " " . map (ffzName . entityPayload)) $
  Reaction replyMessage

bttvCommand :: Reaction (Message ())
bttvCommand =
  liftEM (selectEntities "BttvEmote" All) $
  cmapF (T.concat . intersperse " " . map (bttvName . entityPayload)) $
  Reaction replyMessage

jsonHttpRequest ::
     FromJSON a => Reaction (Message a) -> Reaction (Message String)
jsonHttpRequest =
  cmapF parseRequest .
  silenceOnLeft .
  liftKM httpRequest .
  cmapF (eitherDecode . getResponseBody) .
    -- TODO(#349): we probably don't wanna silence JSON parsing errors
  silenceOnLeft

updateFfzEmotesCommand :: Reaction (Message ())
updateFfzEmotesCommand =
  cmap ffzUrl $
  jsonHttpRequest $
  cmapF ffzResEmotes $
  liftKM
    (\emotes -> do
       void $ deleteEntities "FfzEmote" All
       traverse (createEntity "FfzEmote") emotes) $
  cmapF (T.concat . intersperse " " . map (ffzName . entityPayload)) $
  Reaction replyMessage

updateBttvEmotesCommand :: Reaction (Message ())
updateBttvEmotesCommand =
  cmap bttvUrl $
  jsonHttpRequest $
  cmapF bttvResEmotes $
  liftKM
    (\emotes -> do
       void $ deleteEntities "BttvEmote" All
       traverse (createEntity "BttvEmote") emotes) $
  cmapF (T.concat . intersperse " " . map (bttvName . entityPayload)) $
  Reaction replyMessage
