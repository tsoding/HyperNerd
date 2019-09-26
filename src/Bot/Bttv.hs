{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Bot.Bttv
  ( bttvCommand
  , updateBttvEmotesCommand
  , bttvUrlByName
  ) where

import Bot.Replies
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Effect
import Entity
import HyperNerd.Comonad
import qualified Network.URI.Encode as URI
import Property
import Reaction
import Text.InterpolatedString.QM
import Transport

data BttvEmote = BttvEmote
  { bttvName :: T.Text
  , bttvLargestImageURL :: Maybe T.Text
  }

instance IsEntity BttvEmote where
  nameOfEntity _ = "BttvEmote"
  toProperties entity =
    M.fromList $
    catMaybes
      [ return ("name", PropertyText $ bttvName entity)
      , ("largestImageUrl", ) . PropertyText <$> bttvLargestImageURL entity
      ]
  fromProperties properties =
    BttvEmote <$> extractProperty "name" properties <*>
    pure (extractProperty "largestImageUrl" properties)

newtype BttvRes = BttvRes
  { bttvResEmotes :: [BttvEmote]
  }

instance FromJSON BttvRes where
  parseJSON (Object v) = BttvRes <$> v .: "emotes"
  parseJSON invalid = typeMismatch "BttvRes" invalid

instance FromJSON BttvEmote where
  parseJSON (Object v) = BttvEmote <$> code <*> url
    where
      code = v .: "code"
      url =
        ((\id' -> "https://cdn.betterttv.net/emote/" <> id' <> "/3x") <$>) <$>
        (v .: "id")
  parseJSON invalid = typeMismatch "BttvEmote" invalid

bttvUrl :: T.Text -> String
bttvUrl channel = [qms|https://api.betterttv.net/2/channels/{encodedChannel}|]
  where
    encodedChannel = URI.encode $ T.unpack channel

bttvCommand :: Reaction Message ()
bttvCommand =
  liftR (const $ selectEntities Proxy All) $
  cmapR (T.concat . intersperse " " . map (bttvName . entityPayload)) sayMessage

bttvUrlByName :: T.Text -> Effect (Maybe String)
bttvUrlByName name = do
  emote <-
    listToMaybe <$>
    selectEntities
      Proxy
      (Filter (PropertyEquals "name" (PropertyText name)) All)
  pure (T.unpack <$> (bttvLargestImageURL =<< (entityPayload <$> emote)))

cleanBttvCache :: Reaction Message a
cleanBttvCache =
  Reaction $ \_ -> void $ deleteEntities (Proxy :: Proxy BttvEmote) All

updateBttvGlobalEmotes :: Reaction Message a
updateBttvGlobalEmotes =
  cmapR (const "https://api.betterttv.net/2/emotes") $
  jsonHttpRequestReaction $
  liftR (traverse (createEntity Proxy) . bttvResEmotes) ignore

updateBttvLocalEmotes :: Reaction Message a
updateBttvLocalEmotes =
  transR duplicate $
  cmapR (twitchChannelName . senderChannel . messageSender) $
  replyOnNothing "Only works in Twitch channels" $
  cmapR bttvUrl $
  jsonHttpRequestReaction $
  cmapR bttvResEmotes $ liftR (traverse $ createEntity Proxy) ignore

updateBttvEmotesCommand :: Reaction Message a
updateBttvEmotesCommand = onlyForTwitch f
  where
    f =
      cleanBttvCache <> updateBttvGlobalEmotes <> updateBttvLocalEmotes <>
      Reaction (\msg -> replyMessage ("BTTV cache has been updated" <$ msg))
