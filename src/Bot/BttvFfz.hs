{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Bot.BttvFfz
  ( ffzCommand
  , bttvCommand
  , updateFfzEmotesCommand
  , updateBttvEmotesCommand
  , ffzUrlByName
  ) where

import Bot.Replies
import Control.Comonad
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.List
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T
import Effect
import Entity
import qualified Network.URI.Encode as URI
import Property
import Reaction
import Text.InterpolatedString.QM
import Transport
import Data.Maybe
import Data.Ord
import qualified Data.HashMap.Strict as HM

data FfzEmote = FfzEmote
  { ffzName :: T.Text
  , ffzLargestImageURL :: Maybe T.Text
  }

instance IsEntity FfzEmote where
  nameOfEntity _ = "FfzEmote"
  toProperties entity =
    M.fromList $
    catMaybes
      [ return ("name", PropertyText $ ffzName entity)
      , ("largestImageUrl", ) . PropertyText <$> ffzLargestImageURL entity
      ]
  fromProperties properties =
    FfzEmote <$> extractProperty "name" properties <*>
    pure (extractProperty "largestImageUrl" properties)

newtype BttvEmote = BttvEmote
  { bttvName :: T.Text
  }

instance IsEntity BttvEmote where
  nameOfEntity _ = "BttvEmote"
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
  parseJSON (Object v) = FfzEmote <$> v .: "name" <*> (v .: "urls" >>= maxUrl)
    where
      maxUrl :: Value -> Parser (Maybe T.Text)
      maxUrl (Object v') =
        (\case
           Nothing -> pure Nothing
           (Just x) -> Just <$> parseJSON x) =<<
        pure ((`HM.lookup` v') =<< idx)
        where
          idx = listToMaybe $ sortOn Down $ HM.keys v'
      maxUrl invalid = typeMismatch "FfzEmote" invalid
  parseJSON invalid = typeMismatch "FfzEmote" invalid

instance FromJSON FfzRes where
  parseJSON (Object v) =
    FfzRes <$> do
      setId <- v .: "room" >>= (.: "set") :: Parser Int
      v .: "sets" >>= (.: (T.pack $ show setId)) >>= (.: "emoticons")
  parseJSON invalid = typeMismatch "FfzRes" invalid

ffzUrl :: T.Text -> String
ffzUrl channel = [qms|https://api.frankerfacez.com/v1/room/{encodedChannel}|]
  where
    encodedChannel = URI.encode $ T.unpack channel

bttvUrl :: T.Text -> String
bttvUrl channel = [qms|https://api.betterttv.net/2/channels/{encodedChannel}|]
  where
    encodedChannel = URI.encode $ T.unpack channel

ffzCommand :: Reaction Message ()
ffzCommand =
  liftR (const $ selectEntities Proxy All) $
  cmapR (T.concat . intersperse " " . map (ffzName . entityPayload)) sayMessage

bttvCommand :: Reaction Message ()
bttvCommand =
  liftR (const $ selectEntities Proxy All) $
  cmapR (T.concat . intersperse " " . map (bttvName . entityPayload)) sayMessage

updateFfzEmotesCommand :: Reaction Message ()
updateFfzEmotesCommand =
  transR duplicate $
  cmapR (twitchChannelName . senderChannel . messageSender) $
  replyOnNothing "Only works in Twitch channels" $
  cmapR ffzUrl $
  jsonHttpRequestReaction $
  cmapR ffzResEmotes $
  liftR
    (\emotes -> do
       void $ deleteEntities (Proxy :: Proxy FfzEmote) All
       traverse (createEntity Proxy) emotes) $
  cmapR (T.concat . intersperse " " . map (ffzName . entityPayload)) sayMessage

updateBttvEmotesCommand :: Reaction Message ()
updateBttvEmotesCommand =
  transR duplicate $
  cmapR (twitchChannelName . senderChannel . messageSender) $
  replyOnNothing "Only works in Twitch channels" $
  cmapR bttvUrl $
  jsonHttpRequestReaction $
  cmapR bttvResEmotes $
  liftR
    (\emotes -> do
       void $ deleteEntities (Proxy :: Proxy BttvEmote) All
       traverse (createEntity Proxy) emotes) $
  cmapR (T.concat . intersperse " " . map (bttvName . entityPayload)) sayMessage

ffzUrlByName :: T.Text -> Effect (Maybe String)
ffzUrlByName name = do
  emote <-
    listToMaybe <$>
    selectEntities
      Proxy
      (Filter (PropertyEquals "name" (PropertyText name)) All)
  pure (T.unpack <$> (ffzLargestImageURL =<< (entityPayload <$> emote)))
