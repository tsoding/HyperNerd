{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Bot.Ffz
  ( ffzUrlByName
  , ffzCommand
  , updateFfzEmotesCommand
  ) where

import Bot.Replies
import Data.Aeson.Types
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
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

newtype FfzSet = FfzSet
  { ffzSetEmotes :: [FfzEmote]
  }

instance FromJSON FfzSet where
  parseJSON (Object v) = FfzSet <$> (v .: "emoticons")
  parseJSON invalid = typeMismatch "FfzSet" invalid

data FfzGlobalRes = FfzGlobalRes
  { ffzGlobalResDefaultSets :: [Int]
  , ffzGlobalResSets :: M.Map T.Text FfzSet
  }

instance FromJSON FfzGlobalRes where
  parseJSON (Object v) = FfzGlobalRes <$> v .: "default_sets" <*> v .: "sets"
  parseJSON invalid = typeMismatch "FfzGlobalRes" invalid

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
           (Just x) -> Just . ("https:" <>) <$> parseJSON x) =<<
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

ffzCommand :: Reaction Message ()
ffzCommand =
  liftR (const $ selectEntities Proxy All) $
  cmapR (T.concat . intersperse " " . map (ffzName . entityPayload)) sayMessage

updateFfzGlobalEmotes :: Reaction Message a
updateFfzGlobalEmotes =
  cmapR (const "https://api.frankerfacez.com/v1/set/global") $
  jsonHttpRequestReaction $
  cmapR
    (\res ->
       concatMap ffzSetEmotes $
       mapMaybe ((`M.lookup` ffzGlobalResSets res) . T.pack . show) $
       ffzGlobalResDefaultSets res) $
  liftR (traverse $ createEntity Proxy) ignore

cleanFfzCache :: Reaction Message a
cleanFfzCache =
  Reaction $ \_ -> void $ deleteEntities (Proxy :: Proxy FfzEmote) All

updateFfzLocalEmotes :: Reaction Message a
updateFfzLocalEmotes =
  transR duplicate $
  cmapR (twitchChannelName . senderChannel . messageSender) $
  replyOnNothing "Works only in Twitch channels" $
  cmapR ffzUrl $
  jsonHttpRequestReaction $
  cmapR ffzResEmotes $ liftR (traverse $ createEntity Proxy) ignore

updateFfzEmotesCommand :: Reaction Message a
updateFfzEmotesCommand = onlyForTwitch f
  where
    f =
      cleanFfzCache <> updateFfzGlobalEmotes <> updateFfzLocalEmotes <>
      Reaction (\msg -> replyMessage ("FFZ cache has been updated" <$ msg))

ffzUrlByName :: T.Text -> Effect (Maybe String)
ffzUrlByName name = do
  emote <-
    listToMaybe <$>
    selectEntities
      Proxy
      (Filter (PropertyEquals "name" (PropertyText name)) All)
  pure (T.unpack <$> (ffzLargestImageURL =<< (entityPayload <$> emote)))
