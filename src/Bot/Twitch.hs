{-# LANGUAGE OverloadedStrings #-}

module Bot.Twitch where

import Bot.Replies
import Control.Comonad
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Extra
import Effect
import Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import Reaction
import Text.Printf
import Transport

newtype TwitchResponse a = TwitchResponse
  { trData :: [a]
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (TwitchResponse a) where
  parseJSON (Object obj) = TwitchResponse <$> obj .: "data"
  parseJSON invalid = typeMismatch "TwitchResponse" invalid

data TwitchStream = TwitchStream
  { tsStartedAt :: UTCTime
  , tsTitle :: T.Text
  } deriving (Eq, Show)

instance FromJSON TwitchStream where
  parseJSON (Object obj) =
    TwitchStream <$> obj .: "started_at" <*> obj .: "title"
  parseJSON invalid = typeMismatch "TwitchStream" invalid

twitchStreamByLogin :: T.Text -> Effect (Maybe TwitchStream)
twitchStreamByLogin login = do
  request <-
    parseRequest $
    printf "https://api.twitch.tv/helix/streams?user_login=%s" $
    URI.encode $ T.unpack login
  response <- twitchApiRequest request
  either
    (errorEff . T.pack)
    (return . listToMaybe . trData)
    (eitherDecode $ getResponseBody response)

streamUptime :: TwitchStream -> Effect NominalDiffTime
streamUptime twitchStream = do
  currentTime <- now
  let streamStartTime = tsStartedAt twitchStream
  return $ diffUTCTime currentTime streamStartTime

-- TODO(#475): The target of uptime command should be customizable
uptimeCommand :: Reaction Message ()
uptimeCommand =
  transR duplicate $
  cmapR (twitchChannelName . senderChannel . messageSender) $
  replyOnNothing "Only works in Twitch channels" $
  liftR twitchStreamByLogin $
  replyOnNothing "Not even streaming LUL" $
  liftR streamUptime $
  cmapR humanReadableDiffTime $
  cmapR (T.append "Streaming for ") $ Reaction replyMessage
