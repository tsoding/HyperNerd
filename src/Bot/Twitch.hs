{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Twitch where

import Bot.Replies
import Command
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Effect
import Events
import Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import Safe
import Text.InterpolatedString.QM
import Text.Printf

data TwitchStream = TwitchStream
  { tsStartedAt :: UTCTime
  , tsTitle :: T.Text
  }

twitchStreamsParser :: Object -> Parser [TwitchStream]
twitchStreamsParser obj =
  obj .: "data" >>=
  mapM
    (\s -> do
       title <- s .: "title"
       startedAt <- s .: "started_at"
       return TwitchStream {tsStartedAt = startedAt, tsTitle = title})

twitchStreamByLogin :: T.Text -> Effect (Maybe TwitchStream)
twitchStreamByLogin login = do
  request <-
    parseRequest $
    printf "https://api.twitch.tv/helix/streams?user_login=%s" $
    URI.encode $ T.unpack login
  response <- twitchApiRequest request
  let payload = eitherDecode $ getResponseBody response
  either
    (errorEff . T.pack)
    (return . listToMaybe)
    (payload >>= parseEither twitchStreamsParser)

humanReadableDiffTime :: NominalDiffTime -> T.Text
humanReadableDiffTime t =
  T.pack $
  unwords $
  map (\(name, amount) -> [qms|{amount} {name}|]) $
  filter ((> 0) . snd) components
  where
    s :: Int
    s = round t
    components :: [(T.Text, Int)]
    components =
      [ ("days" :: T.Text, s `div` secondsInDay)
      , ("hours", (s `mod` secondsInDay) `div` secondsInHour)
      , ( "minutes"
        , ((s `mod` secondsInDay) `mod` secondsInHour) `div` secondsInMinute)
      , ( "seconds"
        , ((s `mod` secondsInDay) `mod` secondsInHour) `mod` secondsInMinute)
      ]
    secondsInDay = 24 * secondsInHour
    secondsInHour = 60 * secondsInMinute
    secondsInMinute = 60

uptimeCommand :: CommandHandler ()
uptimeCommand Message {messageSender = sender} = do
  let channel =
        T.pack $ fromMaybe "tsoding" $ tailMay $ T.unpack $ senderChannel sender
  response <- twitchStreamByLogin channel
  maybe
    (replyToSender sender "Not even streaming LUL")
    (\twitchStream -> do
       currentTime <- now
       let streamStartTime = tsStartedAt twitchStream
       let humanReadableDiff =
             humanReadableDiffTime $ diffUTCTime currentTime streamStartTime
       replyToSender sender [qms|Streaming for {humanReadableDiff}|])
    response
